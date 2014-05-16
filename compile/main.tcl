
# Used for embedding in an executable
package require starkit
starkit::startup

#-----------------------------------------------------------------------------------------
proc main {argv} {
  main.readArguments $argv section_name interface_path output_file
  set interface [main.processInterfaceFile $interface_path]
  main.writeOutput $interface $section_name $output_file
}

#-----------------------------------------------------------------------------------------
proc main.readSanitizedLineFromFile {fp out_var} {
  if {[eof $fp]} { return 0 }
  upvar $out_var line
  if {![gets $fp line]} { return 1 }
  set STRIP_COMMENTS {^([^#]*)#.*$}
  regexp $STRIP_COMMENTS $line - line
  set line [string trim $line]
  return 1
}

#-----------------------------------------------------------------------------------------
proc main.appendFunctionDeclarationToRetval {pfd_retval} {
  upvar retval retval
  foreach {key value} $pfd_retval { set retval($key) [concat [array get retval $key] $value] }
}

#-----------------------------------------------------------------------------------------
proc main.processInterfaceFile {interface_path} {
  set fp [open $interface_path r]
  set function_lines [list]
  array set retval {}
  while {[main.readSanitizedLineFromFile $fp line]} {
    if {$line eq "end"} {
      main.appendFunctionDeclarationToRetval [processFunctionDeclaration $function_lines]
      set function_lines [list]
    } elseif {[string length $line]} {
      lappend function_lines $line
    }
  }
  close $fp
  if {[llength $function_lines]} {
    main.appendFunctionDeclarationToRetval [processFunctionDeclaration $function_lines]
  }
  return [array get retval]
}

#-----------------------------------------------------------------------------------------
proc main.readArguments {argv section_name_var interface_path_var output_dir_var} {
  upvar $section_name_var section_name
  upvar $interface_path_var interface_path
  upvar $output_dir_var output_dir

  if {[llength $argv] < 3} {
    puts "usage:\n compile <section name> <interface file> <output directory>"
    exit
  }

  lassign $argv section_name interface_path output_dir
  if {![regexp {^[a-zA-Z0-9_]+$} $section_name]} {
    puts "Section name must be a single word. It gets integrated into various commands"
    exit
  }
}

#-----------------------------------------------------------------------------------------
proc main.writeOutput {interface_array section_name output_file} {
  array set interface $interface_array

  set fp [open $output_file "w"]
  proc output {text} {
    upvar fp fp
    puts $fp $text
  }

  output "#include <memory.h>"
  output "#include <assert.h>"

  output "\n"
  foreach {name value} $interface(SYMBOLS) {
    output "#ifndef $name"
    output "#define $name ($value)"
    output "#endif"
  }

  # Output the enum that declares the unique identifier for each element
  output "enum InterfaceElements {"
  foreach {name symbol index} $interface(INDICES) {
    output "  $symbol = $index, // $name"
  }
  output "\n  INTERFACE_ELEMENT_COUNT"
  output "};\n"

  # Output all the encoder functions
  foreach prototype $interface(ENCODE) {
    output "$prototype\n"
  }

  output "template <typename Class>"
  output "struct ${section_name}FunctionTable {"
  foreach {typedef member} $interface(CALLBACK) {
    output "  $typedef\n  $member\n"
  }
  output "};\n"

  output "template <typename Class>"
  output "bool decode(const char *buffer, size_t bytes, Class *object, ${section_name}FunctionTable<Class> *functions) {"
  output "  const char *__read_ptr = buffer + sizeof(unsigned short);"
  output "  const char *__read_end = buffer + bytes;"
  output "  if (!buffer || __read_ptr > __read_end) return false;"
  output "  unsigned int __msg = *(unsigned short*)buffer;"
  output "  switch (__msg) {"
  foreach decoder $interface(DECODE) {
    set decoder [string map {\n "\n  "} $decoder]
    output "  $decoder"
  }
  output "  }"
  output "  return false; // todo: log error"
  output "}"

  close $fp
}


#-----------------------------------------------------------------------------------------
proc processFunctionDeclaration {declaration_lines} {
  array set output {SYMBOLS {} CALLBACK {} ENCODE {} DECODE {}}
  if {![llength $declaration_lines]} return
  set output(INDICES) [processFunctionDeclarationFirstLine [lindex $declaration_lines 0] function_name function_symbol function_index]
  set arguments [readArgumentsFromFunctionDeclaration [lrange $declaration_lines 1 end]]
  lassign [getFunctionDeclaration $arguments] declaration invocation symbols
  set output(SYMBOLS) $symbols

  set joined_declaration [join $declaration ",\n    "]
  set encode_prototype "template <typename CallbackFunction>\nbool ${function_name}(\n    $joined_declaration,\n    CallbackFunction callback // \[&\](const char *buffer, size_t bytes) -> bool { ... }\n  )"

  lappend output(CALLBACK) "typedef bool (Class::*${function_name}_t)([join $declaration {, }]);" \
                           "${function_name}_t ${function_name};"

  puts [join $arguments \n]
  puts "\n\ndeclaration=\n"
  puts [join $declaration]
  puts "\n\ninvocation=\n"
  puts [join $invocation]
  puts "\n\nsymbols=\n"
  puts [join $symbols]

  return [array get output]

  # Create a C++ argument list for the function calls
  set _bits [list]         ; # each is [# bits, variable_name, int/float/double/enum, ~/~/<fixed/float>/<fixed/float>/EnumName]
  set _copy_structs [list] ; # <name> <struct>
  set _pack_structs [list]
  set _buffers [list] ; # buffer is just copied bytes
  set _strings [list]
  set _arrays [list]
  set _payload [list]
  foreach argument $arguments {
    lassign $argument type
    switch -- $type {
    int  -
    float -
    double -
    enum {
      lassign $argument - name subtype bits
      lappend _bits [list $bits $name $type $subtype]
    }
    buffer {
      lassign $argument - name max_bytes_symbol max_bytes
      set bytes_name "${name}_bytes"
      lappend _bits [list [bits_containing $max_bytes] $bytes_name int unsigned]
      lappend _buffers [list $name $bytes_name $max_bytes_symbol $max_bytes]
    }
    string {
      lassign $argument - name max_length_symbol max_length
      set length_name "${name}_length"
      lappend _bits [list [bits_containing $max_length] $length_name int unsigned]
      lappend _strings [list $name $length_name $max_length_symbol $max_length]
    }
    array {
      set subtype [lassign $argument - max_length_symbol max_length]
      set name [lindex $subtype 1]
      set count_name "${name}_count"
      lappend _bits [list [bits_containing $max_length] $count_name int unsigned]
      lappend _arrays [list $name $count_name $max_length_symbol $max_length $subtype]
    }
    struct {
      lassign $argument - name struct_type copy_pack
      if {$copy_pack eq "copy"} {
        lappend _copy_structs [list $name $struct_type]
      } else {
        lappend _pack_structs [list $name $struct_type]
      }
    }
    payload {
      lassign $argument - name struct_type copy_pack
      lappend _payload [list $name $struct_type $copy_pack]
    }
    }
  }


  # Create the encode/decode sets
  set decode_body [list]
  set encode_body [list]
  set buffer_bytes [list] ; # contains elements of the buffer size expression
  set decode_variables [list]

  # Preamble for Decode
  #-----------------------------------------------------------
  lappend decode_body "if (functions && !functions->${function_name}) return false;"

  # Preamble for Encode
  #-----------------------------------------------------------
  lappend encode_body "char *__write_ptr = __buffer;"
  lappend encode_body "*(unsigned int*)(__write_ptr) = $function_symbol;"
  lappend encode_body "__write_ptr += sizeof(unsigned short);"
  lappend buffer_bytes "sizeof(unsigned short)"
  lappend min_bytes "sizeof(unsigned short)"

  # Sort all the bits to encode
  set _bits [lsort -index 0 -integer $_bits]

  # Encoding Bits
  set packbits32.base 0
  set packbits32.decode_word [list]
  proc packbits32 {expression decode_variable bits encode_var decode_var} {
    upvar packbits32.base base
    upvar packbits32.decode_word decode_word
    upvar $encode_var encode
    upvar $decode_var decode

    if {$bits >= 24} { error "Must have <= 24 bits to use packbits32" }
    if {$base == 0} {
      lappend decode \{ "unsigned int __bits = *(unsigned int*)__read_ptr;"
      lappend encode \{ "unsigned int __bits = 0;"
    } 
    set new_base [expr {$base + $bits}]
    if {$new_base > 32} {
      # finalize the completed number of bytes and shift the bitfield to push them out
      set bytes_encoded [expr {int($base / 8)}]
      lappend encode "*(unsigned int*)__write_ptr = __bits;"
      lappend encode "__write_ptr += $bytes_encoded;"
      lappend encode "__bits >>= [expr {8*$bytes_encoded}];"
      incr base [expr {-$bytes_encoded * 8}]
      lappend encode \{
      lappend encode "  unsigned int __straddle = $expression;"
      lappend encode "  __bits |= __straddle & ((~(unsigned int)0) >> $underflow);"
      lappend encode "  *(unsigned int*)__write_ptr = __bits;"
      lappend encode "  __write_ptr += sizeof(unsigned int);"
      lappend encode "  __bits = __straddle >> $underflow;"
      lappend encode \}
      lappend decode_word "__bits >>= $underflow;"
      lappend decode_word "$decode_variable = __bits & ((~(unsigned int)0) >> (32-$underflow));"
      set decode [concat $decode [lreverse $decode_word]]
      lappend decode \} \{ "unsigned int __bits = *(unsigned int*)(__read_ptr);"
      set $decode_word [list "$decode_variable |= (__bits & ((~(unsigned int 0) >> (32 - $overflow))) << $underflow;" \
      set base $overflow
    } else {
      lappend encode "__bits |= ($expression) & ((~(unsigned int)0) >> (32 - $bits));"
      lappend encode "__bits <<= $bits;"
      lappend decode_word "__bits >>= $bits;"
      lappend decode_word "$decode_variable = __bits & ((~(unsigned int)0) >> (32-$bits));"
      set base $new_base
      if {$base == 32} {
        lappend encode "__write_ptr += sizeof(unsigned int)" \} \{ "unsigned int __bits = 0;"
        set decode [concat $decode [lreverse $decode_word]]
        set decode_word [list]
        lappend decode "__read_ptr += sizeof(unsigned int);" \}
        set base 0
      }
    }

    puts "encode == $encode"
  }
  proc packbits32_finalize {encode_var decode_var} {
    upvar packbits32.base base
    upvar packbits32.decode_word decode_word
    upvar $encode_var encode
    upvar $decode_var decode
    if {$base == 0} { return [list [list] [list]] }
    set decode [concat $decode [lreverse $decode_word]]
    set decode_word [list]
    set bytes [expr {int(($base+7) / 8)}]
    lappend decode "__read_ptr += $bytes;" \}
    lappend encode "*(unsigned int*)__write_ptr = __bits;"
    lappend encode "__write_ptr += $bytes;" \}
  }

  # Bit Field Encoding
  #-----------------------------------------------------------
  set bits_written 999
  set bits_group [list]
  set bits_group_decode [list]
  set declarations_left [llength $_bits]
  foreach bits_decl $_bits {
    lassign $bits_decl bits name type subtype
    if {$type eq "int" && $subtype eq "unsigned"} {
      lappend decode_variables "unsigned int $name;"
      packbits32 $name $name $bits encode_body decode_body
    } elseif {$type eq "enum"} {
      lappend decode_variables "$subtype $name;"
      packbits32 "(unsigned int)$name" $name $bits encode_body decode_body
    } elseif {$type eq "int" && $subtype eq "signed"} {
      lappend decode_variables "int $name;"
      packbits32 "*(unsigned int*)&$name" $name $bits encode_body decode_body
    } elseif {$type eq "float" || $type eq "double"} {
      lappend decode_variables "$type $name;"
      # must be converted from fixed point
      if {$subtype ne "fixed"} {
        lappend bits_group_decode "#error $type not getting converted to fixed point: $name"
      } else {
        lappend bits_group_decode "__fromfixed${bits}(*(unsigned int*)(__read_ptr), &${name});"
      }
    }
    if {$bits == 64} {
      if {$type eq "double" && $subtype eq "float"} {
        lappend encode_body "*(double*)(__write_ptr) = $name;"
        lappend encode_body "__write_ptr += sizeof(double);"
        lappend buffer_bytes "sizeof(double)"
        lappend min_bytes "sizeof(double)"
        lappend decode_body "__read_ptr += sizeof(double);"
        lappend decode_body "${name} = *(double*)(__read_ptr);"
        lappend decode_variables "double ${name};"
      } else {
        lappend encode_body "#error No 64-bit type other than 'double' encoded as 'float64' is recognized: $bits_decl"
      }
      continue;
    }
  }
  packbits32_finalize encode_body decode_body

  # Copy Structs
  #-----------------------------------------------------------
  foreach copy_struct $_copy_structs {
    lassign $copy_struct name subtype
    lappend decode_variables "$subtype $name;"
    lappend encode_body "memcpy(__write_ptr, ${name}, sizeof($subtype));"
    lappend encode_body "__write_ptr += sizeof($subtype);"
    lappend buffer_bytes "sizeof($subtype)"
    lappend min_bytes "sizeof($subtype)"
    lappend decode_body "memcpy(${name}, __read_ptr, sizeof($subtype));"
    lappend decode_body "__read_ptr += sizeof($subtype);"
  }

  # Pack Structs
  #-----------------------------------------------------------
  foreach pack_struct $_pack_structs {
    lassign $pack_struct name subtype
    lappend decode_variables "$subtype $name;"
    lappend encode_body "#error Incomplete pack structs"
    lappend decode_body "#error Incomplete pack structs"
  }

  # Buffers
  #-----------------------------------------------------------
  foreach buffer $_buffers {
    lassign $buffer name bytes_name max_bytes_symbol max_bytes
    lappend decode_variables "char ${name}\[$max_bytes_symbol\];"
    lappend encode_body "if (${bytes_name} > $max_bytes_symbol) return false;"
    lappend encode_body "memcpy(__write_ptr, ${name}, $bytes_name);"
    lappend encode_body "__write_ptr += $bytes_name;"
    lappend buffer_bytes "$max_bytes_symbol"
    lappend decode_body "if (${bytes_name} > $max_bytes_symbol) return false;"
    lappend decode_body "if (__read_ptr + ${bytes_name} > __read_end) return false;"
    lappend decode_body "memcpy(${name}, __read_ptr, ${bytes_name});"
    lappend decode_body "__read_ptr += ${bytes_name};"
  }

  # Strings
  #-----------------------------------------------------------
  foreach string $_strings {
    lassign $string name length_name max_length_symbol max_length
    lappend encode_body "if (${length_name} > $max_length_symbol) return false;"
    lappend encode_body "memcpy(__write_ptr, ${name}, sizeof(char) * $length_name); // TODO: compress strings"
    lappend encode_body "__write_ptr += sizeof(char) * $length_name;"
    lappend buffer_bytes "($max_length_symbol * sizeof(char))"

    lappend decode_variables "char ${name}\[$max_length_symbol+1\];" ; # +1 for null terminator
    lappend decode_body "if (${length_name} > $max_length_symbol) return false;"
    lappend decode_body "if (__read_ptr + (sizeof(char) * ${length_name}) > __read_end) return false;"
    lappend decode_body "memcpy($name, __read_ptr, sizeof(char) * ${length_name});"
    lappend decode_body "__read_ptr += sizeof(char) * ${length_name};"
    lappend decode_body "$name\[${length_name}\] = '\\0';"
  }

  # Arrays
  #-----------------------------------------------------------
  foreach array $_arrays {
  puts "array = $array"
    lassign $array name count_name max_length_symbol max_length subtype
    lappend encode_body "if (${count_name} > ${max_length_symbol}) return false;"
    lappend decode_body "if (${count_name} > ${max_length_symbol}) return false;"
    set keyword {}
    set encode_array [list]

    lassign $subtype subtype_type
    switch -- $subtype_type {
    float -
    double -
    enum -
    int  {

    }
    struct {
      lassign $argument - name struct_type copy_pack
      if {$copy_pack eq "copy"} {

        lappend buffer_bytes "(sizeof($struct_type) * $max_length)"
        lappend encode_body "memcpy(__write_ptr, ${name}, sizeof($struct_type) * $count_name);"
        lappend encode_body "__write_ptr += sizeof($struct_type)*$count_name);"

        lappend decode_variables "$struct_type ${name}\[$max_length_symbol\];"
        lappend decode_body "if (__read_ptr + sizeof($struct_type)*${name} > __read_end) return false;"
        lappend decode_body "memcpy($name, __read_ptr, sizeof($struct_type)*$count_name);"
        lappend decode_body "__read_ptr += sizeof($struct_type)*$count_name);"
      } else {
        error "Packed structs are not supported yet"
      }
    }
    buffer -
    payload -
    string {
      error "Arrays of this type are not supported: $subtype_type"
    }
    default {
      error "Unknown subtype: $subtype_type"
    }
    }


#    lappend buffer_bytes "($max_length_symbol * sizeof($keyword))"
  }

  # Postambles
  #-----------------------------------------------------------
  # Add an extra few bytes at the end of the buffer and add an overflow check
  set sentinel "0x7C7C7C7C"
  lappend buffer_bytes "4"
  lappend encode_body "assert(*(unsigned int*)(__buffer + sizeof(__buffer) - sizeof(unsigned int)) == $sentinel);"

  # Last thing to do is invoke the callback
  lappend encode_body "return callback(__buffer, __write_ptr - __buffer);"

  set joined_invocation [join $invocation ", "]
  lappend decode_body "return !functions || (object->*(functions->$function_name))($joined_invocation);"


  # Output the encode and decode statements
  #-----------------------------------------------------------
  #puts [join $arguments  \n]
  set max_bytes [join $buffer_bytes "+"]
  set buffer_setup [list]
  lappend buffer_setup "char __buffer\[$max_bytes\];"
  lappend buffer_setup "const char *__write_end = __buffer + sizeof(__buffer);"
  lappend buffer_setup "#ifdef _DEBUG"
  lappend buffer_setup "*(unsigned int*)(__buffer + sizeof(__buffer) - sizeof(unsigned int)) = $sentinel;"
  lappend buffer_setup "#endif"
  set encode_body [join [concat $buffer_setup $encode_body] "\n  "]
  set joined_declaration_lines [join $declaration_lines "\n//  "]
  lappend output(ENCODE) "//$first_declaration_line\n//  $joined_declaration_lines\n$encode_prototype \{\n  $encode_body\n\}"

  set decode_body [concat [list "if (bytes < ([join $min_bytes +])) return false;"] $decode_body]
  set spaced_decode_body [string map {\n "\n  "} "  [join $decode_body \n]"]
  set spaced_decode_variables   [string map {\n "\n  "} "  [join $decode_variables \n]"]
  lappend output(DECODE) "case $function_symbol: {\n$spaced_decode_variables\n\n$spaced_decode_body\n}"

return [array get output]

  puts [join $arguments \n]\n
  puts "bits {"
  puts [join $_bits \n]\n
  puts "}"
  puts "copy_structs {"
  puts [join $_copy_structs \n]\n
  puts "}"
  puts "pack_structs {"
  puts [join $_pack_structs \n]\n
  puts "}"
  puts "strings {"
  puts [join $_strings \n]\n
  puts "}"
  puts "arrays {"
  puts [join $_arrays \n]\n
  puts "}"
  puts "payload {"
  puts [join $_payload \n]\n
  puts "}"

  return [array get output]
}



#-----------------------------------------------------------------------------------------
proc processFunctionDeclarationFirstLine {line function_name_var function_symbol_var function_index_var} {
 upvar $function_name_var function_name
 upvar $function_symbol_var function_symbol
 upvar $function_index_var function_index
  if {![regexp {^declare\s+([a-zA-Z0-9_]+)\s*[@]\s*([a-zA-Z0-9_]+)\s*[=]\s*(\d+)$} $line - function_name function_symbol function_index]} {
    puts "Declaration line invalid: $line"
    return "#error Invalid declaration line: $line"
  }
  return [list $function_name $function_symbol $function_index]
}

#-----------------------------------------------------------------------------------------
proc readArgumentsFromFunctionDeclaration {lines} {
  set arguments [list]
  foreach line $lines { lappend arguments [processArgumentDeclaration $line] }
  return $arguments
}

#-----------------------------------------------------------------------------------------
proc processArgumentDeclaration {line} {
  if {![regexp {^([a-zA-Z0-9_]+)\s*[:]\s*(.*)$} $line - name the_rest]} {
    return [list "invalid" "Invalid 'declare' on first line" $line]
  }
  return [processArgumentTypeFromDeclaration $name $the_rest]
}

#-----------------------------------------------------------------------------------------
proc processArgumentTypeFromDeclaration {name declaration} {
  array set retval "name $name"

  # Simple Types
  # ===

  # (u)int
  if {[regexp {^(u?)int\s*{\s*(?:bits\s*[:]\s*)?(\d+)}$} $declaration - unsigned bits]} {
    set retval(bits) $bits
    set retval(type) [expr {($unsigned eq "u") ? "uint" : "int"}]
    return [array get retval]
  }

  # fixed
  if {[regexp {^fixed\s*{\s*(?:bits\s*[:]\s*)?(\d+)[.](\d+)}$} $declaration - bits_whole bits_fract]} {
    set retval(type) "fixed"
    set retval(bits_whole) $bits_whole
    set retval(bits_fract) $bits_fract
    return [array get retval]
  }

  # percent
  if {[regexp {^percent\s*{\s*(?:bits\s*[:]\s*)?(\d+)}$} $declaration - bits]} {
    set retval(type) "percent"
    set retval(bits) $bits
    return [array get retval]
  }

  # enum
  if {[regexp {^enum\s*{\s*(?:bits\s*[:]\s*)?(\d+)\s*}\s*([a-zA-Z_]+)$} $declaration - bits enum_type]} {
    set retval(type) "enum"
    set retval(enum_type) $enum_type
    set retval(bits) $bits
    return [array get retval]
  }

  # float / double
  if {[regexp {^((?:float)|(?:double))$} $declaration - type]} {
    set retval(type) $type
    return [array get retval]
  }

  # blob
  if {[regexp {^blob\s*{\s*(?:\s*bytes\s*[:]\s*)?([a-zA-Z_]+)\s*=\s*(\d+)\s*}$} $declaration - symbol bytes]} {
    set retval(type) "blob"
    set retval(symbol) $symbol
    set retval(bytes) $bytes
    return [array get retval]
  }

  # struct
  if {[regexp {^struct\s*((?:{\s*pack\s*})?)\s*([a-zA-Z_]+)$} $declaration - pack struct_type]} {
    set retval(type) "struct"
    set retval(pack) [expr {[string match -nocase "*pack*" $pack] ? "pack" : "copy"}]
    set retval(struct_type) $struct_type
    return [array get retval]
  }

  # Complex Types
  # ===

  error "complex type"

  return [array get retval]
}


#-----------------------------------------------------------------------------------------
proc bits_containing {value} {
  set k 1
  while {$k <= $value} { set k [expr {$k * 2}] }
  return [expr {int(log($k)/log(2))}]
}



#-----------------------------------------------------------------------------------------
proc getFunctionDeclaration {arguments} {
  set declaration [list]
  set invocation [list]
  set symbols [list]
  foreach argument_data $arguments {
    array set arg $argument_data
    set name $arg(name)
    switch -- $arg(type) {
    int {
      lappend declaration "int $name"
      lappend invocation "$name"
    }
    uint {
      lappend declaration "unsigned int $name"
      lappend invocation "$name"
    }
    enum {
      lappend declaration "$arg(enum_type) $name"
      lappend invocation "$name"
    }
    fixed -
    percent -
    float {
      lappend declaration "float $name"
      lappend invocation "$name"
    }
    double {
      lappend declaration "double $name"
      lappend invocation "$name"
    }
    blob {
      lappend declaration "const void *$name"
      lappend invocation "$name"
    }
    struct {
      lappend declaration "const $arg(struct_type) *$name"
      lappend invocation "&${name}"
    }
    buffer {
      lappend symbols $arg(max_length_symbol) $arg(max_length)
      lappend declaration "const void *$name"
      lappend declaration "size_t ${name}_bytes"
      lappend invocation $name ${name}_bytes
    }
    string {
      lappend symbols $arg(max_length_symbol) $arg(max_length)
      lappend declaration "const char *$name"
      lappend declaration "size_t ${name}_length"
      lappend invocation $name ${name}_length
    }
    array {
      lappend symbols $arg(max_length_symbol) $arg(max_length)
      lassign $subtype_decl subtype name subsubtype subsize
      switch -- $subtype {
      int {
        if {$subsubtype eq "unsigned"} {
          set array_type "unsigned int" 
        } else {
          set array_type "int"
        }
      }
      float -
      double {
        set array_type $subtype
      }
      struct -
      enum {
        set array_type "$subsubtype"
      }
      string {
        error "Arrays of strings are not supported"
      }
      default {
        error "Unknown subtype: $subtype"
      }
      }
      lappend declaration "const $array_type *$name"
      lappend declaration "size_t ${name}_count"
      lappend invocation "$name" "${name}_count"
    }
    payload {
      set subtype_decl [lassign $argument - max_length_symbol max_length]
      lappend symbols $max_length_symbol $max_length
      lassign $subtype_decl subtype name subsubtype subsize
      switch -- $subtype {
      int {
        if {$subsubtype eq "unsigned"} {
          set array_type "unsigned int" 
        } else {
          set array_type "int"
        }
      }
      float -
      double {
        set array_type $subtype
      }
      struct -
      enum {
        set array_type "$subsubtype"
      }
      string {
        error "Payloads of strings are not supported"
      }
      default {
        error "Unknown subtype: $subtype"
      }
      }
      lappend declaration "const $array_type *$name"
      lappend declaration "size_t ${name}_count"
      lappend invocation "$name" "${name}_count"
    }
    }
  }
  return [list $declaration $invocation $symbols]
}

main $argv
