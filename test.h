#include <memory.h>
#include <assert.h>


enum InterfaceElements {
  MSG_TELLACTORNAME = 8, // tellActorName

  INTERFACE_ELEMENT_COUNT
};

template <typename Class>
struct EditorFunctionTable {
};

template <typename Class>
bool decode(const char *buffer, size_t bytes, Class *object, EditorFunctionTable<Class> *functions) {
  const char *__read_ptr = buffer + sizeof(unsigned short);
  const char *__read_end = buffer + bytes;
  if (!buffer || __read_ptr > __read_end) return false;
  unsigned int __msg = *(unsigned short*)buffer;
  switch (__msg) {
  }
  return false; // todo: log error
}
