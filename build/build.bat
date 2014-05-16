@echo off
REM http://www.kocjan.org/tclmentor/10-tclkits-building-standalone-tcl-binaries.html

%~dp0\tclkitsh-win32.exe %~dp0\sdx.kit wrap %~dp0\..\compile.exe -runtime %~dp0\tclkitsh.exe
