call init.bat
set R_LIBS_USER=%~dp0\library

set  R_LIBS_USER=.\library

cd %~dp0


"%R_INSTALL_PATH%\bin\x64"\Rgui.exe
