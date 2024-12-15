call  init.bat

:call ..\..\..\setup_ini.bat

set  R_LIBS_USER=.\lib

"%R_INSTALL_PATH%\bin\x64"\Rgui.exe
