call init.bat

:call ..\..\..\setup_ini.bat

set  R_LIBS_USER=./lib

"%R_INSTALL_PATH%\bin\x64\Rscript.exe" ../mat2csv.r
:"%R_INSTALL_PATH%\bin\x64\R.exe" CMD BATCH --slave --vanilla ../mat2csv.r
