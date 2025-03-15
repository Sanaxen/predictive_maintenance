call init.bat
:call ..\..\setup_ini.bat

set  R_LIBS_USER=.\library


cd %~dp0

mkdir images\Detect
mkdir images\RUL
:del /Q images\*.txt
:del /Q images\*.png
:del /Q images\*.r
:del /Q images\debug\*.png
:del /Q images\RUL\*.png


"%R_INSTALL_PATH%\bin\x64\Rscript.exe" --vanilla ./src/Detection_precursor_phenomena_test.r %1 %2

