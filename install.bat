call init.bat

"%RPATH%\R" CMD BATCH --slave --vanilla install.r

call mat2csv.bat
call bin_exppand.bat

