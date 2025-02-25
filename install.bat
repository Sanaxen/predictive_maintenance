call init.bat

"%RPATH%\R" CMD BATCH --slave --vanilla install.r

call bin_exppand.bat

call mat2csv.bat

