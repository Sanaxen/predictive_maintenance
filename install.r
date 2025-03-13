org_libpath <- .libPaths()

curdir = getwd()


install_libpath = paste(curdir, "/library", sep="")

.libPaths( c(install_libpath))




install.packages("ggplot2", repos = "http://cran.us.r-project.org",dependencies=TRUE, lib=install_libpath, type = "binary")
install.packages("doParallel", repos = "http://cran.us.r-project.org",dependencies=TRUE, lib=install_libpath, type = "binary")

install.packages("gsignal", repos = "http://cran.us.r-project.org",lib=install_libpath, type = "binary")
install.packages("forecast", repos = "http://cran.us.r-project.org",lib=install_libpath, type = "binary")
install.packages("tidyverse", repos = "http://cran.us.r-project.org",lib=install_libpath, type = "binary")
install.packages("tibbletime", repos = "http://cran.us.r-project.org",lib=install_libpath, type = "binary")
install.packages("minpack.lm", repos = "http://cran.us.r-project.org",lib=install_libpath, type = "binary")
install.packages("cowplot", repos = "http://cran.us.r-project.org",lib=install_libpath, type = "binary")
install.packages("plotly", repos = "http://cran.us.r-project.org",lib=install_libpath, type = "binary")
install.packages("outliers", repos = "http://cran.us.r-project.org",lib=install_libpath, type = "binary")
install.packages("ggridges", repos = "http://cran.us.r-project.org",lib=install_libpath, type = "binary")
install.packages("crayon", repos = "http://cran.us.r-project.org",lib=install_libpath, type = "binary")

install.packages("energy", repos = "http://cran.us.r-project.org",lib=install_libpath, type = "binary")
install.packages("minerva", repos = "http://cran.us.r-project.org",lib=install_libpath, type = "binary")
#install.packages("rightgbm", repos = "http://cran.us.r-project.org",lib=install_libpath, type = "binary")
install.packages("lightgbm", repos = "https://cran.r-project.org",lib=install_libpath, type = "binary"))

.libPaths( org_libpath)

