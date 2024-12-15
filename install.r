org_libpath <- .libPaths()

curdir = getwd()


install_libpath = paste(curdir, "/library", sep="")

.libPaths( c(install_libpath))




install.packages("ggplot2", repos = "http://cran.us.r-project.org",dependencies=TRUE, lib=install_libpath)
install.packages("doParallel", repos = "http://cran.us.r-project.org",dependencies=TRUE, lib=install_libpath)

install.packages("gsignal", repos = "http://cran.us.r-project.org",lib=install_libpath)
install.packages("forecast", repos = "http://cran.us.r-project.org",lib=install_libpath)
install.packages("tidyverse", repos = "http://cran.us.r-project.org",lib=install_libpath)
install.packages("tibbletime", repos = "http://cran.us.r-project.org",lib=install_libpath)
install.packages("minpack.lm", repos = "http://cran.us.r-project.org",lib=install_libpath)
install.packages("cowplot", repos = "http://cran.us.r-project.org",lib=install_libpath)
install.packages("plotly", repos = "http://cran.us.r-project.org",lib=install_libpath)
install.packages("outliers", repos = "http://cran.us.r-project.org",lib=install_libpath)
install.packages("ggridges", repos = "http://cran.us.r-project.org",lib=install_libpath)
.libPaths( org_libpath)

