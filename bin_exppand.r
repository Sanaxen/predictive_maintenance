org_libpath <- .libPaths()

curdir = getwd()


install_libpath = paste(curdir, "/library", sep="")

.libPaths( c(install_libpath))




install.packages("archive", repos = "http://cran.us.r-project.org",dependencies=TRUE, lib=install_libpath)

library(archive)

zip_file_path <- "./bin.7z"
dest_dir <- "."
archive_extract(zip_file_path, dir = dest_dir)

.libPaths( org_libpath)

