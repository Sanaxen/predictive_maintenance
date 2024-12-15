options(encoding = "utf-8")

org_libpath <- .libPaths()

curdir = getwd()


install_libpath = paste(curdir, "/lib", sep="")

.libPaths( c(install_libpath))
print(.libPaths())

files=c(
"data-20130307T015746Z.mat",
"data-20130308T023421Z.mat",
"data-20130309T023343Z.mat",
"data-20130310T030102Z.mat",
"data-20130311T030024Z.mat",
"data-20130312T061710Z.mat",
"data-20130313T063404Z.mat",
"data-20130314T065041Z.mat",
"data-20130315T065003Z.mat",
"data-20130316T065643Z.mat",
"data-20130317T065604Z.mat",
"data-20130317T184756Z.mat",
"data-20130318T184715Z.mat",
"data-20130320T003354Z.mat",
"data-20130321T003314Z.mat",
"data-20130322T003950Z.mat",
"data-20130323T003911Z.mat",
"data-20130324T004549Z.mat",
"data-20130325T004512Z.mat",
"data-20130326T014150Z.mat",
"data-20130327T035827Z.mat",
"data-20130328T095531Z.mat",
"data-20130329T095505Z.mat",
"data-20130330T100142Z.mat",
"data-20130331T193818Z.mat",
"data-20130401T193739Z.mat",
"data-20130402T194415Z.mat",
"data-20130403T212942Z.mat",
"data-20130404T212901Z.mat",
"data-20130405T213537Z.mat",
"data-20130406T221209Z.mat",
"data-20130407T221131Z.mat",
"data-20130408T221809Z.mat",
"data-20130409T231445Z.mat",
"data-20130410T231407Z.mat",
"data-20130411T231358Z.mat",
"data-20130412T170231Z.mat",
"data-20130413T170210Z.mat",
"data-20130414T170847Z.mat",
"data-20130415T225524Z.mat",
"data-20130416T230159Z.mat",
"data-20130417T230120Z.mat",
"data-20130418T230803Z.mat",
"data-20130419T230747Z.mat",
"data-20130420T151307Z.mat",
"data-20130421T151231Z.mat",
"data-20130422T151908Z.mat",
"data-20130423T151830Z.mat",
"data-20130424T215514Z.mat",
"data-20130425T232202Z.mat"
)


install.packages("R.matlab", repos = "http://cran.us.r-project.org",dependencies=TRUE, lib=install_libpath)

url0 <- "https://github.com/mathworks/WindTurbineHighSpeedBearingPrognosis-Data/raw/main/"
destfile <- "./WindTurbineHighSpeedBearingPrognosis-Data-main"

for ( i in 1:length(files))
{
	url <- paste(url0, files[i], sep="")
	dest <- paste(destfile, "/", files[i], sep="")
	download.file(url, dest, mode = "wb")
}

library(R.matlab)

#https://github.com/mathworks/WindTurbineHighSpeedBearingPrognosis-Data
get_matfile <- function()
{
	cmdstr='cmd /c dir '
	cmdstr=paste(cmdstr, "\"", getwd(), sep="")
	cmdstr=paste(cmdstr,"\\WindTurbineHighSpeedBearingPrognosis-Data-main\\*.mat\" /b /od", sep="")

	files <- NULL
	while( TRUE )
	{
		tryCatch({
				files = system(cmdstr, intern=T)
				print(files)
				if ( files[1] != "file not found")
				{
					break
				}
			},error = function(e)
			{
				files <- NULL
			},finally = { 
	    	},silent = TRUE
    	)
	}
	
	return (files)
}

options(digits.secs=3)
files <- get_matfile()

path="./WindTurbineHighSpeedBearingPrognosis-Data-main/"

for ( i in 1:length(files))
{
	mat <- readMat( paste(path,files[i], sep=""))
	mat <- mat$vibration
	n = length(mat)
	
	step = (1/n)*60*60*24

	if ( i == 1 )
	{
		start <- as.POSIXct("2013-03-07 01:57:46.001", format = "%Y-%m-%d %H:%M:%OS")
		start0 <- start
	}

	datetime <- seq(from = start,length.out = n, by = step)
	datetime <- as.POSIXct(datetime, format = "%Y-%m-%d %H:%M:%OS")
	
	df <- data.frame(datetime=datetime, vibration=mat)
	
	s = "th"
	if ( i==1 )
	{
		s = "st"
	}
	if ( i==2 )
	{
		s = "nd"
	}
	if ( i==3 )
	{
		s = "rd"
	}
	write.csv(df, paste( sprintf("../dataset/vibration_data/%02d%s-day-vibration-", i, s), 
		gsub("-","_",gsub(":","_",start0)), ".csv",sep=""), row.names=F)
	
	start <- datetime[n]+step
	start0 <- start
	print(sprintf("%d/%d", i, length(files)))
}

	
