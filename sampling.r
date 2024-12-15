options(encoding = "sjis")

curdir = getwd()
curdir

get_csvfile <- function()
{
	cmdstr='cmd /c dir '
	cmdstr=paste(cmdstr, "\"", getwd(), sep="")
	cmdstr=paste(cmdstr,"\\vibration_data\\*.csv\" /b /od", sep="")

	files <- NULL
	while( TRUE )
	{
		tryCatch({
				files = system(cmdstr, intern=T)
				print(files)
				if ( files[1] != "ƒtƒ@ƒCƒ‹‚ªŒ©‚Â‚©‚è‚Ü‚¹‚ñ")
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
files <- get_csvfile()

step = (1/585936)*60*60*24

start <- NULL
df <- read.csv( paste("vibration_data/", files[1], sep=""))
df$datetime <- as.POSIXct(df$datetime)
start <- as.POSIXct("2013-03-07 01:57:46.001", format = "%Y-%m-%d %H:%M:%OS")
start0 <- start


df$datetime <- seq(from = start,length.out = nrow(df), by = step)

df <- as.data.frame(df)
df2 <- NULL
sample <- 0.1

n <- nrow(df)*sample
r <- sort(sample(nrow(df), n))
df2 <- as.data.frame(df[r,])
print(nrow(df2))

for ( i in 2:length(files))
{
	start <- df$datetime[nrow(df)] + step
	start0 <- start
	
	df <- read.csv( paste("vibration_data/", files[i], sep=""))
	df$datetime <- seq(from = start,length.out = nrow(df), by = step )
	
	df <- as.data.frame(df)
	r <- sort(sample(nrow(df), n))
	df2 <- rbind(df2, as.data.frame(df[r,]))

	print(files[i])
	flush.console()
}
write.csv(df2, "vibration_sample.csv", row.names=F)
print(nrow(df2))



