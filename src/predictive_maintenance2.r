#args <- "args.csv"
########################### program start
args <- commandArgs(trailingOnly = T)
if (length(args) == 0 )
{
	args <- c("predictive_maintenance_args.csv")
}

print("=== args ===")
print(args)
print("============")


curdir = getwd()
setwd( paste(curdir, "/work", sep=""))
putpng_path= paste(curdir, "/images/", sep="")

source("parameters.r")

argS <- read.csv(args[1], fileEncoding  = csv_encoding)
varN <- as.integer(argS[1,1])
tracking_feature_ <<- NULL
for ( i in 1:varN )
{
	tracking_feature_ <<- c(tracking_feature_, argS[1+i,1] )
}
print(tracking_feature_)
timeStamp_arg = argS[varN+2,1]
print(timeStamp_arg)
sigin_arg = argS[varN+3,1]
print(sigin_arg)
tracking_feature_args <<- NULL
for ( i in (varN+4):nrow(argS) )
{
	tracking_feature_args <<- c(tracking_feature_args, argS[i,1] )
}
print(tracking_feature_args)


source("../src/predictive_maintenance_funcs.r")
source("parameters.r")

feature_param_csv <<- paste("./", base_name, "_feature_param.csv", sep='')

initial_pm(sigin_arg)
abnormality_detected_data <- FALSE
watch_name <<- paste(tracking_feature_, "..", sep="")

files <- get_csvfile()

print("============================")
print(tracking_feature_)
print(tracking_feature_args)
print("============================")
print("============================")
print(timeStamp_arg)
print("============================")
print("one_input============================")
print(one_input)
print("============================")

if ( file.exists("./break_index_df.csv"))
{
	file.remove("./break_index_df.csv")
}
while( TRUE )
{
	if ( is.null(files))
	{
		#next
		break
	}
	for ( i in 1:length(files))
	{
		file = files[i]
		file.copy(paste("Untreated\\",file,sep=""), getwd())
		delete_csvfile(i)
		
		df <- NULL
		tryCatch({
			df <- get_csvdata(file, tracking_feature_, timeStamp_arg)
		},error <- function(e){
			df <- NULL
		},finally = { 
			#OK
		}, silent = T
		)
		if ( is.null(df) )
		{
			next
		}
		print(sprintf("========== %s ===========", file))
		print(head(df))
		predictin(df, tracking_feature_args, timeStamp_arg, sigin_arg)
		save.image("./predictive_maintenance.RData")
		
		file.remove(file)
	}

	files <- get_csvfile()
}


