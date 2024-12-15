curdir = getwd()
setwd( paste(curdir, "/work", sep=""))
putpng_path= paste(curdir, "/images/", sep="")

########################### program start

source("../src/predictive_maintenance_funcs.r")
source("../src/parameters.r")

initial_pm()

abnormality_detected_data <- FALSE

file = "xxxxxxx.csv"

df <- get_csvdata(file)

if (abnormality_detected_data)
{
	feature_summary_visualization(df, feature_param)
	feature_param
	features_plot(NULL)
}

feature_param <- NULL
images_clear()

predictin(df)
save.image("./predictive_maintenance.RData")

