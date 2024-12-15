options(encoding = "sjis")

curdir = getwd()
setwd( paste(curdir, "/work", sep=""))
putpng_path= paste(curdir, "/images/", sep="")

source("../src/predictive_maintenance_funcs.r")
source("parameters.r")
library(plotly)
source("../src/feature_summary_visualization.r")

sigin_arg = '+'
#tracking_feature_= 'ˆ—“±“d—¦'

initial_pm(sigin_arg)
abnormality_detected_data <- FALSE
#watch_name <<- paste(tracking_feature_, "..", sep="")

sigin <<- 1.0
if ( sigin_arg == '-' )
{
	sigin <<- -1.0
}

smooth_window <<- 0
smooth_window_slide <<- 0
smooth_window2 <<- 100
smooth_window_slide2 <<- 10

lookback <<- 50
lookback_slide <<- 10#•½ŠŠ‰»‚ğlowess‚Ås‚¤
use_lowess = T
smoother_span <<- 0.5


timeStamp <- 'ŠÄ‹“ú'
csvfile = '../testmini.csv'
plt <- feature_summary_visualization(csvfile, timeStamp)


