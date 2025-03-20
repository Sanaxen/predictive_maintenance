options(encoding = "utf-8")
options(digits.secs=3)
library(ggplot2)
library(gridExtra)
library(zoo)
library(scales)

library(energy)
library(minerva)

library(lightgbm)

if (exists("delta_time")) 
{
	delta_time = 1
}
if (exists("delta_index")) 
{
	delta_time = 1
}
if (exists("current_time")) 
{
	current_time = ""
}
if (exists("lookback")) 
{
	lookback = 30
}

source("./src/Detection_precursor_phenomena.r")


Detection_precursor_phenomenaTest_Test <<- T


if (Detection_precursor_phenomenaTest_Test == T )
{
	args <- commandArgs(trailingOnly = T)
	if (length(args) != 0 )
	{
		dataset <- args[1]
		cat("input data:")
		print(dataset)
	}
	if (length(args) > 1 )
	{
		timeStamp <- args[2]
		cat("timeStamp:")
		print(timeStamp)
	}
}

DataSet <- c()
if (length(args) == 0 )
{
	dataset <- "./dataset/vibration_data/01st-day-vibration-2013_03_07 01_57_46.000.csv"
	timeStamp="datetime"
	window <- 960
	slide <- 240
	DataSet <- c(DataSet, c(dataset,timeStamp, window, slide))

	dataset <- "./dataset/Turbofan_Jet_Engine/NASA_Turbofan_Jet_Engine_Data_train_FD001_Uint1.csv"
	timeStamp="cycles_time_dummy"
	window <- 24
	slide <- 6
	DataSet <- c(DataSet, c(dataset,timeStamp, window, slide))

	dataset <- "./dataset/miiling_case2/miiling_case2.csv"
	timeStamp="datetime"
	window <- 960
	slide <- 240
	DataSet <- c(DataSet, c(dataset,timeStamp, window, slide))
	

	dataset <- "./dataset/Turbofan_Jet_Engine2/NASA_Turbofan_Jet_Engine_Data_train_FD004_Unit1.csv"
	timeStamp="cycles_time_dummy"
	window <- 48/2
	slide <- 12/2
	DataSet <- c(DataSet, c(dataset,timeStamp, window, slide))

	
}

if ( Detection_precursor_phenomenaTest_Test )
{
	if (length(DataSet)>0 )
	{			
		n <- length(DataSet)/4
		
		index_number <- 1
		for ( i in 1:n )
		{
			dataset <- DataSet[4*(i-1)+1]
			timeStamp <- DataSet[4*(i-1)+2]
			window <- DataSet[4*(i-1)+3]
			slide <- DataSet[4*(i-1)+4]
			TestFuc(dataset, timeStamp, as.numeric(window), as.numeric(slide), index_number)
			index_number <- index_number + 1
			#quit()
		}
	}else
	{
			TestFuc(dataset, timeStamp, 0)
	}	
}


