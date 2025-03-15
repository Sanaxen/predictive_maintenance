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
	DataSet <- c(DataSet, c(dataset,timeStamp))

	dataset <- "./dataset/Turbofan_Jet_Engine/NASA_Turbofan_Jet_Engine_Data_train_FD001_Uint1.csv"
	timeStamp="cycles_time_dummy"
	DataSet <- c(DataSet, c(dataset,timeStamp))


	dataset <- "./dataset/miiling_case2/miiling_case2.csv"
	timeStamp="datetime"
	DataSet <- c(DataSet, c(dataset,timeStamp))

	dataset <- "./dataset/Turbofan_Jet_Engine2/NASA_Turbofan_Jet_Engine_Data_train_FD004_Unit1.csv"
	timeStamp="cycles_time_dummy"
	DataSet <- c(DataSet, c(dataset,timeStamp))
	
}

if ( Detection_precursor_phenomenaTest_Test )
{
	if (length(DataSet)>0 )
	{			
		n <- length(DataSet)/2
		
		index_number <- 1
		for ( i in 1:n )
		{
			dataset <- DataSet[2*(i-1)+1]
			timeStamp <- DataSet[2*(i-1)+2]
			TestFuc(dataset, timeStamp, index_number)
			index_number <- index_number + 1
		}
	}else
	{
			TestFuc(dataset, timeStamp, 0)
	}	
}


