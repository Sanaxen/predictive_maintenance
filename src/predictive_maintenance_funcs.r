library(ggplot2)
library(dplyr)
library(e1071)
library(doParallel)

#install.packages("gsignal")
library(gsignal)

#install.packages("gridExtra")
library(gridExtra)
library(forecast)

library(tidyverse)
#install.packages("tibbletime")
library(tibbletime)

library(MASS) 
#install.packages("tibbletime")

#install.packages("minpack.lm")
library(minpack.lm)
library(data.table)

#install.packages("ggridges")
library(ggridges)

freeram <- function(...) invisible(gc(...))

time_data_length <- function(length, length_unit="")
{

	if ( length_unit=="" ) length_unit = unit_of_time
	
	t = length
	if ( unit_of_time == "ms" )
	{
		if ( length_unit == "month" )
		{
			t = 30*1000*60*60*24*length/unit_of_record
		}
		if ( length_unit == "day" )
		{
			t = 1000*60*60*24*length
		}
		if ( length_unit == "h" )
		{
			t = 1000*60*60*length/unit_of_record
		}
		if ( length_unit == "min" )
		{
			t = 1000*60*length/unit_of_record
		}
		if ( length_unit == "sec" )
		{
			t = 1000*length/unit_of_record
		}
	}
	if ( unit_of_time == "sec" )
	{
		if ( length_unit == "month" )
		{
			t = 30*60*60*24*length/unit_of_record
		}
		if ( length_unit == "day" )
		{
			t = 60*60*24*length/unit_of_record
		}
		if ( length_unit == "h" )
		{
			t = 60*60*length/unit_of_record
		}
		if ( length_unit == "min" )
		{
			t = 60*length/unit_of_record
		}
	}
	if ( unit_of_time == "min" )
	{
		if ( length_unit == "month" )
		{
			t = 30*60*24*length
		}
		if ( length_unit == "day" )
		{
			t = 60*24*length/unit_of_record
		}
		if ( length_unit == "h" )
		{
			t = 60*length/unit_of_record
		}
	}
	if ( unit_of_time == "h" )
	{
		if ( length_unit == "month" )
		{
			t = 30*24*length/unit_of_record
		}
		if ( length_unit == "day" )
		{
			t = 24*length/unit_of_record
		}
	}
	
	t = as.integer(t+0.5)
	return( t )
}


skewness_ <- function(x)
{
	n = length(x)
	mu = mean(x, na.rm = TRUE)
	m2 = sum((x - mu)^2)/n
	m3 = sum((x - mu)^3)/n

	g1 = m3/m2^(3/2)
	if ( m2 == 0 ) g1 = 0
	G1 = g1*sqrt(n*(n -1 ))/(n -2 )
	b1 = g1*((n-1)/n)^(3/2)
		
	return( c(g1, G1, b1))
}

abnormal_value <- function(f)
{
	mu <- mean(f, na.rm = T)
	sigma <- var(f, na.rm = T)
	
	a = (( f - mu)/sigma )^2
}

convert_time <- function(x, unit_of_record=1, from="day", to="day")
{
 if ( from == to )
 {
 	return(as.integer(unit_of_record*x))
 }
 if ( from == "day" )
 {
 	if ( to == "h" ) x = x*24
 	if ( to == "min" ) x = x*24*60
 	if ( to == "sec" ) x = x*24*60*60
 	if ( to == "ms" ) x = x*24*60*60*1000
 }
 if ( from == "h" )
 {
 	if ( to == "day" ) x = x/24
 	if ( to == "min" ) x = x*60
 	if ( to == "sec" ) x = x*60*60
 	if ( to == "ms" ) x = x*60*60*1000
 }
 if ( from == "min" )
 {
 	if ( to == "day" ) x = x/24/60
 	if ( to == "h" ) x = x/60
 	if ( to == "sec" ) x = x*60
 	if ( to == "ms" ) x = x*60*1000
 }
 if ( from == "sec" )
 {
 	if ( to == "day" ) x = x/24/60/60
 	if ( to == "h" ) x = x/60/60
 	if ( to == "mim" ) x = x/60
 	if ( to == "ms" ) x = x*1000
 }
 if ( from == "ms" )
 {
 	if ( to == "day" ) x = x/1000/60/60/24
 	if ( to == "h" ) x = x/1000/60/60
 	if ( to == "min" ) x = x/1000/60
 	if ( to == "sec" ) x = x/1000
 }
 return( as.integer(unit_of_record*x))
}

feature_param_csv <<- "./feature_param.csv"
fixed_threshold_value = FALSE
init_feature_param <- function(f2, threshold, ymax, ymin)
{
	if ( is.null(feature_param) && file.exists(feature_param_csv))
	{
		feature_param <<-  read.csv( feature_param_csv, header=T, stringsAsFactors = F, na.strings = c("", "NA"))
		fixed_threshold_value <<- TRUE
		return(feature_param)
	}
	
	if ( is.null(feature_param))
	{
		feature_param <<- f2
		feature_param[,2] <<-  threshold
		feature_param[,3] <<-  ymax
		feature_param <<-  cbind(feature_param, feature_param[,3]*0+ymin)
		feature_param <<-  cbind(feature_param, feature_param[,3]*0)
		feature_param <<-  cbind(feature_param, feature_param[,3]*0)
		feature_param <<-  cbind(feature_param, feature_param[,3]*0)
		feature_param <<-  cbind(feature_param, feature_param[,3]*0)
		feature_param <<-  cbind(feature_param, feature_param[,3]*0)
	}
	colnames(feature_param) <<-  c("feature", "threshold", "ymax", "ymin", "count", "a", "b", "c", "d")
	try(write.csv(feature_param, feature_param_csv, row.names = F), silent = FALSE)

	return(feature_param)
}
get_feature_param <- function()
{
	if ( is.null(feature_param))
	{
		#print("feature_param")
		#print(feature_param)
		return(NULL)
	}
	feature_param <<-  read.csv( feature_param_csv, header=T, stringsAsFactors = F, na.strings = c("", "NA"))
	return (feature_param)
}

set_threshold <- function(feature_name, threshold_value)
{
	if ( fixed_threshold_value )
	{
		return(feature_param)
	}
	feature_param <<-  read.csv( feature_param_csv, header=T, stringsAsFactors = F, na.strings = c("", "NA"))

	id = which(feature_name == feature_param["feature"])
	feature_param["threshold"][id,] <<-  threshold_value

	try(write.csv(feature_param, feature_param_csv, row.names = F), silent = FALSE)
	
	return(feature_param)
}

set_ymax <- function(feature_name, ymax)
{
	feature_param <<- read.csv(feature_param_csv, header=T, stringsAsFactors = F, na.strings = c("", "NA"))

	id = which(feature_name == feature_param["feature"])
	feature_param["ymax"][id,] <<-  ymax
	
	write.csv(feature_param, feature_param_csv, row.names = F)
	return(feature_param)
}
set_ymin <- function(feature_name, ymin)
{
	feature_param <<- read.csv( feature_param_csv, header=T, stringsAsFactors = F, na.strings = c("", "NA"))

	id = which(feature_name == feature_param["feature"])
	feature_param["ymin"][id,] <<-  ymin
	
	write.csv(feature_param, feature_param_csv, row.names = F)
	return(feature_param)
}
set_count <- function(feature_name)
{
	feature_param <<-  read.csv( feature_param_csv, header=T, stringsAsFactors = F, na.strings = c("", "NA"))

	id = which(feature_name == feature_param["feature"])
	feature_param["count"][id,] <<-  feature_param["count"][id,]+1
	
	try(write.csv(feature_param, feature_param_csv, row.names = F), silent = FALSE)
	return(feature_param)
}

get_threshold <- function(feature_name)
{
	id = which(feature_name == feature_param["feature"])
	return (feature_param["threshold"][id,])
}
get_ymax <- function(feature_name)
{
	id = which(feature_name == feature_param["feature"])
	return (feature_param["ymax"][id,])
}
get_ymin <- function(feature_name)
{
	id = which(feature_name == feature_param["feature"])
	return (feature_param["ymin"][id,])
}

get_param <- function(feature_name)
{
	id = which(feature_name == feature_param["feature"])
	return ( c(feature_param["a"][id,],feature_param["b"][id,],feature_param["c"][id,],feature_param["d"][id,]))
}
set_param <- function(feature_name, a, b, c, d)
{
	feature_param <<-  read.csv( feature_param_csv, header=T, stringsAsFactors = F, na.strings = c("", "NA"))

	id = which(feature_name == feature_param["feature"])
	feature_param["a"][id,] <<-  a
	feature_param["b"][id,] <<-  b
	feature_param["c"][id,] <<-  c
	feature_param["d"][id,] <<-  d
	
	try(write.csv(feature_param, feature_param_csv, row.names = F), silent = FALSE)
	return(feature_param)
}

moving_sampling <- function(data, window_size, slide_size) {
  result <- NULL

  for (i in seq(1, length(data) - window_size + 1, by = slide_size)) {
    window <- data[i:(i + window_size - 1),]
    index <- runif(1,1,nrow(window))
    sample <- window[index,]
    result <- dplyr::bind_cols(result, sample)
  }
  return(as.data.frame(result))
}

get_data_frame<- function(file, timeStamp)
{
	print(sprintf("get_data_frame(%s)", file))
	print(sprintf("csv_encoding=%s", csv_encoding))
	
	df <- NULL
	if ( csv_encoding == "sjis" )
	{
		df <- try(
			read.csv( file, header=T, stringsAsFactors = F, na.strings = c("", "NA"), fileEncoding  = 'Shift_JIS')
		,silent=F)
		if ( class(df) == "try-error" || is.null(df) || nrow(df) == 0)
		{
			df <- fread(file, na.strings=c("", "NULL"), header = TRUE, stringsAsFactors = F)
		}
	}else
	{
		df <- try(
			fread(file, na.strings=c("", "NULL"), header = TRUE, stringsAsFactors = F)
		,silent=F)
		if ( class(df) == "try-error" || is.null(df)  || nrow(df) == 0)
		{
			df <- read.csv( file, header=T, stringsAsFactors = F, na.strings = c("", "NA"), fileEncoding  = 'Shift_JIS')
		}
	}
	df <- df %>%
	  mutate(across(where(is.numeric), ~ ifelse(is.na(.), mean(., na.rm = TRUE), .)))

	print(str(df))
	df <- as.data.frame(df)
	colnames(df) <- gsub("\\.", "_", colnames(df))

	print(timeStamp)
	print(sprintf("get_data_frame nrow(df):%d", nrow(df)))
	print(sprintf("get_data_frame ncol(df):%d", ncol(df)))
	print(str(df))
	#print(head(df))
	
	df_ <- NULL
	names <- NULL
	for ( i in 1:ncol(df))
	{
		if ( is.character(df[,i]) && colnames(df)[i]!=timeStamp && colnames(df)[i]!="maintenance")
		{
			df[,i] <- as.numeric(df[,i])
			#next
		}
		if ( !is.character(df[,i]))
		{
			if ( length(unique(diff(df[,i]))) == 1)
			{
				next
			}
		}
		if ( is.null(df_))
		{
			if ( colnames(df)[i]=="maintenance" )
			{
				x <- df[,i]
				df[is.na(x),i] <- 0
			}
			df_ <- df[,i]

			if ( colnames(df)[i]!=timeStamp )
			{
				df_ <- as.numeric(df[,i])
			}else
			{
				df_ <- as.character(df[,i])
			}
			names <- c(colnames(df)[i])
			#print(df_)
		}else
		{
			if ( colnames(df)[i]=="maintenance" )
			{
				x <- df[,i]
				df[is.na(x),i] <- 0
			}
		
			if ( colnames(df)[i]==timeStamp )
			{
				#df_ <- cbind(df_, as.character(df[,i]))
				df_ <- dplyr::bind_cols(df_, as.character(df[,i]))
			}else
			{
				#df_ <- cbind(df_, as.numeric(df[,i]))
				df_ <- dplyr::bind_cols(df_, as.numeric(df[,i]))
			}
			names <- c(names, colnames(df)[i])
			#print(df_)
		}
	}
	df <- as.data.frame(df_)
	
	colnames(df) <- names
	for ( i in 1:ncol(df))
	{
		if ( colnames(df)[i]!=timeStamp )
		{
			df[,i] <- as.numeric(df[,i])
		}
	}
	
	print(head(df))
	print(sprintf("get_data_frame ncol(df):%d", ncol(df)))	
	flush.console()

	return (df)
}

moving_average_sub0 <- function(sampling=TRUE, col_name, df2, ff = NULL, lookback=100, slide_window = 1)
{
	colnames_df2 <- colnames(df2)

	exist_timeStamp = TRUE
	time_index = which(timeStamp == colnames_df2)
	if ( length(time_index) < 1 )
	{
		time_index = which("time_index" == colnames_df2)
		exist_timeStamp = FALSE
	}
	if ( length(time_index) < 1 )
	{
		print("Date and time columns are required.")
		print("Must be a specified date/time column name or \"time_index\".")
		quit()
	}
	
	i <- which(col_name == colnames_df2)

	maintenance_index = which("maintenance" == colnames_df2)
	if ( length(maintenance_index) < 1  )
	{
		maintenance_index = 0
	}

	fff <- NULL
	d <- df2[,i]
	if ( i == maintenance_index )
	{
		d[is.na(d)] <- 0
	}
	if ( i == time_index )
	{
		d[is.na(d)] <- mean(d, na.rm = TRUE)
	}
	print("moving_average_sub")
	print(head(d))
	flush.console()
	
	#print(sprintf("lookback:%d length(d):%d", lookback, length(d)))

	rowN = 0
	j = lookback
	while( j <= length(d) )
	{
		j <- j + slide_window
		rowN = rowN + 1
	}
	
	#j_index <- seq(lookback, length(d), by = slide_window)
	#rowN <- length(j_index)
	
	fff <- as.data.frame(matrix(nrow=rowN, ncol=1))

	row_cnt = 1
	
	start <- Sys.time()

	diff0_sum <- 0
	j = lookback
	
	length_d <- length(d)
	while( j <= length_d )
	#for ( j in lookback:length(d))
	{
		start0 <- Sys.time()

		#print(sprintf("%d:%d", (j-lookback+1),j))
	
		dd <- d[(j-lookback+1):j]
    
    	if ( i == maintenance_index )
    	{
    		f <- ifelse(length(dd[dd==1]) > 0,  data.frame(c(1),nrow=1), data.frame(c(0),nrow=1))
    		
    		#if ( length(dd[dd==1]) )
    		#{
			#	f <- data.frame(c(1),nrow=1)
    		#}else
    		#{
			#	f <- data.frame(c(0),nrow=1)
    		#}
    	}
    	if ( i == time_index )
    	{
    		f <- ifelse(exist_timeStamp, as.character(f[,1]), data.frame(c(dd[lookback]),nrow=1))
			
			#f <- data.frame(c(dd[lookback]),nrow=1)
			#if ( exist_timeStamp)
			#{ 
			#	f <- as.character(f[,1])
			#}
    	}else
    	{
    		mean = 0
    		mean <- ifelse(sampling, dd[sample(1:length(dd), size = 1)], mean(dd, na.rm = TRUE))
    		
    		#if ( sampling )
    		#{
			#	mean <- dd[sample(1:length(dd), size = 1)]
			#}else
			#{
			#	mean <- mean(dd, na.rm = TRUE)
			#}
			f <- data.frame(matrix(c(mean),nrow=1))
		}
		
		fff[row_cnt,] <- f
		row_cnt = row_cnt + 1
		

		end0 <- Sys.time()
		diff0 <- as.numeric(difftime(end0, start0, units = "sec"))

		diff0_sum <- diff0_sum + diff0

		if ( row_cnt %% 10000 == 0 )
		{
			cat(sprintf("reduce noise %s %d %d/%d %f%s", col_name, row_cnt, row_cnt, rowN, 100*row_cnt/rowN, "% "))

			#print(sprintf("Time:%f sec", diff0_sum))
			t <- (diff0_sum/(row_cnt-1))*(rowN-row_cnt)
			if ( 1 < t/ (60*60*24) )
			{
				cat(sprintf("Time to finish:%f day", t/(60*60*24)))
				t <- 0
			}
			if ( 1 < t/( 60*60) )
			{
				cat(sprintf("Time to finish:%f hour", t/(60*60)))
				t <- 0
			}
			if ( 1 < t / 60 )
			{
				cat(sprintf("Time to finish:%f min", t/60))
				t <- 0
			}
			if ( t > 0 )
			{
				cat(sprintf("Time to finish:%f sec", t))
			}
			cat("\n")
			flush.console()
		}
		
		j <- j + slide_window
	}
	end <- Sys.time()
	diff <- as.numeric(difftime(end, start, units = "sec"))

	print(sprintf("%s Time:%f", col_name, diff))
	print(head(fff))
	flush.console()
	
	colnames(fff) <- c(col_name)
	
	for ( i in 1:ncol(fff))
	{
		x <- fff[,i]
		if ( i != time_index && i != maintenance_index)
		{
			x[which(is.na(x))]<- mean(x, na.rm=TRUE)
			fff[,i] <- x
		}
	}

	if ( is.null(ff)) {
		ff <- fff
	}else {
		#ff <- cbind(ff, fff)
		ff <- dplyr::bind_cols(ff, fff)
	}
	print("moving_average_sub")
	print(head(fff))
	flush.console()
	
	return(ff)
}

moving_average0 <- function(sampling, df2, lookback=100, slide_window=100)
{
	set.seed(123)
	start <- Sys.time()

	colnames_df2 <- colnames(df2)

	
	ff <- NULL
	
	#for ( i in 1:ncol(df2))
	for ( i in seq(1, ncol(df2), by = 1))
	{
		col_name = colnames_df2[i]
		#print(sprintf("%d %s %d/%d", i, col_name, i, ncol(df2)))
		#flush.console()
		ff <- moving_average_sub0(sampling, colnames_df2[i], df2, ff, lookback=lookback, slide_window)
	}
	set.seed(NULL)

	if ( sampling )
	{
		print("sampling")
	}else
	{
		print("moving_average")
	}
	print(head(ff))
	
	colnames(ff) <- colnames_df2
	df3 <- as.data.frame(ff)

	end <- Sys.time()
	diff <- as.numeric(difftime(end, start, units = "sec"))

	if ( sampling )
	{
		print(sprintf("sampling Time:%f sec( %f min)( %f hour)", diff, diff/60, diff/(60*60)))
		print("-sampling-")
	}else
	{
		print(sprintf("moving_average Time:%f sec( %f min)( %f hour)", diff, diff/60, diff/(60*60)))
		print("-moving_average-")
	}
	print(head(df3))
	
	return (df3)
}

moving_average_sub <- function(sampling=TRUE, col_name, df2, ff = NULL, lookback=100, slide_window = 1, exist_timeStamp=T, time_index=0, maintenance_index=0)
{
	colnames_df2 <- colnames(df2)

	i <- which(col_name == colnames_df2)


	fff <- NULL

	d <- df2[,i]
	if ( i == maintenance_index )
	{
		d[is.na(d)] <- 0
	}
	if ( i == time_index )
	{
		d[is.na(d)] <- mean(d, na.rm = TRUE)
	}
	print("moving_average_sub")
	print(head(d))
	flush.console()
	
	#print(sprintf("lookback:%d length(d):%d", lookback, length(d)))

	j_index <- seq(lookback, length(d), by = slide_window)
	rowN <- length(j_index)
	
	fff <- as.data.frame(matrix(nrow=rowN, ncol=1))

	row_cnt = 1
	
	start <- Sys.time()

	diff0_sum <- 0
	
	length_d <- length(d)
	for ( j in j_index )
	{
		start0 <- Sys.time()
	
		dd <- d[(j-lookback+1):j]
    
    	f <- NULL
    	if ( i == maintenance_index )
    	{
    		f <- ifelse(length(dd[dd==1]) > 0,  data.frame(c(1),nrow=1), data.frame(c(0),nrow=1))
    	}
    	if ( i == time_index )
    	{
    		f <- ifelse(exist_timeStamp, as.character(dd[lookback]), data.frame(c(dd[lookback]),nrow=1))
    	}
    	if ( is.null(f))
    	{
    		mean <- ifelse(sampling, dd[sample(1:length(dd), size = 1)], mean(dd, na.rm = TRUE))
			f <- data.frame(matrix(c(mean),nrow=1))
		}
		
		fff[row_cnt,] <- f
		row_cnt = row_cnt + 1

		diff0 <- as.numeric(difftime(Sys.time(), start0, units = "sec"))

		diff0_sum <- diff0_sum + diff0

		if ( row_cnt %% 10000 == 0 )
		{
			cat(sprintf("reduce noise %s %d %d/%d %f%s", col_name, row_cnt, row_cnt, rowN, 100*row_cnt/rowN, "% "))

			#print(sprintf("Time:%f sec", diff0_sum))
			t <- (diff0_sum/(row_cnt-1))*(rowN-row_cnt)
			if ( 1 < t/ (60*60*24) )
			{
				cat(sprintf("Time to finish:%f day", t/(60*60*24)))
				t <- 0
			}
			if ( 1 < t/( 60*60) )
			{
				cat(sprintf("Time to finish:%f hour", t/(60*60)))
				t <- 0
			}
			if ( 1 < t / 60 )
			{
				cat(sprintf("Time to finish:%f min", t/60))
				t <- 0
			}
			if ( t > 0 )
			{
				cat(sprintf("Time to finish:%f sec", t))
			}
			cat("\n")
			flush.console()
		}
	}
	diff <- as.numeric(difftime(Sys.time(), start, units = "sec"))

	print(sprintf("%s Time:%f", col_name, diff))
	print(head(fff))
	flush.console()
	
	colnames(fff) <- c(col_name)
	
	for ( i in 1:ncol(fff))
	{
		x <- fff[,i]
		if ( i != time_index && i != maintenance_index)
		{
			x[which(is.na(x))]<- mean(x, na.rm=TRUE)
			fff[,i] <- x
		}
	}

	if ( is.null(ff)) {
		ff <- fff
	}else {
		ff <- dplyr::bind_cols(ff, fff)
	}
	print("moving_average_sub")
	print(head(fff))
	flush.console()
	
	return(ff)
}

moving_average <- function(sampling, df2, lookback=100, slide_window=100)
{
	set.seed(123)
	start <- Sys.time()

	colnames_df2 <- colnames(df2)

	exist_timeStamp = TRUE
	time_index = which(timeStamp == colnames_df2)
	if ( length(time_index) < 1 )
	{
		time_index = which("time_index" == colnames_df2)
		exist_timeStamp = FALSE
	}
	if ( length(time_index) < 1 )
	{
		print("Date and time columns are required.")
		print("Must be a specified date/time column name or \"time_index\".")
		quit()
	}
	
	maintenance_index = which("maintenance" == colnames_df2)
	if ( length(maintenance_index) < 1  )
	{
		maintenance_index = 0
	}

	
	ff <- NULL
	
	for ( i in seq(1, ncol(df2), by = 1))
	{
		ff <- moving_average_sub(sampling, colnames_df2[i], df2, ff, lookback=lookback, slide_window, exist_timeStamp, time_index, maintenance_index)
	}
	set.seed(NULL)

	if ( sampling )
	{
		print("sampling")
	}else
	{
		print("moving_average")
	}
	print(head(ff))
	
	colnames(ff) <- colnames_df2
	df3 <- as.data.frame(ff)

	end <- Sys.time()
	diff <- as.numeric(difftime(end, start, units = "sec"))

	if ( sampling )
	{
		print(sprintf("sampling Time:%f sec( %f min)( %f hour)", diff, diff/60, diff/(60*60)))
		print("-sampling-")
	}else
	{
		print(sprintf("moving_average Time:%f sec( %f min)( %f hour)", diff, diff/60, diff/(60*60)))
		print("-moving_average-")
	}
	print(head(df3))
	
	return (df3)
}

#the smoother span. 
#This gives the proportion of points in the plot which influence the smooth at each value. 
#Larger values give more smoothness.
smoother_span <- 0.05

smooth <- function(x, smooth_window = 10, smooth_window_slide = 1)
{
	#if ( nrow(x) < smooth_window+3 )
	#{
	#	return(x)
	#}
	colname <- colnames(x)
	time_index = which("time_index" == colname)
	maintenance_index = which("maintenance" == colname)
	if ( length(maintenance_index) < 1 )
	{
		maintenance_index = 0
	}
	#print("maintenance_index")
	#print(maintenance_index)
	
	print("---------------------")
	print("smooth start")
	print(sprintf("nrow(x):%d ncol(x):%d", nrow(x), ncol(x)))
	print(sprintf("smooth_window:%d", smooth_window))
	print(sprintf("smooth_window_slide:%d", smooth_window_slide))
	#print(colname)
	#print(str(x))
	df3 <- NULL
	for ( i in 1:ncol(x))
	{
		y <- x[,i]
		
		f <- NULL
		j = smooth_window
		if ( use_lowess )
		{
			if ( i == time_index )
			{
				z <- y
			}else
			{
				#print(sprintf("smoother_span:%f", smoother_span))
				#print(sprintf("length(x):%d", length(x[,time_index])))
				#print(x[,time_index])
				#print(sprintf("length(y):%d", length(y)))
				#print(y)
				
				#(default)f=0.75
				z <- lowess(x[,time_index], y, f = smoother_span)$y
				#lines(x[,time_index], z, col='red')
			}
	    	if ( i == maintenance_index )
	    	{
	    		if ( length(y[y==1]) > 0 )
	    		{
					z <- y
	    		}
	    	}
			
			f <- z
		}else
		{
			maintenance_flg = 0
			while ( (j - smooth_window+1) >= 1 && j <= nrow(x) )
			{
				z <- mean(y[(j - smooth_window+1):j], rm.na = T)
				if ( i == time_index )
				{
					z <- y[j]
				}
		    	if ( i == maintenance_index )
		    	{
		    		z <- y[(j - smooth_window+1):j]
		    		z[is.na(z)] <- 0

		    		if ( length(z[z==1]) > 0 )
		    		{
						z <- c(1)
						if ( maintenance_flg > max(5,smooth_window/4) )
						{
							z <- c(0)
						}
						maintenance_flg = maintenance_flg + 1
		    		}else
		    		{
						maintenance_flg = 0
		    			z <- c(0)
		    		}
		    	}
				
				if ( is.null(f))
				{
					f <- z
				}else
				{
					f <- c(f, z)
				}
				
				j <- j + smooth_window_slide
			}
		}
		if ( is.null(df3))
		{
			df3 <- f
		}else
		{
			df3 <- cbind(df3, f)
		}
	}
	#print("df3")

	if ( !is.null(df3))
	{
		df3 <- as.data.frame(df3)
		colnames(df3) <- colname
	}
	#print(str(df3))
	#print("colnames:df3")
	#print(str(df3))
	print("smooth end")
	return (df3)
	
	#x <- stats::filter(x, rep(1/smooth_window, smooth_window), sides = 1)
	#x <- as.data.frame(x)[smooth_window:nrow(x),]
	#x <- as.data.frame(x)
	#colnames(x) <- colname
	
	#return(x)
}


# unsupervised anomaly detection
anomaly_detection_train <- function( df )
{
	if ( length(which("maintenance" == colnames(df))) > 0 )
	{
		df[,"maintenance"] <- NULL
	}
	if ( length(which("time_index" == colnames(df))) > 0 )
	{
		df[,"time_index"] <- NULL
	}
	if ( length(which(timeStamp == colnames(df))) > 0 )
	{
		df[,timeStamp] <- NULL
	}

	df[is.na(df)] <- 0
	mu <- apply(df,2,mean)
	sd <- apply(df,2,sd)
	
	tmp <- df
	for ( i in 1:ncol(df))
	{
		tmp[,i] = (df[,i] - mu[i])/sd[i]
	}
	#apply(tmp,2,mean)
	#apply(tmp,2,sd)

	X <- as.matrix(tmp)

	X[is.na(X)] <- 0
	mu_ <- apply(X,2,mean)
	sd_ <- apply(X,2,sd)

	
	
	X[is.na(X)] <- 0
	X[is.nan(X)] <- 0
	X[is.infinite(X)] <- 0.00000001
	#print(X)
	
	Xc <- as.matrix(X) - matrix(1, nrow(X), 1) %*% mu_
	Sigma <- t(Xc) %*% Xc / nrow(X)
	
	tryCatch({
		invSigma <- solve(Sigma)
	},
	error = function(e) {
	    #message(e)
	    print(e)
	    print("Substitute in general inverse for singular matrix")
	},	
	finally   = {
		invSigma <- ginv(Sigma)
	},
	silent = TRUE
	)
	return (list(X, mu, sd, Sigma, invSigma))
}

anomaly_detection_test <- function( model, df, method="mahalanobis", threshold=0)
{
	if ( length(which("maintenance" == colnames(df))) > 0 )
	{
		df[,"maintenance"] <- NULL
	}
	if ( length(which("time_index" == colnames(df))) > 0 )
	{
		df[,"time_index"] <- NULL
	}
	if ( length(which(timeStamp == colnames(df))) > 0 )
	{
		df[,timeStamp] <- NULL
	}

	df[is.na(df)] <- 0
	mu = model[[2]]
	sd = model[[3]]
	
	tmp <- df
	for ( i in 1:ncol(df))
	{
		tmp[,i] = (df[,i] - mu[i])/sd[i]
	}

	X <- as.matrix(tmp)
	
	invSigma = model[[5]]
	
	X[is.na(X)] <- 0
	X[is.nan(X)] <- 0
	X[is.infinite(X)] <- 0.00000001

	mu_ <- apply(X,2,mean)
	sd_ <- apply(X,2,sd)
	
	am <- NULL
	if (method == "hotelling")
	{
		Xc <- as.matrix(X) - matrix(1, nrow(X), 1) %*% mu_
		tryCatch({
			am <- rowSums((Xc %*% invSigma * Xc))
		},
		error = function(e) {
		    #message(e)
		    print(e)
		},	
		finally   = {
		},
		silent = TRUE
		)
		if ( threshold == 0 ) threshold <- qchisq(0.99, 3)
	}else
	{
		Xc <- as.matrix(X) - matrix(1, nrow(X), 1) %*% mu_
		if ( threshold == 0 ) threshold <- 1
		tryCatch({
			am <- rowSums((Xc %*% invSigma * Xc)) / ncol(Xc)
		},
		error = function(e) {
		    #message(e)
		    print(e)
		},	
		finally   = {
		},
		silent = TRUE
		)
	}
	#print(am)
	
	#png("anomaly_detection_test.png", width = 640*3, height = 480)
	#plot(am)
	#segments(0, threshold, nrow(df), threshold, col='red', lty=3, lwd=3)
	#dev.off()
	
	return(list(X,am, threshold))
}



feature_names <- c( ".", "mean", "sd", "var", 
			#"q25", "q75", 
			"skewness", "kurtosis", "peak2peak", "RMS","range",
			"CrestFactor", "ShapeFactor", "ImpulseFactor", "MarginFactor"
			,"logEnergy","spectrum","spectral_mean", "spectral_sd", "spectral_kurtosis"
			,"spectral_skewness"
			)


feature_sub <- function(col_name, df2, ff = NULL, lookback=100, slide_window = 1)
{
	colnames_df2 <- colnames(df2)

	time_index = which("time_index" == colnames_df2)
	i <- which(col_name == colnames_df2)
	
	maintenance_index = which("maintenance" == colnames_df2)

	fff <- NULL
	d <- df2[,i]
	if ( length(maintenance_index) < 1 )
	{
		maintenance_index = 0
	}
	if ( i == maintenance_index )
	{
		d[is.na(d)] <- 0
	}
	
	d[is.na(d)] <- mean(d, na.rm = TRUE)
	
	print(sprintf("feature_sub lookback:%d slide_window:%d length(d):%d", lookback, slide_window, length(d)))
	#print(head(d))
	
	rowN = 0
	j = lookback
	while( j <= length(d) )
	{
		j <- j + slide_window
		rowN = rowN + 1
	}
	if ( i == time_index || i == maintenance_index)
	#if ( i == time_index )
	{
		fff <- as.data.frame(matrix(nrow=rowN, ncol=1))
	}else
	{
		fff <- as.data.frame(matrix(nrow=rowN, ncol=19))
	}
	row_cnt = 1
	
	
	j = lookback
	length_d <- length(d)
	start <- Sys.time()

	maintenance_flg = 0
	
	while( j <= length_d )
	#for ( j in lookback:length(d))
	{
		start0 <- Sys.time()
		#print(sprintf("%d:%d", (j-lookback+1),j))
	
		dd <- d[(j-lookback+1):j]
		y <- sigin*d[j-lookback+1]
		mean <- sigin*mean(dd, na.rm = TRUE)
		sd <- sigin*sd(dd, na.rm = TRUE)
		var <- sigin*var(dd, na.rm = TRUE)
		
		spectrum <- sigin*abs(fft(dd))
  		spectrum_mean <- mean(spectrum)
  		spectrum_std <- sd(spectrum)
  		spectral_skewness <- sigin*skewness(spectrum, type=2)
  		spectral_kurtosis <- sigin*kurtosis(spectrum, type=2)
		
		#q <- quantile(dd, c(0.25, 0.75), type=1, na.rm = TRUE)
		
		ske <- sigin*skewness(dd, type=2)
		kur <- sigin*kurtosis(dd, type=2)
		pe2p <- sigin*peak2peak(dd)
		
		dd2 = dd^2
		RMS = sigin*sqrt(mean(abs(dd)^2, na.rm = TRUE))
		
		abs_dd_mean = mean(abs(dd), na.rm = TRUE)
		max_dd = max(dd)
		range = max_dd - min(dd)
		
	    CrestFactor = max_dd/RMS;
	    ShapeFactor = RMS/abs_dd_mean;
	    ImpulseFactor = sigin*max_dd/abs_dd_mean;
	    MarginFactor = sigin*max_dd/abs_dd_mean^2;
	    logEnergy = sigin*log(sum(dd2)+1);
	    
    
    	if ( i == time_index || i == maintenance_index)
    	{
			f <- data.frame(matrix(c(dd[length(dd)]),nrow=1))
			#f <- data.frame(c(dd[lookback]),nrow=1)
			
			if ( i == maintenance_index )
			{
				f <- data.frame(matrix(c(0),nrow=1))
				if ( length(dd[dd==1]) > 0 )
				{
					f <- data.frame(matrix(c(1),nrow=1))
					if ( maintenance_flg > max(5,lookback/5) )
					{
						f <- data.frame(matrix(c(0),nrow=1))
					}
					maintenance_flg = maintenance_flg + 1
				}else
				{
					f <- data.frame(matrix(c(0),nrow=1))
					maintenance_flg = 0
				}
			}

    	}else
    	{
			f <- data.frame(matrix(c(y, mean,sd,var, 
							#q, 
							ske, kur, pe2p, RMS,range,
							CrestFactor, ShapeFactor, ImpulseFactor, MarginFactor
							, logEnergy,
							spectrum,
					  		spectrum_mean,
					  		spectrum_std,
					  		spectral_skewness,
					  		spectral_kurtosis
							),nrow=1))
		}
		fff[row_cnt,] <- f
		row_cnt = row_cnt + 1
		
		#fff <- rbind(fff, f)
		#print(sprintf("%d/%d %d nrow(fff):%d", i, ncol(df2), j, nrow(fff)))
		#flush.console()
		
		j <- j + slide_window

		end0 <- Sys.time()
		diff0 <- as.numeric(difftime(end0, start0, units = "sec"))

		if ( row_cnt %% 500 == 0 )
		{
			print(sprintf("feature_sub[%d]:%f sec( %f min)( %f hour)", row_cnt, diff0, diff0/60, diff0/(60*60)))
			flush.console()
		}
	}
	end <- Sys.time()
	diff <- as.numeric(difftime(end, start, units = "sec"))

	
	if ( i == time_index || i == maintenance_index)
	{
		colnames(fff) <- c("time_index")
		if ( i == maintenance_index )
		{
			colnames(fff) <- c("maintenance")
		}
	}else
	{
		colnames(fff) <- paste(colnames(df2)[i], feature_names, sep=".")
	}

	print(sprintf("feature_sub:%f sec( %f min)( %f hour)", diff, diff/60, diff/(60*60)))
	flush.console()
	
	for ( i in 1:ncol(fff))
	{
		x <- fff[,i]
		x[which(is.na(x))]<- mean(x, na.rm=TRUE)
		fff[,i] <- x
	}
	#print(colnames(fff))
	#print("-------------------------")
	#print("")
	#flush.console()

	if ( is.null(ff)) {
		ff <- fff
	}else {
		ff <- cbind(ff, fff)
	}
	
	return(ff)
}

feature <- function(df2, lookback=100, slide_window=1)
{
	start <- Sys.time()

	colnames_df2 <- colnames(df2)

	time_index = which("time_index" == colnames_df2)
	maintenance_index = which("maintenance" == colnames_df2)
	if ( length(maintenance_index) < 1 )
	{
		maintenance_index = 0
	}
	#print("maintenance_index")
	#print(maintenance_index)
	#print(colnames_df2)
	
	ff <- NULL
	for ( i in 1:ncol(df2))
	{
		col_name = colnames_df2[i]
		print(sprintf("%d %s", i, col_name))
		ff <- feature_sub(colnames_df2[i], df2, ff, lookback=lookback, slide_window)
	}
	#for ( i in 1:ncol(ff))
	#{
	#	if ( colnames(df)[i]!=timeStamp )
	#	{
	#		x <- ff[,i]
	#		x[which(is.na(x))]<- mean(x, na.rm=TRUE)
	#		ff[,i] <- x
	#	}
	#}

	df3 <- NULL
	colnames_ff <- colnames(ff)
	#print("///////////////////////////")
	#print(colnames_ff)
	#print(head(ff))
	flush.console()
	
	for ( i in 1:ncol(ff) )
	{
		if ( colnames_ff[i]!= "maintenance" && colnames_ff[i]!= "time_index" && length( which(colnames_ff[i]==paste(colnames(df2),"..",sep=""))) == 0 )
		{
			#print(colnames_ff[i])
			next
		}
		if ( is.null(df3))
		{
			df3 <- ff[,i]
		}else
		{
			df3 <- cbind(df3, ff[,i])
		}
	}
	
	
	colnames(df3) <- colnames(df2)
	df3 <- as.data.frame(df3)
	#cat("df3")
	#print(str(df3))
	#flush.console()
	
	mahalanobis1 <- anomaly_detection_test(m_mahalanobis, df3)
	
	ff <- cbind(ff, data.frame(mahalanobis=mahalanobis1[[2]]))
	
	end <- Sys.time()
	diff <- as.numeric(difftime(end, start, units = "sec"))

	print(sprintf("feature_sub:%f sec( %f min)( %f hour)", diff, diff/60, diff/(60*60)))
	flush.console()

	return (ff)
}

monotonicity <- function(x, monotonicity_num = 20, eps = 0.0)
{
	#print("monotonicity_num")
	#print(monotonicity_num)
	
	mm = monotonicity_num
	if ( monotonicity_num < 0 )
	{
		mm = length(x)
	}
	x <- x[(length(x)-mm+1):length(x)]
	n = length(x)
	dx<- diff(x)
	m = (length(dx[dx > eps]) - length(dx[dx < -eps]))/(n - 1)
	
	return(m)
}

library(mgcv)
monotonicity2 <- function(x, monotonicity_num = 20, eps = 0.0)
{

	mm = monotonicity_num
	if ( monotonicity_num < 0 )
	{
		mm = length(x)
	}
	Y <- x[(length(x)-mm+1):length(x)]
	X <- c(1:length(Y))
	sp <- gam(Y~s(X), data=data.frame(X=X, Y=Y))
	ypred <- predict(sp,as.data.frame(X))
	
	n = length(ypred)
	dx<- diff(ypred)
	m = (length(dx[dx > eps]) - length(dx[dx < -eps]))/(n - 1)
	
	return(m)
}

feature_monotonicity <- function(feature_df, monotonicity_num = 20)
{
	time_index = which("time_index" == colnames(feature_df))
	maintenance_flag_idx = which("maintenance" == colnames(feature_df))
	if ( length(maintenance_flag_idx) < 1 )
	{
		maintenance_flag_idx <- 0
	}

	ff <- c(1:ncol(feature_df))
	for ( i in 1:ncol(feature_df))
	{
		if ( i == time_index || i == maintenance_flag_idx)
		{
			ff[i] = 0
		}else
		{
			if ( use_spline )
			{
				ff[i] = monotonicity2(feature_df[,i], monotonicity_num=monotonicity_num)
			}else
			{
				ff[i] = monotonicity(feature_df[,i], monotonicity_num=monotonicity_num)
			}
		}
	}
	ff <- data.frame(matrix(ff,nrow=1))

	colnames(ff) <- colnames(feature_df)
	
	return(ff)
}

gfm2_get_train_data <- function(gfm2)
{
	y = gfm2$y
	
	len <- length(y)
	if ( length(y) > abs(train_num) )
	{
		gfm2 = gfm2[(length(y)-abs(train_num)):length(y),]
	}
	
	return(gfm2)
}

predict_forecast <- function(gfm2, h=600, rank="", train_num = 20, feature_smooth_window=2)
{
	y <- gfm2_get_train_data(gfm2)$y
	print("gfm2_get_train_data(gfm2)$y")
	#print(y)
	#print(length(y))
	xt <- ts(as.numeric(y), frequency = 1)
	
	model = NULL

	threshold = get_threshold(rank)
	ymax = get_ymax(rank)
	
	if ( use_plophet )
	{
		
		library(prophet)
		
		ds <- seq(as.POSIXlt("2018-03-07"), by = "day", length.out = length(y))
		
		# prophet form
		pf = data.frame(ds = ds, y = y)
		pf$cap <- threshold*2.0
		pf$floor <- 0
		
		seasonality = "multiplicative"
		#seasonality = "additive"
		
		growth = "logistic"
		#growth="linear"	
		
		model <- prophet(
			pf,
            growth = growth,
            changepoints = NULL,
            n.changepoints = 10,#25,

            #時系列データの前から##%のデータを用いて、潜在的な変化点を推測
            changepoint.range = 0.8,

			#スパースな事前分布の強さを調整(過敏なトレンドに反応する場合は小さくする）
            changepoint.prior.scale = 0.02, #0.05,

            yearly.seasonality = FALSE, #'auto',
            weekly.seasonality = FALSE, #'auto',
            daily.seasonality = FALSE, 	#'auto',
            holidays = NULL,
            seasonality.mode = seasonality,
            seasonality.prior.scale = 10,
            holidays.prior.scale = 10,
            mcmc.samples = 0,
            interval.width = 0.80,
            uncertainty.samples = 1000,
			#fit=FALSE
		)
		#model <-fit.prophet(model, pf)

		future = make_future_dataframe(model,h, freq ="day")
		future$cap <- threshold*1.25
		future$floor <- 0

		forecast = predict(model, future, growth = growth)
		
		
		#offst =  pf[,"y"][length(pf$y)] - forecast[,"yhat"][length(pf$y)]

		#forecast[,"yhat"] = forecast[,"yhat"] + offst
		#forecast[,"yhat_lower"] = forecast[,"yhat_lower"] +  offst
		#forecast[,"yhat_upper"] = forecast[,"yhat_upper"] +  offst
		#plot(model, forecast)
	}
	if ( use_auto_arima )
	{
		model <- auto.arima(xt,ic="aic",trace=T,stepwise=T, 
						approximation=F,allowmean=F,allowdrift=F, 
						parallel=F, num.cores = 1,
						max.p=3,max.q=3,max.order=5,max.d=2,max.D=1,
						start.p = 2,start.q = 2,start.P = 1,start.Q = 1,seasonal=F)
						
	}
	if (use_arima )
	{
		tryCatch(
			{ 
				model <- arima(xt, order=c(min(15,abs(train_num)-1),0,1))
			} , error = function(e) {
				model = NULL
			}
		)		
	}
	if ( use_ets || is.null(model))
	{
		model <- ets(xt, ic='aic', damped=T, allow.multiplicative.trend=T)					
	}
	#print("model")
	#print(model)
	
	predict_ <- NULL
	if ( use_plophet )
	{
		#predict <- data.frame(forecast=forecast$trend, h95=forecast$yhat_upper, l95=forecast$yhat_lower)
		predict <- data.frame(forecast=forecast$yhat, h95=forecast$yhat_upper, l95=forecast$yhat_lower)
		predict <- predict[(nrow(predict)-h+1):nrow(predict),]
	}else{
		predict_ <- forecast(model, level = c(25,50,75,95),h = h)
		#plot(predict_)
		
		predict <- as.data.frame(predict_)
		colnames(predict) <- c("forecast", "l25","h25","l50","h50","l75","h75","l95","h95")
	}

	dif <- diff(gfm2$time_index)
	step <- mean(dif)
	x <- seq(gfm2$time_index[length(gfm2$time_index)]+step, length.out = nrow(predict), by = step)
	predict <- data.frame(time_index=x, predict)
	#colnames(predict)[1] <-"time_index"
	#print(predict)
	
	return(predict)
}

predict_plot <- function(predict, rank="")
{
	threshold = get_threshold(rank)
	ymax = get_ymax(rank)

	if ( use_plophet )
	{
		plt <- ggplot(predict) +
		  geom_line(mapping = aes_string(x = "time_index", y = "forecast")) +
		  geom_line(mapping = aes_string(x = 'time_index', y = 'forecast'), colour='blue') +
		  geom_ribbon(fill="#6875B1", mapping = aes_string(x = 'time_index', ymin = 'l95', ymax = 'h95'), alpha = 0.15)+
		theme(legend.position = "none")
	}else{
		plt <- ggplot(predict) +
		  geom_line(mapping = aes_string(x = "time_index", y = "forecast")) +
		  geom_line(mapping = aes_string(x = 'time_index', y = 'forecast'), colour='blue') +
		  geom_ribbon(fill="#6875B1", mapping = aes_string(x = 'time_index', ymin = 'l95', ymax = 'h95'), alpha = 0.15)+
		  geom_ribbon(fill="#A0A5C7", mapping = aes_string(x = 'time_index', ymin = 'l75', ymax = 'h75'), alpha = 0.15)+
		  geom_ribbon(fill="#CBCCD9", mapping = aes_string(x = 'time_index', ymin = 'l50', ymax = 'h50'), alpha = 0.15)+
		  geom_ribbon(fill="#E0E0E1", mapping = aes_string(x = 'time_index', ymin = 'l25', ymax = 'h25'), alpha = 0.15)+
		theme(legend.position = "none")
	}
	plt <- plt + geom_hline(aes(yintercept=threshold, linetype = "twodash",color = "red"))

	
	return(plt)
}

rlnorm2 <- function(n, mean=1, sd=1) {
  sdlog <- sqrt(log((sd/mean)^2 + 1))
  meanlog <- log(mean) - (sdlog^2) / 2
  rlnorm(n, meanlog = meanlog, sdlog = sdlog)
}
#data <- rlnorm2(10000, mean=5, sd=2)
#hist(data)


fit_tray_count <- 0
fit_success <- 0
curve_fitting <- function(y, h, reference=NULL, rank="")
{
	threshold = get_threshold(rank)
	ymax0 = get_ymax(rank)
	ymin0 = get_ymin(rank)
	
	ymax = max(y)
	ymin = min(y)
	
	residual_error = NULL
	fit_mode = ""
	fited <- NULL
	fit_pred <- NULL
	
	down = 0
	for ( i in length(y):2)
	{
		if ( y[i] < y[i-1] )
		{
			down = down + 1
		}else
		{
			break
		}
	}
	
	null_model1 <- FALSE
	null_model2 <- FALSE
		
	if ( TRUE )
	{
		x = c(1:length(y))
	
		lm.fit <- lm(y ~ c(1:length(y)))
		#print(lm.fit)
		
		grad = 0
		fit_pred <- NULL
		if ( !is.na(lm.fit$coefficients[2]) ) 
		{
			fit_pred = predict(lm.fit, x=data.frame(x=c(1:h)))
			coef = coefficients(lm.fit)
			grad = coef[2]
		}else
		{
			lm.fit = NULL
		}

		
		if ( !is.null(lm.fit) && grad <= 0.0001/abs(train_num) )
		{
			#newx <- data.frame(x=x)
			#conf.interval <- predict(lm.fit, newdata = newx, interval = 'confidence', level = 0.95)
			
			#plot(x, y, xlab = 'Girth', ylab = 'Height')
			#abline(lm.fit)
			#lines(newx$x, conf.interval[, 1], col = 'orange')
			#lines(newx$x, conf.interval[, 2], col = 'darkgreen')
			#lines(newx$x, conf.interval[, 3], col = 'darkgreen')
			
			print(sprintf("x grad:%f", grad))
			#return( NULL)
		}
		
							
		
		print(sprintf("o grad:%f", grad))
		
		fit_tray_count <<- fit_tray_count + 1
		
		control = nls.control()
		tols = c( 0.00001, 0.0001, 0.001, 0.01)
   		fit = NULL
   		
   		lockback_max = as.integer(abs(train_num))
   		lockback_min = max(lockback_max/2, 3)
   		if ( lockback_max < lockback_min )
   		{
   			print(sprintf("lockback_max < lockback_min"))
   			#return ( NULL )
   		}
    	print(sprintf("lockback_max:%d  lockback_min:%d",lockback_max,lockback_min))
  		
   		xx <- NULL
   		yy <- NULL
		

		a_coef = c()
		b_coef = c()
		c_coef = c()
		d_coef = c()
		noise_varience = NULL
		
		sampling_num <<- min(0, sampling_num)
		err_min <- 999999.0
		err_min_stpnt <- 999999.0
		best_fit <- NULL
		#for ( kk in lockback_max:lockback_min)
		for ( kk in lockback_max:lockback_max)
		{
	   		if ( lockback_max < lockback_min )
	   		{
	   			print(sprintf("lockback_max < lockback_min"))
	    		print(sprintf("lockback_max:%d  lockback_min:%d",lockback_max,lockback_min))
	   			break
	   		}
			
	   		#if ( length(y) <= kk ) break
			xx = c((lockback_max-kk+1):length(y))
			#xx = log(xx)/log(h)
			xx = c(1:length(xx))/(length(xx) + h)

			yy = y[(lockback_max-kk+1):length(y)]
			
						
			fit_prm_e = 0
			fit_prm_sd = 0
			
			fit_prm_th = 0
			fit_prm_be = 0

			a_ <- rlnorm2(1, mean = 1, sd = 1000)
			b_ <- rnorm(n =1,mean = 1, sd = 10)

			sd <- sd(yy, na.rm = T)
			e <- 0.1*mean(rnorm(n =length(yy), mean = 0, sd = sd), na.rm = T)

			xx_org = xx
			yy_org = yy
			use_nls.lm = TRUE
			
			loopMax1 = 100
			for ( k in 3:length(tols))
			{
				tryCatch({
					for ( kk in 1:loopMax1)
					{
						if ( sampling_num > 0 )
						{
							tmp_df <- data.frame(x=xx_org, y=yy_org)
							xx <- tmp_df$x
							yy <- tmp_df$y
							
							if (T)
							{
								if ( nrow(tmp_df) > sampling_num )
								{
									tmp_df1 <- tmp_df1[sample(nrow(tmp_df), sampling_num),]
									tmp_df1 <- tmp_df1[order(tmp_df1$x), ]
									xx <- tmp_df0$x
									yy <- tmp_df0$y
									yy <- lowess(xx, yy, f = 0.075)$y
								}
							}else
							{
								tmp_df0 <- tmp_df
								tmp_df1 <- tmp_df[1:(nrow(tmp_df)*0.6),]
								tmp_df2 <- tmp_df[(nrow(tmp_df1)+1):(nrow(tmp_df)),]
								if ( nrow(tmp_df1) > sampling_num && nrow(tmp_df2) > 3)
								{
									tmp_df1 <- tmp_df1[sample(nrow(tmp_df1), sampling_num),]
									tmp_df1 <- tmp_df1[order(tmp_df1$x), ]
									tmp_df0 <- rbind(tmp_df1, tmp_df2)
									#print("----------------------------------")
									#print(tail(tmp_df1))
									#print(head(tmp_df2))
									#print(sprintf("tmp_df:%d -> %d", nrow(tmp_df), nrow(tmp_df0)))
									
									xx <- tmp_df0$x
									yy <- tmp_df0$y
								}
							}
						}
					
						if ( use_nls.lm )
						{
							lm_pa <- runif(1,-0.001,1)
							lm_pb <- runif(1,-0.001,1)
							lm_pc <- yy[1] + runif(1,-0.0001,0.0001)
							lm_pd <- runif(1,-0.001,1)
							
							if ( length(a_coef) > 1 )
							{
								lm_pa <- rlnorm2(1, mean = mean(a_coef), sd = sd(a_coef))
								lm_pb <- rnorm(n =1,mean = mean(b_coef), sd = sd(b_coef))
								lm_pd <- rnorm(n =1,mean = 0, sd = sqrt(noise_varience)) - noise_varience/2
							}
							if ( kk > 50 )
							{
								lm_pa <- runif(1,-1,1)
								lm_pb <- runif(1,-1,1)
								lm_pc <- yy[1] + runif(1,-0.0001,0.0001)
								lm_pd <- runif(1,-1,1)
								if ( length(a_coef) > 1 && kk < 80)
								{
									lm_pa <- rlnorm2(1, mean = mean(a_coef), sd = sd(a_coef))
									lm_pb <- rnorm(n =1,mean = mean(b_coef), sd = sd(b_coef))
									lm_pd <- rnorm(n =1,mean = 0, sd = sqrt(noise_varience)) - noise_varience/2
								}
							}
							lm_pc = yy_org[1]- lm_pa*exp(lm_pb*xx_org[1] + lm_pd)

							pred <- function(parS, xx) parS$c + parS$a*exp(parS$b*xx + parS$d)
							resid <- function(p, observed, xx) observed - pred(p,xx)
							parStart <- list(a=lm_pa, b=lm_pb, c=lm_pc, d=lm_pd)
							fit <- nls.lm(par=parStart, fn=resid, observed=yy, xx=xx, 
										control=nls.lm.control(maxiter=1024,maxfev=1000,nprint=0))
						}else
						{
							fit <- nls(yy ~ c + a*exp(b*xx + e ),
								control=nls.control(tol=tols[k]),
								start=c(
									#a=fit_$par$a,
									#b=fit_$par$b,
									#c=fit_$par$c,
									#d=fit_$par$d
									a = runif(1,-0.001,1),
									b = runif(1,-0.001,1),
									c = runif(1,-0.001,1)
									),trace=F)
						}
						
						if ( !is.null(fit))
						{
							coef = coefficients(fit)
							if ( coef[1] < 0 || coef[2] < 0) next
							fit_pred <-  coef[3] + coef[1]*exp(coef[2]*xx_org + coef[4])
							
							#err <- sqrt(sum((yy_org[1:length(yy_org)] - fit_pred)^2)/length(yy_org))
							
							w <- c(1:length(yy_org))/length(yy_org)
							w <- ifelse( x < 0.001, 0.001, x)
							w <- log(w)
							w <- ( w - min(w))/(max(w) - min(w))
							err <- sqrt(sum(((yy_org[1:length(yy_org)] - fit_pred)*w)^2)/length(yy_org))
							
							if ( err_min_stpnt > abs(yy_org[1] - fit_pred[1]))
							{
								err_min_stpnt = abs(yy_org[1] - fit_pred[1])
							}
							if ( err_min > err )
							{
								best_fit <- fit
								err_min = err

								fit_prm_e = e
								if ( use_nls.lm )
								{
									fit_prm_e = coef[4]
								}
								fit_prm_sd = sd

								fit_prm_be = b_
								fit_prm_th = a_
							}
						}
					 }
					},
					error = function(e)
					{
						#print(e)
						fit = NULL
					},
					finally ={
					},
					silendt = T
				)
				#if ( !is.null(best_fit)) break
			}
			if ( !is.null(best_fit)) break
		}
		fit <- best_fit
		
		if ( err_min > (ymax - ymin)*0.025 && err_min_stpnt > (ymax - ymin)*0.025)
		{
			print(sprintf("err:%f (ymax - ymin)*0.025:%f  ymax:%f ymin:%f", err_min, (ymax - ymin)*0.025, ymax, ymin))
			null_model1 <- TRUE
		}
		
		if ( is.null(fit))
		{
			print("fit error")
			#newx <- data.frame(x=x)
			#conf.interval <- predict(lm.fit, newdata = newx, interval = 'confidence', level = 0.95)
			
			#plot(x, y, xlab = 'x', ylab = 'y')
			#abline(lm.fit)
			#lines(newx$x, conf.interval[, 1], col = 'orange')
			#lines(newx$x, conf.interval[, 2], col = 'darkgreen')
			#lines(newx$x, conf.interval[, 3], col = 'darkgreen')
		}else
		{
			xx = xx_org
			yy = yy_org
			coef = coefficients(fit)
			fit_pred <-  coef[3] + coef[1]*exp(coef[2]*xx + fit_prm_e)
			a_coef <- c(a_coef, coef[1])
			b_coef <- c(b_coef, coef[2])
			c_coef <- c(c_coef, coef[3])
			d_coef <- c(d_coef, fit_prm_e)
			
			noise_varience <- var(yy_org[1:length(yy_org)] - fit_pred)
			
			plot(xx, yy, xlab = 'time', ylab = 'org')
			par(new=T)
			plot(xx, fit_pred, type = "l", xlab = 'time', ylab = 'fit', col = 'orange')
		}
		
		if ( !is.null(fit))
		{
			fit_success <<- fit_success + 1

			print(fit)
			coef = coefficients(fit)
			
			if ( TRUE )
			{
				#if ( abs(coef[1]) < 0.0001 ) coef[1] = 0.0

				set_param(rank, coef[1], coef[2], coef[3], fit_prm_e )

				fit_pred <-  coef[3] + coef[1]*exp(coef[2]*xx + fit_prm_e)
				
				coef = coefficients(fit)
				if ( coef[1] < 0 || coef[2] < 0) next
				fit_pred <-  coef[3] + coef[1]*exp(coef[2]*xx_org + coef[4])
				
				err <- yy_org[1:length(yy_org)] - fit_pred
				if ( is.null(residual_error))
				{
					residual_error = c(err)
				}else
				{
					residual_error = c(residual_error, err)
				}
				er <- sqrt(sum(err^2)/length(yy_org))
				print(sprintf("er:%f", er))
				
				#x = c(0:(h-1))/length(xx) + 1
				#x = c((length(xx)+1):(length(xx)+h))/(length(xx) + h)
				x = c(1:(length(xx)+h))/(length(xx) + h)
				#x = log(x)/log(h)
				#x = xx/(length(y)+h)

				fit_pred <-  coef[3] + coef[1]*exp(coef[2]*x +fit_prm_e)
								
				fit_pred25 <- NULL
				fit_pred75 <- NULL
				fit_pred05 <- NULL
				fit_pred95 <- NULL
				tryCatch({
					loup <- confint(fit, level=0.5)
					fit_pred25 <- loup[3,1] + loup[1,1]*exp(loup[2,1]*x + loup[4,1])
					fit_pred75 <- loup[3,2] + loup[1,2]*exp(loup[2,2]*x + loup[4,2])
					loup <- confint(fit, level=0.9)
					#print(loup)
					fit_pred05 <- loup[3,1] + loup[1,1]*exp(loup[2,1]*x + loup[4,1])
					fit_pred95 <- loup[3,2] + loup[1,2]*exp(loup[2,2]*x + loup[4,2])
					
					#for ( jj in 1:3 )
					#{
					#	if ( loup[jj,1]/coef[jj] > 10 )
					#	{
					#		fit_pred <- NULL
					#		fit_pred25 <- NULL
					#		fit_pred75 <- NULL
					#		fit_pred05 <- NULL
					#		fit_pred95 <- NULL
					#	}
					#}
					if ( !is.null(fit_pred) && threshold > fit_pred[length(fit_pred)-1])
					{
						if (fit_pred[length(fit_pred)] /fit_pred[length(fit_pred)-1] > 0.25*(ymax0-ymin0))
						{
							print(sprintf("############ %f -> %f", fit_pred[length(fit_pred)-1],fit_pred[length(fit_pred)]))
							#fit_pred <- NULL
							#fit_pred25 <- NULL
							#fit_pred75 <- NULL
							#fit_pred05 <- NULL
							#fit_pred95 <- NULL
						}
					}
					
					#plot(x, fit_pred, type = "l", xlab = 'time', ylab = 'fit', col = 'orange')
					#par(new=T)
					#plot(x, fit_pred25, type = "l", xlab = 'time', ylab = 'fit', col = 'orange')
					#par(new=T)
					#plot(x, fit_pred75, type = "l", xlab = 'time', ylab = 'fit', col = 'orange')
					#par(new=T)
					#plot(x, fit_pred05, type = "l", xlab = 'time', ylab = 'fit', col = 'orange')
					#par(new=T)
					#plot(x, fit_pred95, type = "l", xlab = 'time', ylab = 'fit', col = 'orange')
					
				},error = function(e)
				{
						fit_pred25 <- NULL
						fit_pred75 <- NULL
						fit_pred05 <- NULL
						fit_pred95 <- NULL
						
						fit_pred <- NULL
					},
					finally ={
					},
					silendt = T
				)
			}
			err = 0
			if (!is.null(fit_pred))
			{
				if ( sum(is.na(fit_pred)) != 0 ) err = 1
				if ( sum(is.na(fit_pred25)) != 0 ) err = 1
				if ( sum(is.na(fit_pred75)) != 0 ) err = 1
				if ( sum(is.na(fit_pred05)) != 0 ) err = 1
				if ( sum(is.na(fit_pred95)) != 0 ) err = 1
			}else
			{
				err = 1
			}
			
			if ( err == 1 )
			{
				print("fit_pred## in NA")
			}
			if ( coef[1] < 0 )
			{
				#print(sprintf("coef[1]:%f < 0", coef[1]))
				#err = 1
			}
			#if ( er > 0.1 ) err = 1
			
			if ( !is.null(reference) )
			{
				dd = 0
				for ( kk in 1:length(reference))
				{
					dd = max(dd, abs(fit_pred[kk] - reference[kk]))
				}
				print(sprintf("fit reference dd:%f", dd))
				if ( dd > 0.5*(ymax-ymin) )
				{
					err = 1
				}
			}
			print(sprintf("fit er:%f", er))
			print(sprintf("fit err:%d", err))
			#print("fit")
			#print(err)
			#print(head(fit_pred))
			#print(head(fit_pred25))
			#print(head(fit_pred75))
			#print(head(fit_pred05))
			#print(head(fit_pred95))
			if ( err == 0 )
			{
				h1 = length(fit_pred25)
				h2 = length(fit_pred75)
				h3 = length(fit_pred05)
				h4 = length(fit_pred95)
				if ( h1 == 0 && h2 == 0 && h3 == 0 && h4 == 0 )
				{
					if (use_nls.lm)
					{
						fit_pred25 <- fit_pred
						fit_pred75 <- fit_pred
						fit_pred05 <- fit_pred
						fit_pred95 <- fit_pred
						h1 = length(fit_pred25)
						h2 = length(fit_pred75)
						h3 = length(fit_pred05)
						h4 = length(fit_pred95)
					}
				}
			}else
			{
				h1 = 0
				h2 = 0
				h3 = 0
				h4 = 0
			}
			if ( !(length(x) == h1 && length(x) == h2 && length(x) == h3 && length(x) == h4 )|| err == 1)
			{
				fit = NULL
				fit_pred25 <- NULL
				fit_pred75 <- NULL
				fit_pred05 <- NULL
				fit_pred95 <- NULL
				
				fit_pred <- NULL
			}else
			{
				fit_mode = "exp"
#				if ( null_model1 )
#				{
#					fit_mode = "exp_"
#				}
			}
		}
		
		#x = c(1:length(y))
		#plot(x=x, y=y, type="l", col=1)
		#par(new=T)
		#x = c((length(y)+1):(length(y)+h))
		#fit_pred <-  coef[3] + coef[1]*exp(offset_c**coef[2]*x + fit_prm_e )
		#plot(x=x, y=fit_pred, type="l", col=2)
		#fit_pred
		
		if ( !is.null(lm.fit) && is.null(fit) )
		{
			print(lm.fit)
			print("lm.fit -------------------------")
			coef = coefficients(lm.fit)
			#x = c((length(y)+1):(length(y)+h))
			x = c(1:(length(y)))
			
			fit_pred <- coef[2]*x + coef[1]
			err1 <- y[1:length(y)] - fit_pred
			if ( is.null(residual_error))
			{
				residual_error = c(err1)
			}else
			{
				residual_error = c(residual_error, err1)
			}

			x = c(1:(length(y)+h))
			fit_pred <- coef[2]*x + coef[1]

			tryCatch({
				
				loup <- confint(lm.fit, level=0.5)
				fit_pred25 <- loup[1,1] + loup[2,1]*x
				fit_pred75 <- loup[1,2] + loup[2,2]*x
				loup <- confint(lm.fit, level=0.9)
				#print(loup)
				fit_pred05 <- loup[1,1] + loup[2,1]*x
				fit_pred95 <- loup[1,2] + loup[2,2]*x
				
			
				#xx = c(1:length(y))
				#conf.interval <- predict(lm.fit, newdata = data.frame(x=xx), interval = 'confidence', level = 0.95)
				#lm2.fit <- lm(conf.interval[,2] ~ xx)
				#coef2 = coefficients(lm2.fit)

				#fit_pred05 <- coef2[2]*x + coef2[1]
				
				#lm2.fit <- lm(conf.interval[,3] ~ xx)
				#coef2 = coefficients(lm2.fit)
				#fit_pred95 <- coef2[2]*x + coef2[1]

				#conf.interval <- predict(lm.fit, newdata = data.frame(x=xx), interval = 'confidence', level = 0.5)
				#lm2.fit <- lm(conf.interval[,2] ~ xx)
				#coef2 = coefficients(lm2.fit)
				#fit_pred25 <- coef2[2]*x + coef2[1]
				
				#lm2.fit <- lm(conf.interval[,3] ~ xx)
				#coef2 = coefficients(lm2.fit)
				#fit_pred75 <- coef2[2]*x + coef2[1]
			},error = function(e)
				{
					print(e)
					fit_pred25 <- NULL
					fit_pred75 <- NULL
					fit_pred05 <- NULL
					fit_pred95 <- NULL
					
					fit_pred <- NULL
				},
				finally ={
				},
				silendt = T
			)

			err = 0
			if ( sum(is.na(fit_pred)) != 0 ) err = 1
			if ( sum(is.na(fit_pred25)) != 0 ) err = 1
			if ( sum(is.na(fit_pred75)) != 0 ) err = 1
			if ( sum(is.na(fit_pred05)) != 0 ) err = 1
			if ( sum(is.na(fit_pred95)) != 0 ) err = 1


			if ( err == 0 && abs(err1[1]) > (ymax - ymin)*0.05 )
			{
				print(sprintf("abs(err1[1]):%f (ymax - ymin)*0.05:%f  ymax:%f ymin:%f", abs(err1[1]), (ymax - ymin)*0.05, ymax, ymin))
				null_model2 = TRUE
			}
			print("lm.fit")
			if ( err == 1 )
			{
				print("lm.fit fit_pred## in NA")
			}
			#if ( coef[2] < 0 )
			#{
			#	print(sprintf("coef[1]:%f < 0", coef[2]))
			#	err = 1
			#}
			
			print(sprintf("lm.fit err:%d", err))
			#print(length(fit_pred))
			#print(length(fit_pred25))
			#print(length(fit_pred75))
			#print(length(fit_pred05))
			#print(length(fit_pred95))
			h1 = length(fit_pred25)
			h2 = length(fit_pred75)
			h3 = length(fit_pred05)
			h4 = length(fit_pred95)
			if ( !(length(x) == h1 && length(x) == h2 && length(x) == h3 && length(x) == h4 ) || err == 1)
			{
				print("im.fit error ------------------------")
				lm.fit = NULL
				fit_pred25 <- NULL
				fit_pred75 <- NULL
				fit_pred05 <- NULL
				fit_pred95 <- NULL
				
				fit_pred <- NULL
			}else
			{
#				fit_mode = "lm"
#				if ( null_model2 )
#				{
#					fit_mode = "lm_"
#				}
			}
		}
		if ( !is.null(fit_pred) && length(x) == length(fit_pred))
		{
			fit_pred <- data.frame(time_index=x, y=c(fit_pred),
						 l25=c(fit_pred25),
						 u75=c(fit_pred75),
						 l05=c(fit_pred05),
						 u95=c(fit_pred95)
						 )
		}else
		{
			fit_pred <- NULL
			fit_mode = ""
		}
	}
	
#	if ( down >= 4 )
#	{
#		if ( null_model1 || fit_mode == "exp")
#		{
#			fit_mode = "exp_"
#		}
#		if ( null_model2 || fit_mode == "lm")
#		{
#			fit_mode = "lm_"
#		}
#	}
	
	print(sprintf("%d/%d %.3f", fit_success, fit_tray_count, fit_success/fit_tray_count))
	return( list(fit_pred, fit_mode, residual_error))
}

plot_feature <- function(feature_df, rank="")
{
	threshold = get_threshold(rank)
	ymax = get_ymax(rank)
	
	colname = rank
	id <- which(colname == colnames(feature_df))
	gfm2 <- data.frame(time_index=feature_df$time_index, y=feature_df[,id])
	plt2 <- gfm2 %>% ggplot(aes(x = time_index, y = y)) + geom_line(color=y, linewidth =1.0)+ geom_point(size =1.0)
		#scale_y_continuous(limits = c(0, ymax))+
		labs(title=colname)+
		geom_hline(aes(yintercept=threshold, linetype = "twodash",color = "red"))
		
	return(plt2)
}

library(outliers)
fit_id = 1
gyap_ratio = 0.075
break_index_df <- NULL
plot_plot_feature_predict <- function(feature_df_, train_num = 20, rank="", h=600, feature_smooth_window=2)
{
	threshold = get_threshold(rank)
	ymax = get_ymax(rank)
	ymin = get_ymin(rank)

	feature_df <- feature_df_[[1]]
	colname = rank
	id <- which(colname == colnames(feature_df))
	
	gfm2 <- data.frame(time_index=feature_df$time_index, y=feature_df[,id])
	
#/////////////////////////////////////////////////////
	maintenance_flag_idx = which("maintenance" == colnames(feature_df))
	if ( length(maintenance_flag_idx) < 1 )
	{
		maintenance_flag_idx <- 0
	}
	
	if ( maintenance_flag_idx > 0 )
	{
		gfm2 <- data.frame(time_index=feature_df$time_index, y=feature_df[,id], maintenance=feature_df$maintenance)
	}
#/////////////////////////////////////////////////////
	
	#if ( use_spline )
	#{
	#	sp <- gam(y~s(time_index), data=gfm2)
	#	ypred <- predict(sp,data.frame(x=gfm2$time_index))
	#	gfm2$y <- ypred
	#}

	#print("gfm2")
	#print(str(gfm2))
	#print("rank")
	#print(rank)
	
	break_index = NULL

	
	ymax_cur = max(gfm2$y)
	ymin_cur = min(gfm2$y)
	ymax = ymax_cur
	ymin = ymin_cur
	
	down_cnt = 0
	down_max_id = 0
	down_delta_sum = 0
	delta_mean <- abs(mean(diff(gfm2$y)))
	delta_sd <- abs(sd(diff(gfm2$y)))
	for ( i in 2:nrow(gfm2))
	{
		delta = gfm2$y[i] - gfm2$y[i-1]
		if ( delta < 0.0 )
		{
			down_cnt = down_cnt + 1
			down_delta_sum = down_delta_sum + abs(delta)
			down_max_id = i
		}else
		{
			#print(sprintf("%d %s down_delta_sum:%f ymax_cur:%f ymin:%f %f", down_cnt, rank, down_delta_sum, ymax_cur, ymin_cur,gyap_ratio*abs(ymax_cur - ymin_cur)))
			if ( down_delta_sum > gyap_ratio*abs(ymax_cur - ymin_cur) && down_cnt >= 1)
			{
				break_index <- c(break_index, gfm2$time_index[down_max_id])
			}
			down_cnt = 0
			down_delta_sum = 0
			next
		}
	}

#////////////////////////////////////
	if ( !is.null(break_index))
	{
		up_cnt = 0
		up_max_id = 0
		up_delta_sum = 0
		delta_mean <- abs(mean(diff(gfm2$y)))
		delta_sd <- abs(sd(diff(gfm2$y)))
		for ( i in 2:nrow(gfm2))
		{
			delta = gfm2$y[i] - gfm2$y[i-1]
			if ( delta > 0.0 )
			{
				up_cnt = up_cnt + 1
				up_delta_sum = up_delta_sum + abs(delta)
				up_max_id = i
			}else
			{
				#print(sprintf("%d %sup_delta_sum:%f ymax_cur:%f ymin:%f %f", up_cnt, rank, up_delta_sum, ymax_cur, ymin_cur,gyap_ratio*abs(ymax_cur - ymin_cur)))
				if ( up_delta_sum > gyap_ratio*abs(ymax_cur - ymin_cur) && down_cnt >= 1)
				{
					break_index <- c(break_index, gfm2$time_index[up_max_id])
				}
				up_cnt = 0
				up_delta_sum = 0
				next
			}
		}
	}
#////////////////////////////////////
	
	gfm2_org <- gfm2
	break_pos = 0
	if ( !is.null(break_index))
	{
		break_pos <- which(gfm2$time_index == max(break_index))
		if ( length(break_pos) == 0 )
		{
		  break_pos <- which.min(abs(gfm2$time_index - max(break_index))) 
		}
				
		if (break_pos < nrow(gfm2) )
		{
			x <- gfm2[break_pos:nrow(gfm2),]
			gfm2 <- x
		}else
		{
			print(sprintf("break_pos:%d >= nrow(gfm2) :%d", break_pos,nrow(gfm2) ))
			return(NULL)
		}
	}
	if ( break_pos > 0 )
	{
		break_index_df1 <- data.frame( key=rep(rank, length(break_index)), break_pos = c(break_index))
		if ( is.null(break_index_df))
		{
			break_index_df <<- break_index_df1
		}else
		{
			break_index_df <<- rbind(break_index_df, break_index_df1)
		}
		break_index_df <<- unique(break_index_df)
		write.csv(break_index_df, "./break_index_df.csv", row.names=F)
	}
	
#/////////////////////////////////////////////////////
if(T)
{
	if ( maintenance_flag_idx > 0 )
	{
		#print("@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@")
		#print(maintenance_flag_idx)
	
		maintenance_posidx = which( gfm2$maintenance == 1 )
		if ( length(maintenance_posidx) < 1)
		{
			maintenance_posidx = NULL
		}else
		{
			#print(maintenance_posidx)
		
			kk = 0
			for ( k in length(gfm2$maintenance):1)
			{
				kk = k
				if ( gfm2$maintenance[k] == 0 ) next
				break
			}
			kkk = kk
			for ( k in kkk:1)
			{
				kk = k
				if ( gfm2$maintenance[k] == 1 ) next
				break
			}
			maintenance_break_pos = max(1,kk-1)
			#cat("maintenance_break_pos")
			#print(maintenance_break_pos)
			#cat("nrow(gfm2)")
			#print(nrow(gfm2))
			#quit()
				
			if (maintenance_break_pos < nrow(gfm2) )
			{
				x <- gfm2[maintenance_break_pos:nrow(gfm2),]
				gfm2 <- x
			}
		}
	}
}
#/////////////////////////////////////////////////////
	
	if ( nrow(gfm2) < 3 )
	{
		return(NULL)
	}

	pred <- predict_forecast(gfm2, h=h, train_num= abs(train_num), rank=rank, feature_smooth_window=feature_smooth_window)
	plt1 <- predict_plot(pred, rank=rank)
	
	print(plt1)
	curv_fit = T
	
	fit_pred <- NULL
	if ( curv_fit )
	{
		#y <- gfm2_get_train_data(gfm2)
		#y <- gfm2$y
		
		yo <- gfm2
		if ( train_num > 0 )
		{
			yo <- gfm2_get_train_data(gfm2)
		}
		y <- yo$y
		
		if ( length(y)-train_num > 0 )
		{
			#yy <- y[(length(y)-train_num):length(y)]
			yy <- y
		}else
		{
			yy <- y
		}
		
		
		#fit_pred <- curve_fitting(yy, h, reference=c(pred$forecast[1:20]), rank)
		fit_pred <- curve_fitting(yy, h, reference=NULL, rank)

		#fit_pred return value -> ( list(fit_pred, fit_mode, residual_error))
	}
	
	fit_predict <- NULL
	fit_predict_org <- NULL
	if ( !is.null(fit_pred) && !is.null(fit_pred[[1]]))
	{
		fit_predict <- fit_pred[[1]] 
		timeidx = fit_predict$time_index
		residual_error <- fit_pred[[3]]

		#print("plot_plot_feature_predict->curve_fitting")
		#print(str(fit_predict))
		#print(str(gfm2))
		
		fit_predict$time_index[1:length(yo$time_index)] <- yo$time_index
		dif <- diff(yo$time_index)
		step <- mean(dif)
		fit_predict$time_index[(length(yo$time_index)+1):nrow(fit_predict)] <- seq(yo$time_index[length(yo$time_index)]+step,
		 length.out = nrow(fit_predict), by = step)
		
		tmp <- data.frame(time_index=fit_predict$time_index, y=fit_predict$y)
		plt_tmp <- tmp %>% ggplot(aes(x = time_index, y = y)) + geom_line(linewidth =1.0)+ 
			geom_point(data = gfm2, aes(x=time_index, y=y))
		ggsave(file = sprintf("%s%s%d.png", putpng_path, "debug/fit", fit_id), plot = plt_tmp, , dpi = 100, width = 12.5*1.2, height = 6.8*1.0)
		fit_id <<- fit_id + 1

		#plot(x=timeidx,y=fit_predict$y, type="l")
		#browser()

		fit_predict_org <- fit_predict
		fit_predict <- fit_predict[(nrow(fit_predict)-h+1):nrow(fit_predict),]
		#print("fit_predict")
		#print(str(fit_predict))
		#print("pred")
		#print(str(pred))
		#print("gfm2")
		#print(str(gfm2))
		
		a = 1.0
		if ( fit_pred[[2]] == "exp" )
		{
			a = 0.9
		}
		if ( fit_pred[[2]] == "lm" )
		{
			a = 0.9
		}
		#fit_predict = fit_pred[[1]]*a + (1.0 -a)*pred$forecast
		
		#fit_pred[[1]]$l25=fit_pred[[1]]$l25*a + (1.0 -a)*pred$l25
		#fit_pred[[1]]$u75=fit_pred[[1]]$u75*a + (1.0 -a)*pred$h25
		#fit_pred[[1]]$l05=fit_pred[[1]]$l05*a + (1.0 -a)*pred$l95
		#fit_pred[[1]]$u95=fit_pred[[1]]$u95*a + (1.0 -a)*pred$h95

		#fit_pred[[1]]$l25=fit_pred[[1]]$y*a + (1.0 -a)*(pred$forecast*0.9 + pred$l25*0.1)
		#fit_pred[[1]]$u75=fit_pred[[1]]$y*a + (1.0 -a)*(pred$forecast*0.9 + pred$h25*0.1)
		#fit_pred[[1]]$l05=fit_pred[[1]]$y*a + (1.0 -a)*(pred$forecast*0.9 + pred$l95*0.1)
		#fit_pred[[1]]$u95=fit_pred[[1]]$y*a + (1.0 -a)*(pred$forecast*0.9 + pred$h95*0.1)

		fit_predict$y=fit_predict$y*a + (1.0 -a)*pred$forecast

		if ( TRUE )
		{
			fit_predict$l25[1] = fit_predict$y[1]
			fit_predict$u75[1] = fit_predict$y[1]

			fit_predict$l05[1] = fit_predict$y[1]
			fit_predict$u95[1] = fit_predict$y[1]

			if ( FALSE )
			{
				for ( i in 2:length(fit_predict$y))
				{
					q =  qt((1-0.75)/2,i, lower.tail=F)
					fit_predict$l25[i] = fit_predict$y[i]- q*sd(fit_predict$y[1:i])*sqrt(1/i)
					fit_predict$u75[i] = fit_predict$y[i]+ q*sd(fit_predict$y[1:i])*sqrt(1/i)

					q =  qt((1-0.95)/2,i, lower.tail=F)
					fit_predict$l05[i] = fit_predict$y[i]- q*sd(fit_predict$y[1:i])*sqrt(1/i)
					fit_predict$u95[i] = fit_predict$y[i]+ q*sd(fit_predict$y[1:i])*sqrt(1/i)
				}
			}else
			{
				sd.fit <- NULL
				fd = min(3, length(fit_predict_org$y))
				
				sd_max <- 0.00001
				fit_predict_org$l25 = fit_predict_org$y
				fit_predict_org$u75 = fit_predict_org$y
				fit_predict_org$l05 = fit_predict_org$y
				fit_predict_org$u95 = fit_predict_org$y
				
				for ( i in 1:length(fit_predict_org$y))
				{
#					sd1 <- 1
#					if ( i >= length(fit_predict$y))
#					{
#						#sd1 <- sd(fit_predict_org$y[1:i])
#						sd1 = sd(residual_error)
#					}else
#					{
#						if ( i <= 3 )
#						{
#							#sd1 <- sd(fit_predict_org$y[1:3])
#							sd1 = sd(residual_error[1:3])
#						}else
#						{
#							#sd1 <- sd(fit_predict_org$y[1:i])
#							sd1 = sd(residual_error[1:min(i,length(residual_error))])
#						}
#						if ( sd1 > sd_max )
#						{
#							sd_max = sd1
#						}
#					}
#					sd1 <- sd_max
					
					fd <- i
					sd1 <- var(fit_predict_org$y[1:i])*(1+1/fd)
					if ( i >= lookback*3 )
					{
						fd <- lookback*3
						sd1 <- var(fit_predict_org$y[(i-lookback*3+1):i])*(1+1/fd)
					}
					
					q =  qt((1-0.75)/2, fd, lower.tail=F)
					dd = q*sqrt(sd1)
					fit_predict_org$l25[i] = fit_predict_org$y[i]- dd
					fit_predict_org$u75[i] = fit_predict_org$y[i]+ dd

					q =  qt((1-0.95)/2, fd, lower.tail=F)
					dd = q*sqrt(sd1)
					fit_predict_org$l05[i] = fit_predict_org$y[i]- dd
					fit_predict_org$u95[i] = fit_predict_org$y[i]+ dd
				}
				fit_predict <- fit_predict_org[(nrow(fit_predict_org)-h+1):nrow(fit_predict_org),]
			}
		}
		
			
		#fit_predict$time_index = timeidx
		#print("fit_predict")
		#print(str(fit_predict))
	}
	
	failure_time = failure_time_init
	failure_time50p = failure_time_init
	failure_time2 = failure_time_init
	
	
	for ( i in 1:h )
	{
		if ( !is.null(fit_predict))
		{
			if ( is.na(fit_predict$l05[i])) break
			if ( is.na(fit_predict$u95[i])) break
		
			#print(head(fit_predict))
			if (  (fit_predict$l05[i] > fit_predict$u95[i]) && fit_predict$l05[i] > threshold )
			{
				failure_time = i
				break
			}
			if (  (fit_predict$u95[i] > fit_predict$l05[i]) && fit_predict$u95[i] > threshold )
			{
				failure_time = i
				break
			}
			if (  fit_predict$y[i] > threshold )
			{
				failure_time = i
				break
			}
		}
	}
	
	if ( failure_time != failure_time_init && failure_time < h )
	{
		for ( i in (failure_time+1):h )
		{
			if ( !is.null(fit_predict))
			{
				if (  fit_predict$y[i] > threshold )
				{
					failure_time50p = i
					break
				}
			}
		}
	}
	
	if ( failure_time50p != failure_time_init && failure_time50p < h )
	{
		for ( i in (failure_time50p+1):h )
		{
			if ( !is.null(fit_predict))
			{
				#if ( is.na(fit_predict$l05[i])) break
				#if ( is.na(fit_predict$u95[i])) break

				#print(head(fit_predict))
				if ( !is.na(fit_predict$l05[i]) && fit_predict$l05[i] > threshold )
				{
					failure_time2 = i
					break
				}
				#if ( !is.na(fit_predict$u95[i]) && fit_predict$u95[i] > threshold )
				#{
				#	failure_time2 = i
				#	break
				#}
				#if ( !is.na(fit_predict$u75[i]) && fit_predict$u75[i] > threshold )
				#{
				#	failure_time2 = i
				#	break
				#}
				#if ( !is.na(fit_predict$l25[i]) && fit_predict$l25[i] > threshold )
				#{
				#	failure_time2 = i
				#	break
				#}
			}
		}
	}
	

	
	dt <- mean(diff(gfm2$time_index))
	print(sprintf("dt:%f", dt))
	print("===========================================================")
	print(sprintf("failure_time:%d  failure_time_init:%d", as.integer(failure_time), as.integer(failure_time_init)))
	print(sprintf("failure_time2:%d  failure_time_init:%d", as.integer(failure_time2), as.integer(failure_time_init)))
	print(sprintf("failure_time50p:%d  failure_time_init:%d", as.integer(failure_time50p), as.integer(failure_time_init)))
	print("===========================================================")

	failure_time_str = "+Infinity"
	failure_time50p_str = "+Infinity"
	if ( failure_time < failure_time_init && failure_time50p < failure_time_init )
	{
		failure_time_str = sprintf("%d step 5%%[%d %s]", as.integer(failure_time-1), 
				convert_time((failure_time-1)*dt, unit_of_record=unit_of_record,
				from=unit_of_time,to=forecast_time_unit), forecast_time_unit)
		failure_time50p_str = sprintf("50%%[%d %s]",  
				convert_time((failure_time50p-1)*dt, unit_of_record=unit_of_record,
				from=unit_of_time,to=forecast_time_unit), forecast_time_unit)
	}else
	{
		failure_time_str = sprintf(" > %d step 5%%[> %d %s]", as.integer(h),
			 convert_time(h*dt, unit_of_record=unit_of_record,
			 from=unit_of_time,to=forecast_time_unit), forecast_time_unit)
		failure_time50p_str = sprintf("50%%[> %d %s]", 
			 convert_time(h*dt, unit_of_record=unit_of_record,
			 from=unit_of_time,to=forecast_time_unit), forecast_time_unit)
	}
	
	

	if ( !is.null(feature_df_[[2]]) )
	{
		tmp <- feature_df_[[2]]
		gfm2_org <- data.frame(time_index=tmp$time_index, y=tmp[,id])
	}
	delta_index <- gfm2_org$time_index[nrow(gfm2)] - gfm2_org$time_index[nrow(gfm2)-1]
	gfm2_org[, timeStamp] <- current_time
	gfm2_org[,timeStamp] <- as.POSIXct(gfm2_org[,timeStamp], tz='UTC')
	x <- rev(seq(gfm2_org[,timeStamp][nrow(gfm2_org)], length.out = nrow(gfm2_org), by = -delta_time*delta_index))
	gfm2_org[,timeStamp] <- x
	
	plt2 <- ggplot()
	plt2 <- plt2 + geom_line(data=gfm2_org,aes(x = time_index, y = y),color="blue", linewidth =0.5)+ 
	geom_point(data=gfm2_org,aes(x = time_index, y = y),size =0.5)+
	geom_vline(xintercept = gfm2_org$time_index[nrow(gfm2_org)], linewidth =0.5)
	plt2 <- plt2 + annotate("rect", xmin = 1, xmax = gfm2_org$time_index[nrow(gfm2_org)], ymin = -Inf, ymax = Inf, alpha = .1,fill = "blue")
	
	#plt2 <- plt2 + geom_vline(xintercept = gfm2_org$time_index[max(break_index)], linewidth =0.5, color="red")
	if ( !is.null(break_index))
	{
		for ( i in 1:length(break_index))
		{
			plt2 <- plt2 + geom_vline(xintercept = gfm2_org$time_index[which.min(abs(gfm2_org$time_index - break_index[i]))], linewidth =0.5, color="red")
		}
	}
	#plt2 <- plt2 + geom_hline(aes(yintercept=ymin, linetype = "twodash"),color = "gray")
	#plt2 <- plt2 + geom_hline(aes(yintercept=ymax, linetype = "twodash"),color = "gray")

#/////////////////////////////////////////////////////
	maintenance_flag_idx = which("maintenance" == colnames(feature_df))
	if ( length(maintenance_flag_idx) < 1 )
	{
		maintenance_flag_idx <- 0
	}
	
	maintenance_flag = NULL
	if ( maintenance_flag_idx > 0 )
	{
		maintenance_flag = feature_df$maintenance

		maintenance_posidx = which( maintenance_flag == 1 )
		if ( length(maintenance_posidx)==0)
		{
			maintenance_posidx = NULL
		}
		
		maintenance_timeidx = NULL
		if ( !is.null(maintenance_posidx))
		{
			maintenance_timeidx = feature_df$time_index[maintenance_posidx]
		}
		if ( !is.null(maintenance_timeidx))
		{
			#print(maintenance_timeidx)
			for ( i in 1:length(maintenance_timeidx))
			{
				plt2 <- plt2 + geom_vline(data=maintenance_timeidx, xintercept = maintenance_timeidx[i], linewidth =1.5, color="green", alpha = 0.2)
			}
		}
	}
#/////////////////////////////////////////////////////


	print(plt2)
	fit_model = ""
	fill_col <- c("#ff8c00","#ffa500")
	col <- "#ff4500"

	if ( !is.null(fit_pred[[1]]))
	{
		fit_model <- fit_pred[[2]]

		print(sprintf("[%s]fit_model:%s",rank, fit_model))
		print(head(fit_predict$y,3))
		
		if ( fit_model == "x^2" )
		{
			fill_col <- c("#1e90ff","#87cefa")
			col <- "#008000"
		}
		if ( fit_model == "lm" )
		{
			fill_col <- c("#3cb371","#66cdaa")
			col <- "#ff00ff"
		}
		
		fit_predict <- fit_predict_org
		fit_predict$y <- ifelse(fit_predict$y > threshold+abs(ymax-ymin)*0.01, threshold+abs(ymax-ymin)*0.01, fit_predict$y)
		fit_predict$l05 <- ifelse(fit_predict$l05 > threshold+abs(ymax-ymin)*0.01,threshold+abs(ymax-ymin)*0.01,fit_predict$l05)
		fit_predict$u95 <- ifelse(fit_predict$u95 > threshold+abs(ymax-ymin)*0.01,threshold+abs(ymax-ymin)*0.01,fit_predict$u95)
		fit_predict$u75 <- ifelse(fit_predict$u75 > threshold+abs(ymax-ymin)*0.01,threshold+abs(ymax-ymin)*0.01,fit_predict$u75)
		fit_predict$l25 <- ifelse(fit_predict$l25 > threshold+abs(ymax-ymin)*0.01,threshold+abs(ymax-ymin)*0.01,fit_predict$l25)
		
		fit_predict$y <- ifelse(fit_predict$y < ymin-abs(ymin*0.01),ymin-abs(ymin*0.01),fit_predict$y)
		fit_predict$l05 <- ifelse(fit_predict$l05 < ymin-abs(ymin*0.01),ymin-abs(ymin*0.01),fit_predict$l05)
		fit_predict$u95 <- ifelse(fit_predict$u95 < ymin-abs(ymin*0.01),ymin-abs(ymin*0.01),fit_predict$u95)
		fit_predict$u75 <- ifelse(fit_predict$u75 < ymin-abs(ymin*0.01),ymin-abs(ymin*0.01),fit_predict$u75)
		fit_predict$l25 <- ifelse(fit_predict$l25 <  ymin-abs(ymin*0.01),ymin-abs(ymin*0.01),fit_predict$l25)

		fit_predict$time_index[1:length(yo$time_index)] <- yo$time_index
		dif <- diff(yo$time_index)
		step <- mean(dif)
		fit_predict$time_index[(length(yo$time_index)+1):nrow(fit_predict)] <- seq(yo$time_index[length(yo$time_index)]+step,
		 length.out = nrow(fit_predict), by = step)
		
		delta_index <- fit_predict$time_index[nrow(fit_predict)] - fit_predict$time_index[nrow(fit_predict)-1]
		fit_predict[, timeStamp] <- current_time
		
		fit_predict[,timeStamp] <- as.POSIXct(fit_predict[,timeStamp], tz='UTC')
		x <- rev(seq(fit_predict[,timeStamp][nrow(yo)], length.out = nrow(yo), by = -delta_time*delta_index))
		fit_predict[,timeStamp][1:nrow(yo)] <- x

		x <- seq(fit_predict[,timeStamp][nrow(yo)], length.out = length(nrow(yo):nrow(fit_predict)), by = delta_time*delta_index)
		fit_predict[nrow(yo):nrow(fit_predict),timeStamp] <- x
		
		fit_predict_tmp <- fit_predict
		if ( max_prediction_length_org != 0 && max_prediction_length > max_prediction_length_org)
		{
			fit_predict_tmp <- fit_predict[1:(nrow(gfm2_org)+max_prediction_length_org),]
		}
				
		if ( !(fit_model == "lm_" || fit_model == "exp_" || fit_model == ""))
		{
			plt2 <- plt2 +  geom_line(data=fit_predict_tmp,aes(x = time_index, y = y),color=col, linewidth =0.5)

			plt2 <- plt2 +
			geom_line(data=fit_predict_tmp, mapping = aes_string(x = "time_index", y = "y")) +
			geom_line(data=fit_predict_tmp, mapping = aes_string(x = 'time_index', y = 'y'), colour=col) +
			geom_ribbon(data=fit_predict_tmp, fill=fill_col[2], mapping = aes_string(x = 'time_index', ymin = 'l05', ymax = 'u95'), alpha = 0.3)+
			geom_ribbon(data=fit_predict_tmp, fill=fill_col[1], mapping = aes_string(x = 'time_index', ymin = 'l25', ymax = 'u75'), alpha = 0.3)
		}
		
#///////////////////////////////////////////////
		if ( F )
		{
		tmp <- data.frame(time_index=fit_predict_tmp$time_index, y=fit_predict_tmp$y, timeStamp=fit_predict_tmp[,timeStamp])
		colnames(tmp)[3] <- c(timeStamp)
		if ( maintenance_flag_idx > 0 )
		{
			gfm2_org$maintenance <- NULL
		}
		
		#print("============================")
		#print(str(tmp))
		#print(str(gfm2_org))
		tmp <- as.data.frame(dplyr::bind_rows(gfm2_org,tmp))

		#print(timeStamp)
		#print(colnames(tmp))
		#print(head(tmp))
		#print(tail(tmp))
		#print(delta_index)
		tmp2 <- NULL
		x <- tmp$time_index[1]
		xx <- 1
		
		#print("*****************************************")
		#print("tmp$time_index[nrow(tmp)] ")
		#print(tmp$time_index[nrow(tmp)] )
		#print("x")
		#print(x)
		#print("")
		if ( is.na(tmp$time_index[nrow(tmp)]))
		{
			print(tail(tmp$time_index))
			print(tail(tmp[timeStamp]))
		}
		
		endidx = 1
		dt <- NULL
		if ( is.na(tmp$time_index[nrow(tmp)]))
		{
			for ( i in 1:nrow(tmp) )
			{
				if ( !is.na(tmp$time_index[i]) )
				{
					endid = tmp$time_index[i]
					if ( i > 2 && is.null(dt))
					{
						#print(i)
						s <- tmp[timeStamp]
						#print(s[1,])
						#print(s[2,])
						#flush.console()

						s <- c(s[(i-2),],s[(i-1),])
						#print(s)
						#flush.console()
						s <- as.POSIXct(s, tz="UTC")
						dt <- difftime(s[1] , s[2])
						#print(dt)
					}
				}else
				{
					endidx = endidx + delta_index
					tmp$time_index[i] = endidx

					if ( i > 2 )
					{
						s <- tmp[timeStamp]
						s <- c(s[(i-2),],s[(i-1),])
						s <- as.POSIXct(s, tz="UTC")
						#print("========")
						s <- seq(s[1], length.out = 2, by = -dt)
						#print(s)
						tmp[i,timeStamp] <- s[2]
					}
				}
			}
		}
		
		for ( i in 1:nrow(tmp) )
		{
			x <- x + delta_index
			if ( tmp$time_index[nrow(tmp)] < x )
			{
				break
			}
			idx <- which.min(abs(tmp$time_index - x))
			if ( idx <= xx )
			{
				next
			}
			xx <- idx
			
			if ( is.null(tmp2))
			{
				tmp2 <- data.frame(tmp$time_index[idx], tmp[,timeStamp][idx])
			}else
			{
				tmp2 <- dplyr::bind_rows(tmp2, data.frame(tmp$time_index[idx], tmp[,timeStamp][idx]))
				tmp2 <- as.data.frame(tmp2)
			}
		}
		#print("$$$$$")
		#print(head(tmp))
		#print(head(tmp2))
		if ( !is.null(tmp2))
		{
			tmp <- NULL
			rm(tmp)
			tmp <- tmp2
		}else
		{
			tmp <- data.frame(time_index=tmp$time_index, timeStamp=tmp[,timeStamp])
		}
		colnames(tmp) <- c( "time_index", timeStamp)
		#print(head(tmp))
		#print(tail(tmp))

		step <- nrow(tmp)/5
		break_pos <- c()
		labels <- c()
		for ( i in 1:5 )
		{
			k = (i-1)*step+1
			if ( k > nrow(tmp))
			{
				k = nrow(tmp)
			}
			break_pos <- c(break_pos, tmp$time_index[(i-1)*step+1])
			labels <- c(labels, as.character(tmp[,timeStamp][(i-1)*step+1]))
		}
		tmp <- NULL
		rm(tmp)
		}
#///////////////////////////////////////////////
		
		plt2 <- plt2 + labs(x=sprintf("[%s] Current time:%s",timeStamp, current_time))
		#plt2 <- plt2 + scale_x_continuous(breaks = break_pos, labels = labels)
		#plt2 <- plt2 + theme(axis.text.x = element_text(angle = 0, vjust=0.0, hjust=0.5, face="bold")) 
		#plt2 <- plt2 + theme(axis.text.x = element_text(angle = 90, vjust=0.5, hjust=0.0, face="bold")) 
		
		
		if ( threshold < 0 )
		{
			#plt2 <- plt2 + scale_y_continuous(limits = c(threshold, ymax*(1.2)))
		}else
		{
			#plt2 <- plt2 + scale_y_continuous(limits = c(0, threshold*(1.2)))
			#plt2 <- plt2 + scale_y_continuous(limits = c(0, ymax*(1.2)))
		}
		#labs(title=sprintf("[%s] RUL:%s", colname, failure_time_str))+
		plt2 <- plt2 + theme(legend.position = "none")
		print(plt2)
	}
	
	ymax_cur = max(gfm2$y)
	ymin_cur = min(gfm2$y)
	
	if ( ymax <= ymax_cur) 
	{
		feature_param <<- set_ymax(colname, ymax_cur)
		#feature_param
	}
	if ( ymin >= ymin_cur) 
	{
		feature_param <<- set_ymin(colname, ymin_cur)
		#feature_param
	}
	
	pred$forecast <- ifelse(pred$forecast> threshold+abs(ymax-ymin)*0.01,threshold+abs(ymax-ymin)*0.01,pred$forecast)
	pred$l95 <- ifelse(pred$l95> threshold+abs(ymax-ymin)*0.01,threshold+abs(ymax-ymin)*0.01,pred$l95)
	pred$l75 <- ifelse(pred$l75> threshold+abs(ymax-ymin)*0.01,threshold+abs(ymax-ymin)*0.01,pred$l75)
	pred$l50 <- ifelse(pred$l50> threshold+abs(ymax-ymin)*0.01,threshold+abs(ymax-ymin)*0.01,pred$l50)
	pred$l25 <- ifelse(pred$l25> threshold+abs(ymax-ymin)*0.01,threshold+abs(ymax-ymin)*0.01,pred$l25)
	pred$h95 <- ifelse(pred$h95> threshold+abs(ymax-ymin)*0.01,threshold+abs(ymax-ymin)*0.01,pred$h95)
	pred$h75 <- ifelse(pred$h75> threshold+abs(ymax-ymin)*0.01,threshold+abs(ymax-ymin)*0.01,pred$h75)
	pred$h50 <- ifelse(pred$h50> threshold+abs(ymax-ymin)*0.01,threshold+abs(ymax-ymin)*0.01,pred$h50)
	pred$h25 <- ifelse(pred$h25> threshold+abs(ymax-ymin)*0.01,threshold+abs(ymax-ymin)*0.01,pred$h25)
	
	pred$forecast <- ifelse(pred$forecast< ymin-abs((ymax-ymin)*0.01),ymin-abs((ymax-ymin)*0.01),pred$forecast)
	pred$l95 <- ifelse(pred$l95< ymin-abs((ymax-ymin)*0.01),ymin-abs((ymax-ymin)*0.01),pred$l95)
	pred$l75 <- ifelse(pred$l75< ymin-abs((ymax-ymin)*0.01),ymin-abs((ymax-ymin)*0.01),pred$l75)
	pred$l50 <- ifelse(pred$l50< ymin-abs((ymax-ymin)*0.01),ymin-abs((ymax-ymin)*0.01),pred$l50)
	pred$l25 <- ifelse(pred$l25< ymin-abs((ymax-ymin)*0.01),ymin-abs((ymax-ymin)*0.01),pred$l25)
	pred$h95 <- ifelse(pred$h95< ymin-abs((ymax-ymin)*0.01),ymin-abs((ymax-ymin)*0.01),pred$h95)
	pred$h75 <- ifelse(pred$h75< ymin-abs((ymax-ymin)*0.01),ymin-abs((ymax-ymin)*0.01),pred$h75)
	pred$h50 <- ifelse(pred$h50< ymin-abs((ymax-ymin)*0.01),ymin-abs((ymax-ymin)*0.01),pred$h50)
	pred$h25 <- ifelse(pred$h25< ymin-abs((ymax-ymin)*0.01),ymin-abs((ymax-ymin)*0.01),pred$h25)

	pred_tmp <- pred
	if ( max_prediction_length_org != 0 && max_prediction_length > max_prediction_length_org)
	{
		pred_tmp <- pred[1:max_prediction_length_org,]
	}


	alp = 0.15
	if ( fit_model == "" || fit_model == "lm_" || fit_model == "exp_")
	{
		alp = 0.5
	}else
	{
		col = 'blue'
		fill_col <- c("#6875B1","#A0A5C7", "#CBCCD9", "#E0E0E1")
	}
	
	if (!use_plophet )
	{
		plt2 <- plt2 +
		geom_line(data=pred_tmp, mapping = aes_string(x = "time_index", y = "forecast")) +
		geom_line(data=pred_tmp, mapping = aes_string(x = 'time_index', y = 'forecast'), colour= col) +
		geom_ribbon(data=pred_tmp, fill=fill_col[1], mapping = aes_string(x = 'time_index', ymin = 'l95', ymax = 'h95'), alpha = alp)+
		geom_ribbon(data=pred_tmp, fill=fill_col[1], mapping = aes_string(x = 'time_index', ymin = 'l75', ymax = 'h75'), alpha = alp)+
		geom_ribbon(data=pred_tmp, fill=fill_col[2], mapping = aes_string(x = 'time_index', ymin = 'l50', ymax = 'h50'), alpha = alp)+
		geom_ribbon(data=pred_tmp, fill=fill_col[2], mapping = aes_string(x = 'time_index', ymin = 'l25', ymax = 'h25'), alpha = alp)+
		#scale_y_continuous(limits = c(0, ymax))+
		labs(title=sprintf("[%s] RUL:%s([%s] arima)\n%s", colname, failure_time_str, fit_model,failure_time50p_str))+
		theme(legend.position = "none")
	}else{
		plt2 <- plt2 +
		geom_line(data=pred_tmp, mapping = aes_string(x = "time_index", y = "forecast")) +
		geom_line(data=pred_tmp, mapping = aes_string(x = 'time_index', y = 'forecast'), colour=col) +
		geom_ribbon(data=pred_tmp, fill=fill_col[1], mapping = aes_string(x = 'time_index', ymin = 'l95', ymax = 'h95'), alpha = alp)+
		#scale_y_continuous(limits = c(0, ymax))+
		labs(title=sprintf("[%s] RUL:%s([%s]prophet\n%s", colname, failure_time_str, fit_model,failure_time50p_str))+
		theme(legend.position = "none")
	}
	
	plt2 <- plt2 + geom_hline(aes(yintercept=threshold, linetype = "twodash"),color = "red")
	#------------------------------------------------
	if ( fit_model == "" )
	{
		
		plt2 <- plt2 + labs(x=sprintf("[%s] Current time:%s",timeStamp, current_time))
	}
	#------------------------------------------------
	print(plt1)
	print(plt2)

	gfm2_org <- NULL
	rm(gfm2_org)
	return( list(plt1, plt2, failure_time, failure_time50p, failure_time2))
}

#Data feature confirmation


# feature_summary_visualization <- function(df0, feature_param)
# {
# 	colname = colnames(df0)
# 
# 	#df <-cbind(c(1:nrow(df0)), df0)
# 	#colnames(df) <- c("time_index",colname)
# 
# 	df2 <- smooth(df, smooth_window = smooth_window)
# 
# 	#前半80%を正常データとして訓練
# 	mahalanobis_train <- df2[1:(nrow(df2))*0.8,]
# 	m_mahalanobis <<- anomaly_detection_train(mahalanobis_train[colnames(mahalanobis_train)!="time_index"])
# 
# 	mahalanobis_test <- anomaly_detection_test(m_mahalanobis, mahalanobis_train[colnames(mahalanobis_train)!="time_index"])
# 	plot(mahalanobis_test[[2]], type="l")
# 
# 	mahalanobis_df <- data.frame(
# 			time_index=c(1:length(mahalanobis_test[[2]])), 
# 			Abnormality=as.vector(mahalanobis_test[[2]]))
# 
# 	threshold_ = max(mahalanobis_df["Abnormality"])
# 	print(sprintf("threshold:%f", threshold_))
# 	flush.console()
# 
# 	plt_abnormality <- mahalanobis_df %>% ggplot(aes(x=time_index,y=Abnormality)) + geom_line()
# 	plt_abnormality
# 
# 	#特徴量生成
# 	#後半10%から特徴量を推定
# 	feature_train <- df2[(nrow(df2) - (nrow(df2))*0.9):nrow(df2),]
# 	feature_df <- feature(df2, lookback=lookback)
# 	#print(sprintf("%d/%d nrow(feature_df):%d", i, as.integer(nrow(df)/one_input),nrow(feature_df)))
# 	print("feature_df")
# 	print(colnames(feature_df))
# 	write.csv(feature_df, "./feature_df.csv", row.names = F)
# 	flush.console()
# 
# 	#各特徴量のmonotonicity算出
# 	fm <- feature_monotonicity(feature_df, monotonicity_num=monotonicity_num)
# 	fm <- rbind(fm, c(1:ncol(fm)))
# 
# 
# 	#各特徴量のパラメータ並べ替え
# 	print("fm")
# 	print(colnames(fm))
# 	f1 <- data.frame(matrix(colnames(fm)),ncol=1)[,1]
# 	f2 <- cbind(f1, data.frame(as.numeric(fm[1,]),ncol=1))[,1:2]
# 	f2 <- cbind(f2, data.frame(as.numeric(fm[2,]),ncol=1))[,1:3]
# 	colnames(f2) <- c("feature", "monotonicity", "index")
# 	print(f2)
# 
# 	#各特徴量のパラメータの初期値設定（閾値、Ymaxのパラメータセット）
# 	feature_param <<- init_feature_param(f2, threshold, -10000, 10000)
# 	feature_param
# 	
# 	for ( k in 2:nrow(feature_param) )
# 	{
# 		x <- c(feature_df[,feature_param[k,1]])
# 		mu = mean(x, na.rm = TRUE)
# 		sd = sd(x, na.rm = TRUE)
# 		
# 		n = length(x)
# 		p = 1.0 - 99.99999/100
# 		a = abs(qt((p/2),n-1))
# 		thr = mu+a*sd/n
# 
# 		max = max(x, na.rm = TRUE)
# 		if ( max*0.4 > thr )
# 		{
# 			thr = max*0.4
# 		}
# 		feature_param <<- set_threshold(feature_param[k,1], thr)
# 	}
# 	feature_param <<- set_threshold("mahalanobis", threshold_*0.97)
# 
# 	#monotonicity毎の棒グラフ
# 	xlab=sprintf("feature [total number of features:%d]", nrow(f2)-1)
# 	plt0 <- f2 %>% ggplot(aes(x = reorder(feature,-monotonicity), y = monotonicity, fill = feature))+ geom_bar(stat = "identity")+ theme(legend.position = 'none')+xlab(xlab)+ theme(axis.text.x = element_blank())
# 	plt0
# 	
# 
# 	#monotonicityの大きい順にソート
# 	leave_num = 5
# 	fm2 <- f2[order(f2$monotonicity, decreasing=T),][1:leave_num,]
# 	print("fm2")
# 	print(colnames(fm2))
# 	flush.console()
# 	
# 	tracking_feature <<- c(1:leave_num)
# 
# 	fm22 <- rbind(fm2, f2[f2$feature=="mahalanobis",])
# 	fm22[fm22$feature=="mahalanobis",]$feature = "abnormality"
# 	#monotonicity毎の棒グラフ
# 	plt1 <- fm22 %>% ggplot(aes(x = reorder(feature,-monotonicity), y = monotonicity, fill = feature))+ geom_bar(stat = "identity")+xlab(sprintf("feature top %d & abnormality",leave_num))+
# 	geom_text(aes(label = ifelse(monotonicity > 0.001 ,as.integer(monotonicity*1000)/1000,monotonicity)), size = 4, hjust = 0.5, vjust = 2, position = "stack") 
# 	
# 	plt1
# 	
# 
# 	for ( i in 1:(leave_num) )
# 	{
# 		tracking_feature[i] <<- colnames(feature_df)[fm2$index[i]]
# 	}
# 	if ( !length(which("mahalanobis" == tracking_feature)))
# 	{
# 		tracking_feature[leave_num] <<- "mahalanobis"
# 	}
# 	print(tracking_feature)
# 	
# 	feature_name = colnames(feature_df)[fm2$index[1]]
# 	feature_plt <- feature_df %>% ggplot(aes(x=time_index,y=feature_name)) + geom_line()+
# 	xlab(feature_name)
# 	feature_plt
# 
# 	plt <- gridExtra::grid.arrange(plt_abnormality, feature_plt, plt0, plt1, nrow = 2)
# 	plt
# 	
# 	s = "feature_summary.png"
# 	ggsave(file = paste(putpng_path, s, sep=""), plot = plt, , dpi = 100, width = 12.5*1.2, height = 6.8*1.0)
# 
# 	#return(list(feature_param,tracking_feature))
# }
# 

get_csvdata <- function( file, tracking_feature_ , timeStamp)
{
	print(sprintf("get_csvdata(%s)", file))
	df0 <- get_data_frame(file, timeStamp)
	#print(tracking_feature_)
	#print("==== df0 colnames====")
	#print(colnames(df0))
	#print(df0[,tracking_feature_])

	maintenance_flag_idx = which("maintenance" == colnames(df0))
	if ( length(maintenance_flag_idx) > 0 )
	{
		df0 <- as.data.frame(df0[,c(timeStamp,tracking_feature_, "maintenance")])
		colnames(df0) <- c(timeStamp,tracking_feature_, "maintenance")
	}else
	{
		df0 <- as.data.frame(df0[,c(timeStamp,tracking_feature_)])
		colnames(df0) <- c(timeStamp,tracking_feature_)
	}
	colname = colnames(df0)

	print("==== df0 ====")
	print(head(df0))
	df <-cbind(data.frame(time_index=c(time_Index:(time_Index+nrow(df0)-1))), df0)
	colnames(df) <- c("time_index",colname)
	
	time_Index <<- time_Index+nrow(df0)
	return (df)
}

features_plot <- function(tracking_feature)
{
	feature_df <- read.csv( "./feature_df.csv", header=T, stringsAsFactors = F, na.strings = c("", "NA"))
	
	if ( is.null(tracking_feature))
	{
		if ( nrow(feature_df) > 2000 )
		{
			x <- feature_df[(nrow(feature_df)-1000):nrow(feature_df),]
		}else
		{
			x <- feature_df[1:nrow(feature_df),]
		}
		x <- reshape2::melt(x, id.vars=c("time_index"), measure.vars=colnames(x)[colnames(x)!="time_index"], 
						variable.name="key",value.name="target")
		p <- x %>% 
		  ggplot(aes(x = time_index, y = target, color=key))+
		  geom_line()
	}else
	{
		x <- feature_df[c("time_index",tracking_feature)]
		#x <- feature_df[c(tracking_feature)]
		x <- reshape2::melt(x, id.vars=c("time_index"), measure.vars=colnames(x)[colnames(x)!="time_index"], 
						variable.name="key",value.name="target")
		p <- x %>% 
		  ggplot(aes(x = time_index, y = target, color=key))+
		  geom_line()
	}
	plot(p)
	s = "feature_summary2.png"
	ggsave(file = paste(putpng_path, s, sep=""), plot = p, , dpi = 100, width = 12.5*1.2, height = 6.8*1.0)
	
	return(p)
}	

parameter_check <- function()
{
	if (max_data_len <  abs(train_num) )
	{
		print(sprintf("max_data_len < abs(train_num)"))
		return( -1 )
	}
	if (max_data_len <  abs(monotonicity_num) )
	{
		print(sprintf("max_data_len < monotonicity_num"))
		return( -1 )
	}
	if (max_data_len <  lookback )
	{
		print(sprintf("max_data_len < lookback"))
		return( -1 )
	}
}

initial_pm <- function(sigin_arg)
{
	#各特徴量毎の閾値、Ymaxのパラメータセット
	feature_param <<- NULL
	
	m_mahalanobis <<- NULL

	#送られてくるデータの全てで過去から現在まで
	#予測に用いる最大データ長までに制限したデータフレーム
	pre <<- NULL
	#最大保持長までに制限したデータフレーム
	past <<- NULL

	#予測モデル選択
	use_auto_arima <<- F
	use_arima <<- F
	use_ets <<- F
	use_plophet <<- F

	#追跡していく特徴量
	tracking_feature <<- c()
	
	dynamic_threshold <<- TRUE
	#max_train_span <<- 1000000
	
	#if ( sigin_arg == "-")
	#{
	#	threshold <<-  -threshold
	#}
}

file_division <- function(df)
{
	if ( ! file.exists("files") )
	{
		dir.create("files")
	}
	
	FN <- list.files("./files", pattern="\\.csv$")
	
	file.remove(paste("./files/",FN,sep=""))
#print(one_input)

	files <- NULL
	for ( i in 1:nrow(df))
	{	#データ切り出し
		if ( i*one_input > nrow(df) ) break
		df2 <- as.data.frame(df[((i-1)*one_input+1):(i*one_input),])
		if ( (i+1)*one_input >= nrow(df) )
		{
			df2 <- as.data.frame(df[((i-1)*one_input+1):nrow(df),])
		}
		
		
		colnames(df2) <- colnames(df)
		file <- sprintf("./files/%d_%d-%d.csv", i, ((i-1)*one_input+1), (i*one_input))
		
		#print(sprintf("%d->%d nrow:%d", ((i-1)*one_input+1), (i*one_input), nrow(df2)))
		write.csv(df2, file, row.names = F)
		
		if ( is.null(files))
		{
			files <- file
		}else
		{
			files <- c(files, file)
		}
	}
	
	return(files)
}

images_clear <- function()
{
	FN <- list.files(putpng_path, pattern="\\.png$")
	
	file.remove(paste(putpng_path,FN,sep=""))
}

window_moving_size <- function(N, window_size, slide_size)
{
	if ( window_size < 1 ) return (N)
	if ( slide_size < 1 ) return (N)
	if ( N - window_size < 1 ) return (0)
	return( floor((N - window_size) / slide_size) + 1)
}

RUL_hist <- NULL
RUL_hist_pre <- NULL

RUL <- NULL
sigin = 1
max_prediction_length_org = 0
current_time <- NULL
current_time_index <- 0
delta_time <- NULL
startup_data_frame <- TRUE
predictin <- function(df, tracking_feature_args, timeStamp_arg, sigin_arg)
{
	print("======tracking_feature_args==")
	print(tracking_feature_args)
	print("============================")
	print("======timeStamp_arg==")
	print(timeStamp_arg)
	print("============================")
	print("======sigin_arg==")
	print(sigin_arg)
	print("============================")
	if ( sigin_arg == "" || sigin_arg == "+" )
	{
		sigin <<- 1
	}else
	{
		sigin <<- -1
	}

	#sink(file = "log.txt") 
	file_list <- file_division(df)
	print("file_list")
	print(length(file_list))
	if ( nrow(df) >= 1 && length(file_list) == 0 )
	{
		file <- sprintf("./files/%d_%d-%d.csv", 0, 0, nrow(df))
		write.csv(df, file, row.names = F)
		file_list <- c( file)
	}
	
	for ( i in 1:length(file_list))
	{
		index_number <<- index_number + 1
		
		df2 <- fread(file_list[i], na.strings=c("", "NULL"), header = TRUE, stringsAsFactors = TRUE)
		if ( !is.null(df2))
		{
			df2 <- as.data.frame(df2)
		}
		if ( is.null(df2) || nrow(df2) < 1 || ncol(df2) < 1)
		{
			tryCatch({
					df2 <- read.csv( file_list[i], header=T, stringsAsFactors = F, na.strings = c("", "NA"))
			},error = function(e) {
				df2 <- NULL
			},finally = { 
				#OK
			}, silent = T
			)
		}
		if ( is.null(df2) )
		{
			next
		}

		
		timeStamp <<- timeStamp_arg
		df2[, timeStamp] <- as.POSIXct(df2[, timeStamp], tz='UTC')
		
		current_time <<- df2[nrow(df2), timeStamp]
		current_time_index <<- df2$time_index[nrow(df2)]
		
		print("####################")
		time_diff <- difftime(current_time , df2[(nrow(df2)-1), timeStamp],units = "auto")
		if ( unit_of_time == 'sec' )
		{
			time_diff <- difftime(current_time , df2[(nrow(df2)-1), timeStamp],units = "secs")
		}
		if ( unit_of_time == 'min' )
		{
			time_diff <- difftime(current_time , df2[(nrow(df2)-1), timeStamp],units = "mins")
		}
		if ( unit_of_time == 'h' )
		{
			time_diff <- difftime(current_time , df2[(nrow(df2)-1), timeStamp],units = "hours")
		}
		if ( unit_of_time == 'day' )
		{
			time_diff <- difftime(current_time , df2[(nrow(df2)-1), timeStamp],units = "days")
		}
		print(sprintf("%d current_time:%s dt:%f %s", i, current_time, time_diff, unit_of_time))

		delta_time <<- time_diff <- difftime(current_time , df2[(nrow(df2)-1), timeStamp])
		
		#
		#if ( startup_data_frame )
		if ( FALSE )
		{
			startup_data_frame = FALSE
			
			df2_length = nrow(df2)
			n <- window_moving_size(nrow(df2), smooth_window, smooth_window_slide)
			n <- window_moving_size(n, lookback, lookback_slide)
			n <- window_moving_size(n, smooth_window2, smooth_window_slide2)
			#print(n)
			#print(head(df2))
			#print(delta_time)
			#while( n < max(abs(train_num),abs(monotonicity_num)))
			for( kk in 1:(3*lookback+smooth_window2+smooth_window))
			{
				#print(df2[1, timeStamp])
				x <- seq(df2[1, timeStamp], length.out = 2, by=-delta_time)
				#print(x)
				#print("")
				df2 <- bind_rows(df2[1,], df2)
				df2[1,timeStamp] <- x[2]
				#print(head(df2))
				n <- window_moving_size(nrow(df2), smooth_window, smooth_window_slide)
				n <- window_moving_size(n, lookback, lookback_slide)
				n <- window_moving_size(n, smooth_window2, smooth_window_slide2)
				print(sprintf("df2_org:%d new df2:%d n:%d", df2_length, nrow(df2), n))
			}
			dummy_length = nrow(df2) - df2_length
			max_train_span = max_train_span + dummy_length
			try(write.csv(df2, "./df2.csv", row.names = F), silent=F)
		}
		
		
		df2_bak <- df2
		df2[,timeStamp] <- NULL
		#print(head(df2))
		#print(current_time)
		
		
		df2_org <- df2

		#if ( use_spline )
		if ( FALSE )
		{
			for ( k in 1:ncol(df2))
			{
				if ( colnames(df2)[k] == "time_index")
				{
					next
				}
				
				for ( kk in 1:2 )
				{
					sp <- gam(y~s(x), data=data.frame(y=df2[,k], x=df2$time_index))
					ypred <- predict(sp,data.frame(x=df2$time_index))
					df2[,k] <- c(ypred)
				}
				colnames(df2) <- colnames(df)
			}
		}
		
		df2[is.na(df2)] <- 0
		
		for ( k in 1:ncol(df2))
		{
			if ( colnames(df2)[k] == "time_index")
			{
				next
			}
			if ( colnames(df2)[k] == "maintenance")
			{
				next
			}
			df2[,k] <- as.numeric(df2[,k])
		}
		#過去データとの結合と最大長超えカット
		if ( is.null(past))
		{
			past <<- df2
		}else
		{
			past <<- dplyr::bind_rows(past, df2)
		}
		past <<- as.data.frame(past)
		#print(past)	
		if ( nrow(past) > max_retained_length )
		{
			past <<- as.data.frame(past[one_input:nrow(past),])
			colnames(past) <- colnames(df2)
			
			print("nrow(past) > max_retained_length")
			#break
		}
		
		print(sprintf("train:%d/%d %.3f%%", df2$time_index[nrow(df2)], max_train_span, 100*df2$time_index[nrow(df2)]/max_train_span))
		if ( df2$time_index[nrow(df2)] > max_train_span )
		{
			dynamic_threshold = FALSE
		}

		if ( !is.null(pre) )
		{
			#print(str(pre))
			#print("=====")
			#print(str(df2))
		
			#df2 <- rbind(pre, df2)
			df2 <- dplyr::bind_rows(pre, df2)
			#df2_org <- rbind(pre_org, df2_org)
			for ( k in 1:ncol(pre_org))
			{
				if ( colnames(pre_org)[k] == "time_index")
				{
					next
				}
				if ( colnames(pre_org)[k] == "maintenance")
				{
					next
				}
				pre_org[,k] <- as.numeric(pre_org[,k])
				df2_org[,k] <- as.numeric(df2_org[,k])
			}
			
			#print(str(pre_org))
			#print("=====")
			#print(str(df2_org))
			
			df2_org <- dplyr::bind_rows(pre_org, df2_org)
			
			print(sprintf("row bind :nrow(df2):%d", nrow(df2)))
			#if ( nrow(df2) > max_data_len*3 + one_input )
			#{
			#	df2 <- as.data.frame(df2[one_input:nrow(df2),])
			#	colnames(df2) <- colnames(df2_org)
			#	
			#	df2_org <- as.data.frame(df2_org[one_input:nrow(df2_org),])
			#	colnames(df2_org) <- colnames(df2_org)
			#}
		}
		
		if ( nrow(df2) > max_retained_length )
		{
			n = as.integer(nrow(df2)*0.2)
			df2 <- as.data.frame(df2[n:nrow(df2),])
		}

		pre <<- NULL
		pre_org <<- NULL
		freeram()
		
		pre <<- df2
		pre_org <<- df2_org
		print(sprintf("-> nrow(df2):%d", nrow(df2)))


		n <- window_moving_size(nrow(df2), smooth_window, smooth_window_slide)
		print(n)
		n <- window_moving_size(n, lookback, lookback_slide)
		print(n)
		n <- window_moving_size(n, smooth_window2, smooth_window_slide2)
		print(n)
		if ( n < max(abs(train_num),abs(monotonicity_num)))
		{
			print(sprintf("n:%d max(abs(train_num),abs(monotonicity_num)):%d",n, max(abs(train_num),abs(monotonicity_num))))
			#print("#データがまだ足りていない")
			flush.console()
			#next
		}
		
		#if ( nrow(df2) - smooth_window <  max_data_len*3 )
		#{
		#	print("*データがまだ足りていない")
		#	#print(lookback)
		#	print(sprintf("past:%d",nrow(past)))
		#	flush.console()
		#	next
		#}
		if ( smooth_window > 1 )
		{
			df2_tmp <- try(
						moving_average(sampling=TRUE, df2, lookback=smooth_window, slide_window=smooth_window_slide)
						,silent=F)
			
			#df2_tmp <- try(smooth(df2, smooth_window = smooth_window, smooth_window_slide=smooth_window_slide),silent=F)
			if ( class(df2_tmp) == "try-error"  || is.null(df2_tmp))
			{
				print(sprintf("past:%d",nrow(past)))
				print("*データがまだ足りていない")
				flush.console()
				next
			}
			current_time_index2 <- df2_tmp$time_index[nrow(df2_tmp)]
			s = which(current_time_index2 == df2_bak$time_index)
			if ( length(s) == 1 )
			{
				current_time <<- df2_bak[s, timeStamp]
			}
		}else
		{
			df2_tmp <- df2
		}
		print(sprintf("sampling nrow(df2_tmp):%d", nrow(df2_tmp)))


		df2 <- NULL
		rm(df2)
		freeram()

		#print(sprintf("lookback*3:%d > df2_tmp:%d", lookback*3, nrow(df2_tmp)))
		#print(sprintf("smooth_window*3:%d > df2_tmp:%d", smooth_window*3, nrow(df2_tmp)))
		print(sprintf("max(abs(monotonicity_num),abs(train_num)):%d > df2_tmp:%d", max(abs(monotonicity_num),abs(train_num)), nrow(df2_tmp)))
		#if ( lookback*3 > nrow(df2_tmp) || smooth_window*3 > nrow(df2_tmp) || max(abs(monotonicity_num),abs(train_num)) > nrow(df2_tmp))
		if ( max(abs(monotonicity_num),abs(train_num)) > nrow(df2_tmp))
		{
			print("データがまだ足りていない")
			#print(lookback)
			print(sprintf("past:%d",nrow(past)))
			flush.console()
			next
		}

		
		if ( use_spline )
		#if ( FALSE )
		{
			#print(colnames(df2_tmp))
			#print(str(df2_tmp))
			for ( k in 1:ncol(df2_tmp))
			{
				if ( colnames(df2_tmp)[k] == "time_index")
				{
					next
				}
				
				for ( kk in 1 )
				{
					sp <- gam(y~s(x), data=data.frame(y=df2_tmp[,k], x=df2_tmp$time_index))
					ypred <- predict(sp,data.frame(x=df2_tmp$time_index))
					df2_tmp[,k] <- c(ypred)
				}
				colnames(df2_tmp) <- colnames(df2_org)
			}
			#print(colnames(df2_tmp))
			#print(str(df2_tmp))
		}
		
		print(colnames(df2_tmp))
		print(str(df2_tmp))
		flush.console()
		
		#異常度計算モデル作成
		mahalanobis_train <- past[1:min(10000,(nrow(past))*0.8),]
		print(sprintf("mahalanobis_train:%d", nrow(mahalanobis_train)))
		flush.console()
		m_mahalanobis <<- anomaly_detection_train(mahalanobis_train)
		
		print("m_mahalanobis end")
		#print(m_mahalanobis)
		flush.console()
		
		#特徴量生成
		print("create feature start")
		feature_df <- try(
			feature(df2_tmp, lookback=lookback, slide_window = lookback_slide))
		if ( class(feature_df) == "try-error" )
		{
			print(sprintf("feature_df:%d",nrow(feature_df)))
			flush.console()
			next
		}
		print("create feature end")
		
		feature_df_none_smooth <- NULL
		rm(feature_df_none_smooth)
		
		df2_tmp <- NULL
		rm(df2_tmp)
		freeram()

		feature_df_none_smooth <- NULL
		if ( smooth_window2 > 1 )
		{
			if ( use_lowess )
			{
				feature_df_none_smooth <- feature_df
			}
			feature_df <- try(
			smooth(feature_df, smooth_window = smooth_window2, smooth_window_slide=smooth_window_slide2),silent=F)
			if ( class(feature_df) == "try-error" || is.null(feature_df))
			{
				print("データがまだ足りていない")
				print(sprintf("smooth_window2:%d", smooth_window2))
				flush.console()
				next
			}
		}
		
		
		print(sprintf("%d/%d nrow(feature_df):%d", i, as.integer(nrow(df)/one_input),nrow(feature_df)))
		print("feature_df")
		print(colnames(feature_df))
		try(write.csv(feature_df, "./feature_df.csv", row.names = F), silent=F)
		
		result_png = sprintf("result-%06d.png", index_number)
		
		
		#各特徴量のmonotonicity算出
		if ( nrow(feature_df) <= max(abs(train_num),abs(monotonicity_num)))
		{
			print(sprintf("nrow(feature_df):%d <= abs(monotonicity_num):%d", nrow(feature_df),abs(monotonicity_num)) )
			print(sprintf("nrow(feature_df):%d <= abs(train_num):%d", nrow(feature_df),abs(train_num)) )
			print("データがまだ足りていない")
			flush.console()
			next
		}
		
		fm <- feature_monotonicity(feature_df, monotonicity_num=monotonicity_num)
		fm <- rbind(fm, c(1:ncol(fm)))

		print("fm")
		print(colnames(fm))
		#各特徴量のパラメータ並べ替え
		f1 <- data.frame(matrix(colnames(fm)),ncol=1)[,1]
		f2 <- cbind(f1, data.frame(as.numeric(fm[1,]),ncol=1))[,1:2]
		f2 <- cbind(f2, data.frame(as.numeric(fm[2,]),ncol=1))[,1:3]
		colnames(f2) <- c("feature", "monotonicity", "index")
		
		#各特徴量のパラメータの初期値設定（閾値、Ymaxのパラメータセット）
		if ( is.null(feature_param) )
		{
			feature_param <<- init_feature_param(f2, threshold, -10000, 10000)
		}
			
		
		
		feature_df_org <- feature_df
		#for ( k in 1:ncol(fm))
		#{
		#	if ( fm[1,k] < 0 ) feature_df[,k] <- (-1.0)*feature_df[,k]
		#}
		
		#注視したいターゲットPlot
		id <- which(watch_name == colnames(feature_df))
		gfm2 <- data.frame(x=feature_df_org$time_index, y=feature_df_org[,id])
		looked_var_plt <- ggplot(data=gfm2, aes(x = x, y = y)) + geom_line(color = "#4169e1", linewidth =1.0)+
			labs(title=sprintf("Looked at Variables[%s]",watch_name))
		
		#if ( use_spline )
		#{
		#	sp <- gam(y~s(x), data=gfm2)
		#	ypred <- predict(sp,data.frame(x=gfm2$x))
		#	spdf <- data.frame(x=gfm2$x, y=ypred)
		#	looked_var_plt <- looked_var_plt +geom_line(data=spdf,aes(x = x, y = y), color = "red", linewidth =1.0)
		#}
		if ( use_spline )
		{
			idx <- which(watch_name == paste(colnames(df2_org),"..", sep=""))
			#sp <- gam(y~s(x), data=data.frame(x=df2_org$time_index, y=df2_org[,idx]))
			#ypred <- predict(sp,data.frame(x=df2_org$time_index))
			#spdf <- data.frame(x=df2_org$time_index, y=ypred)
			#looked_var_plt <- looked_var_plt +geom_line(data=spdf,aes(x = x, y = y), color = "#ff4500", linewidth =1.0)
			
			org_data <- data.frame(x=df2_org$time_index, y=df2_org[,idx])
			looked_var_plt <- looked_var_plt +geom_line(data=org_data,aes(x = x, y = y), color = "#c0c0c0", linewidth =1.0)
		}
		
		tracking_feature_tmp <- c(1:5)
		if ( is.null(tracking_feature))
		{
			#monotonicityの大きい順にソート
			if ( sigin > 0 )
			{
				fm2 <- f2[order((f2$monotonicity), decreasing=T),][1:5,]
			}else
			{
				fm2 <- f2[order((f2$monotonicity), decreasing=F),][1:5,]
			}
			print("fm2")
			print(colnames(fm2))

			#monotonicity毎の棒グラフ
			plt0 <- fm2 %>% ggplot(aes(x = feature, y = abs(monotonicity), fill = feature))+ geom_bar(stat = "identity")
			#ggsave(file = paste(putpng_path, result_png, sep=""), plot = plt0, dpi = 320)

			max_id <- which(abs(fm2$monotonicity) == max(abs(fm2$monotonicity)))
			fm2$feature[max_id]

			#注視したいターゲット
			#watch_name = colnames(feature_df)[2]
			#print(sprintf("watch_name %s", watch_name))
			
			
			tracking_feature_tmp[1] = fm2$feature[1]
			tracking_feature_tmp[2] = fm2$feature[2]
			tracking_feature_tmp[3] = fm2$feature[3]
			
			feature_param <<- set_count(tracking_feature_tmp[1])
			feature_param <<- set_count(tracking_feature_tmp[2])
			feature_param <<- set_count(tracking_feature_tmp[3])

			tracking_feature_tmp = tracking_feature_args

			#tracking_feature_tmp[1] = "mahalanobis"
			#tracking_feature_tmp[2] = "X793.peak2peak"
			#tracking_feature_tmp[3] = "X793.ShapeFactor"
			
		}else
		{
			tracking_feature_tmp = tracking_feature
		}
		
		print(sprintf("dynamic_threshold:%d", ifelse(dynamic_threshold==TRUE,1,0)))
		if (dynamic_threshold && is.null(tracking_feature))
		{
			print(tracking_feature_tmp)
			for ( k in 1:3 )
			{
				threshold_empty = F
				thr0 = get_threshold(tracking_feature_tmp[k])
				if ( length(thr0) != 1 )
				{
					threshold_empty = T
				}
				ymax0 = get_ymax(tracking_feature_tmp[k])
				if ( length(ymax0) != 1 )
				{
					ymax0 = -10000
				}
				ymin0 = get_ymin(tracking_feature_tmp[k])
				if ( length(ymin0) != 1 )
				{
					ymin0 = 10000
				}
				
				x <- c(feature_df[,tracking_feature_tmp[k]])
				n <- length(x)
				mu = mean(x, na.rm = TRUE)
				sd = sd(x, na.rm = TRUE)
				
				p = 1.0 - 99.99999/100
				a = abs(qt((p/2),n-1))
				thr = mu+a*sd/n
				max = max(x, na.rm = TRUE)
				min = min(x, na.rm = TRUE)
				if ( threshold_empty || ymin0==10000 || ymax0 == -10000)
				{
					ymax0 = max
					ymin0 = min
					thr0 = ymax0 + abs((ymax0-ymin0)*0.175)
				}
				print(thr0)
				print(sprintf("%s N:%d", tracking_feature_tmp[k], n))
				print(sprintf("mu:%.3f sd:%.3f max:%.3f min:%.3f a:%.3f thr:%.3f", mu, sd, max, min, a, thr))
				print(sprintf("thr0:%.3f thr:%.3f", thr0, thr))
				print(sprintf("ymax0:%.3f ymin0:%.3f", ymax0, ymin0))
				
				if (tracking_feature_tmp[k] == "mahalanobis")
				{
					 thr = qchisq(0.9999,1)*0.8
				}
				if ( thr0 > thr )
				{
					thr = thr0
				}
				if ( ymax0 > thr )
				{
					thr = ymax0
					
				}
				if (tracking_feature_tmp[k] != "mahalanobis" && (!threshold_empty))
				{
					if ( thr < ymax0 + abs((ymax0-ymin0)*0.175) )
					{
						thr = ymax0 + abs((ymax0-ymin0)*0.175)
					}
				}
				if ( k == 2 && exists("threshold_target") )
				{
					print(sprintf("%s threshold_target:%f", tracking_feature_tmp[k], threshold_target))
					thr = threshold_target
				}
				print(sprintf("thr0:%.3f thr:%.3f", thr0, thr))
				feature_param <<- set_threshold(tracking_feature_tmp[k],thr)
			}
		}
		feature_param
		
		#各特徴量の予測結果とPlot
		tracking_feature_Num = 3
		failure_time_s = c(1:tracking_feature_Num)
		failure_time50p_s = c(1:tracking_feature_Num)
		failure_time2_s = c(1:tracking_feature_Num)
		plt_s = list()
		
		if ( max_prediction_length_org == 0 )
		{
			max_prediction_length_org <<- max_prediction_length;
		}
		print(sprintf("max_prediction_length:%d max_prediction_length_org:%d", max_prediction_length, max_prediction_length_org))

		break_flag = FALSE
		for ( k in 1:tracking_feature_Num )
		{
			max_prediction_length <<- 1*max_prediction_length_org

			rank = tracking_feature_tmp[k]
			plt1 <- plot_plot_feature_predict(list(feature_df,feature_df_none_smooth), train_num=train_num, rank=rank,
								 h=max_prediction_length, feature_smooth_window=feature_smooth_window)

			max_prediction_length <<- max_prediction_length_org
			
			if ( !is.null(plt1) )
			{
				failure_time_s[k] = plt1[[3]]
				failure_time50p_s[k] = plt1[[4]]
				failure_time2_s[k] = plt1[[5]]
				plt_s <- c(plt_s, list(plt1[[2]]))
			}else
			{
				plt_s <- NULL
				break
			}
			flush.console()
		}
		feature_df_none_smooth <- NULL
		rm(feature_df_none_smooth)
		
		if ( is.null(plt_s))
		{
			print("plot_plot_feature_predict error skipp")
			flush.console()
			next
		}


		#monotonicity最大になった特徴量を１カウントUp
		#feature_param <<- set_count(tracking_feature_tmp[1])

		#異常発生時刻が早い順にソート
		plt_list = list(plt_s[[1]], plt_s[[2]], plt_s[[3]])
		failure = data.frame(time = failure_time50p_s, pltid=c(1,2,3))
		#failure <- failure[order(failure$time),]

#//////////////////////////
		print(sprintf("#failure_time:%d  failure_time_init:%d", as.integer(failure_time_s[2]), as.integer(failure_time_init)))
		print(sprintf("#failure_time2:%d  failure_time_init:%d", as.integer(failure_time2_s[2]), as.integer(failure_time_init)))
		print(sprintf("#failure_time_50p:%d  failure_time_init:%d", as.integer(failure_time50p_s[2]), as.integer(failure_time_init)))
		delta_index <- mean(diff(feature_df$time_index))
		print(sprintf("delta_index:%f", delta_index))
		cat("delta_time")
		print(delta_time)


		failure_time_set = FALSE
		if ( failure_time50p_s[2] != failure_time_init && failure_time_s[2] != failure_time_init  && failure_time2_s[2] != failure_time_init)
		{
			if ( failure_time_s[2] < failure_time50p_s[2] && failure_time50p_s[2] < failure_time2_s[2] )
			{
				failure_time_set = TRUE
			}
		}
		cat("failure_time_set")
		print(failure_time_set)
		
		failure_time_index = current_time_index + failure_time50p_s[2]*delta_index
		failure_time_index1 = current_time_index + failure_time_s[2]*delta_index
		failure_time_index2 = current_time_index + failure_time2_s[2]*delta_index
		failure_time_index0 = failure_time_index2
		
		under_maintenance = FALSE
		if ( failure_time_set )
		{
			if ( is.null(RUL_hist) )
			{
				print(sprintf("delta_index:%f", delta_index))
			

				x <- rev(seq(current_time_index, length.out = nrow(feature_df), by=-delta_index))
				x <- seq(x[1], length.out = as.integer(failure_time_index2/delta_index + 1), by = delta_index)

				RUL_hist <- data.frame(time_index = x, hist=numeric(length(x)))

				print("*nrow(RUL_hist)")
				print(nrow(RUL_hist))
				print(sprintf("*current_time_index:%d  failure_time50p_s:%f failure_time_index:%f", current_time_index, failure_time50p_s[2],failure_time_index))
				flush.console()
			}else
			{
			
				maintenance_flag_idx = which("maintenance" == colnames(feature_df))
				if ( length(maintenance_flag_idx) < 1 )
				{
					maintenance_flag_idx <- 0
				}
					
				if ( maintenance_flag_idx > 0 )
				{
					maintenance_posidx = which( feature_df$maintenance == 1 )
					if ( length(maintenance_posidx) < 1)
					{
						maintenance_posidx = NULL
					}else
					{
						for ( jj in 0:max(2,min(c(lookback_slide/4,smooth_window2/4))) )
						{
							if ( feature_df$maintenance[nrow(feature_df)-jj] == 1 ) 
							{
								under_maintenance = TRUE
								break
							}
						}
					}
				}
			
				e = RUL_hist$time_index[nrow(RUL_hist)]
				print(sprintf("current_time_index:%d  failure_time50p_s:%f failure_time_index:%f failure_time_index2:%f", current_time_index, failure_time50p_s[2],failure_time_index,failure_time_index2))
				flush.console()
								 
				
				failure_time_index0 = failure_time_index2
				if ( e < failure_time_index0 )
				{
					ee <- seq((e+delta_index), failure_time_index0, dy=delta_index)

					RUL_tmp <- data.frame(time_index = ee, hist=numeric(length(ee)))
					cat("RUL_tmp")
					print(str(RUL_tmp))

					RUL_hist <- dplyr::bind_rows(RUL_hist, RUL_tmp)
				}
			}

			RUL_hist$hist[1:nrow(RUL_hist)] <- RUL_hist$hist[1:nrow(RUL_hist)]*0.0

			cat("failure_time_index1")
			print(failure_time_index1)
			cat("failure_time_index")
			print(failure_time_index)
			cat("failure_time_index0")
			print(failure_time_index0)
			
			if ( failure_time_index1 <= failure_time_index && failure_time_index <= failure_time_index0 )
			{
				if (T)
				{
					row =  which.min( abs(failure_time_index - RUL_hist$time_index))
					row1 = which.min( abs(failure_time_index1 - RUL_hist$time_index))
					row0 = which.min( abs(failure_time_index0 - RUL_hist$time_index))
					indices1 = (row1:row0)

					print("row")
					print(row)
					print("row1")
					print(row1)
					print("row0")
					print(row0)
					print("nrow(RUL_hist)")
					print(nrow(RUL_hist))
					
					indices2 = indices1[indices1 < row]
					indices3 = (indices2 - row1)/(row - row1)

					indices4 = indices1[indices1 >= row]
					indices5 = (row0 - indices4)/(row0 - row)
					 
					v = numeric(nrow(RUL_hist))
					v[indices2] = indices3
					v[indices4] = indices5
					RUL_hist$hist[indices2] = RUL_hist$hist[indices2] + 0.05 + v[indices2]^2
					RUL_hist$hist[indices4] = RUL_hist$hist[indices4] + 0.05 + v[indices4]^2


					#plt_tmp <- ggplot()+ geom_line(data=RUL_hist, aes(x=time_index, y=hist), color = "#ff69b4")
					#ggsave(file=sprintf("%s%s%d_A.png", putpng_path, "debug/result-", index_number), plot = plt_tmp, dpi = 130, width = 13*1.2, height = 6.8*1.0)
					
					#tmp <- RUL_hist[indices1,]
					#plt_tmp <- ggplot()+ geom_line(data=tmp, aes(x=time_index, y=hist), color = "#ff69b4")
					#ggsave(file=sprintf("%s%s%d_B.png", putpng_path, "debug/result-", index_number), plot = plt_tmp, dpi = 130, width = 13*1.2, height = 6.8*1.0)
					
					rm(v)
					freeram()
				}
			}
			if ( under_maintenance && !is.null(RUL_hist))
			{
				RUL_hist$hist[1:nrow(RUL_hist)] <- 0
			}

#-------------------------------------------
			RUL_hist$TimeStamp <- as.POSIXct(RUL_hist$time_index, tz="UTC", origin="2024-01-01")

			cur_row = which.min( abs(current_time_index - RUL_hist$time_index))
			RUL_hist$TimeStamp[cur_row:nrow(RUL_hist)] <- seq(current_time, length.out = length(cur_row:nrow(RUL_hist)), by = delta_time*delta_index)
			RUL_hist$TimeStamp[1:cur_row] <- rev(seq(current_time, length.out = length(1:cur_row), by = -delta_time*delta_index))

#-------------------------------------------			
			#try(write.csv(RUL_hist, paste("./", base_name, "_RUL_hist.csv",sep=''), row.names = F),, silent = FALSE)
			
			RUL <<- c(RUL, failure_time50p_s[2])
			#try(write.csv(RUL, paste("./", base_name, "_RUL.csv",sep=''), row.names = F),, silent = FALSE)
		}else
		{
			if ( is.null(RUL_hist) )
			{
				x <- rev(seq(current_time_index, length.out = nrow(feature_df), by=-delta_index))
				x <- seq(x[1], length.out = nrow(feature_df)+max_prediction_length, by = delta_index)
				RUL_hist <- data.frame(time_index = x, hist=numeric(length(x)))
			}else
			{
				RUL_hist$hist <- 0
			}
		}
#//////////////////////////
			

		print(failure)
		plt_1 = plt_list[[failure$pltid[1]]]
		plt_2 = plt_list[[failure$pltid[2]]]
		plt_3 = plt_list[[failure$pltid[3]]]
		
		print(plt_1)
		print(plt_2)
		print(plt_3)
		
		plt <- NULL
		
		looked_var_plt <- plt_s[[2]]
		
		tracking_name = tracking_feature_tmp[2]
		
		cat("tracking_name")
		print(tracking_name)
		cat("RUL_hist")
		print(head(RUL_hist))
		cat("nrow(RUL_hist)")
		print(nrow(RUL_hist))
		print("------------------------------------------------------------------")
		flush.console()
		
		rul_hist_plt <- NULL

		if (!is.null(RUL_hist) && nrow(RUL_hist) > 1)
		{
			threshold_empty = F
			thr0 = get_threshold(tracking_name)
			if ( length(thr0) != 1 )
			{
				threshold_empty = T
			}
			ymax0 = get_ymax(tracking_name)
			if ( length(ymax0) != 1 )
			{
				ymax0 = -10000
			}
			ymin0 = get_ymin(tracking_name)
			if ( length(ymin0) != 1 )
			{
				ymin0 = 10000
			}
			flush.console()
		
			print(sprintf("max:%.3f min:%.3f thr:%.3f", ymax0, ymin0, thr0))
			x <- c(feature_df[,tracking_name])
			max = max(x, na.rm = TRUE)
			min = min(x, na.rm = TRUE)
			if ( threshold_empty || ymin0==10000 || ymax0 == -10000)
			{
				ymax0 = max
				ymin0 = min
				thr0 = ymax0 + abs((ymax0-ymin0)*0.175)
				if ( exists("threshold_target") )
				{
					print(sprintf("threshold_target:%f", threshold_target))
					thr0 = threshold_target
				}
			}
			
			
			RUL_hist$hist[is.na(RUL_hist$hist)] <- 0
			#print(max(RUL_hist$hist))
			#print(min(RUL_hist$hist))
			#cat("RUL_hist")
			#print(str(RUL_hist))

			dy = abs(ymax0 - ymin0)
			if ( !threshold_empty && thr0 > ymax )
			{
				dy = abs(thr0 - ymin0)
			}
			dh = (max(RUL_hist$hist) - min(RUL_hist$hist))
			print(sprintf("max:%.3f min:%.3f thr:%.3f dy:%f dh:%f", ymax0, ymin0, thr0, dy, dh))
			cat("threshold_empty")
			print(threshold_empty)
			flush.console()

			if ( !threshold_empty && dy > 0.0001 && dh > 0.00001)
			{
				print(sprintf("**max:%.3f min:%.3f thr:%.3f dy:%f dh:%f", ymax0, ymin0, thr0, dy, dh))
				flush.console()
				RUL_hist_tmp <- RUL_hist
				
				RUL_hist_tmp$hist_org <- RUL_hist_tmp$hist

				RUL_hist_tmp$hist <- (RUL_hist_tmp$hist - min(RUL_hist$hist))/dh
				RUL_hist_tmp$hist <- thr0 + 0.3*dy*RUL_hist_tmp$hist
				
				z1 <- lowess(RUL_hist_tmp$time_index, RUL_hist_tmp$hist, f = 0.001*3.0)$y

				RUL_hist_tmp$z1 <- ifelse(z1 < thr0, thr0, z1)
				
				cur_row = which.min(abs(current_time_index - RUL_hist$time_index))
				if ( cur_row-1 > 1 )
				{
					RUL_hist_tmp$z1[1:(cur_row-1)] <- thr0
				}
				
				#
				print("head(RUL_hist_tmp)")
				print(head(RUL_hist_tmp))

				#plt_tmp <- ggplot()+ geom_line(data=RUL_hist_tmp, aes(x=time_index, y=z1), color = "#ff69b4")+
				# geom_line(data=RUL_hist_tmp, aes(x=time_index, y=hist), color = "#ff69b4")
				#ggsave(file=sprintf("%s%s%d.png", putpng_path, "debug/result-", index_number), plot = plt_tmp, dpi = 130, width = 13*1.2, height = 6.8*1.0)

				#looked_var_plt <- looked_var_plt + geom_line(data=RUL_hist_tmp, aes(x=time_index, y=z1), color = "#ff69b4")
				#looked_var_plt <- looked_var_plt + geom_ribbon(data=RUL_hist_tmp, fill="#ff69b4", mapping = aes_string(x = 'time_index', ymin = thr0, ymax = z1), alpha = 0.2)

				if ( !is.null(RUL_hist_pre))
				{
					if ( nrow(RUL_hist_pre) < nrow(RUL_hist_tmp))
					{
						s = nrow(RUL_hist_pre)
						p <- RUL_hist_tmp[1:nrow(RUL_hist_tmp),]
						p$z1 <- thr0
						RUL_hist_pre <- rbind(RUL_hist_pre, p[(s+1):nrow(p),])
					}
					looked_var_plt <- looked_var_plt + geom_line(data=RUL_hist_pre, aes(x=RUL_hist_pre$time_index, y=RUL_hist_pre$z1), color = "#ff69b4",linetype="dashed", alpha = 0.3)
					looked_var_plt <- looked_var_plt + geom_ribbon(data=RUL_hist_pre, fill="#ff69b4", mapping = aes_string(x = 'time_index', ymin = thr0, ymax = RUL_hist_pre$z1), alpha = 0.3)
				}
				if ( F )
				{
					
					looked_var_plt <- looked_var_plt  +
						geom_bar(data=RUL_hist_tmp, aes(x = time_index, y = (z1-thr0), fill=(z1-thr0)), stat = "identity", position_nudge(y=thr0), alpha=0.95) +
						##scale_fill_viridis_c(option = "A")
						##scale_fill_viridis_c(option = "D")
						##scale_fill_viridis_c(option = "plasma")
						scale_fill_viridis_c(option = "H")
						##scale_fill_gradient(low="green", high="blue")
				}else
				{
					looked_var_plt <- looked_var_plt + geom_line(data=RUL_hist_tmp, aes(x=time_index, y=z1),alpha = 0.9)
					looked_var_plt <- looked_var_plt + geom_ribbon(data=RUL_hist_tmp, fill="green", mapping = aes_string(x = 'time_index', ymin = thr0, ymax = RUL_hist_tmp$z1), alpha = 0.9)
				}
				
				step <- nrow(RUL_hist)/4
				break_pos <- c()
				labels <- c()
				for ( i in 1:5 )
				{
					k = (i-1)*step+1
					if ( k > nrow(RUL_hist))
					{
						k = nrow(RUL_hist)
					}
					break_pos <- c(break_pos, RUL_hist$time_index[k])
					labels <- c(labels, as.character(RUL_hist$TimeStamp[k]))
				}
				
				z1max = which.max(RUL_hist_tmp$z1)
				timeStamp_ = RUL_hist$TimeStamp[z1max]
				#print(sprintf("timeStamp_:%s", timeStamp_))
				if ( T )
				{
					x <- rev(seq(current_time_index, length.out = nrow(feature_df), by=-delta_index))
					x <- seq(x[1], length.out = nrow(feature_df)+ failure_time50p_s[2], by = delta_index)

					tmp2 <- data.frame(time_index=x)
					tmp2$TimeStamp <- as.POSIXct(tmp2$time_index, tz="UTC", origin="2024-01-01")

					cur_row = nrow(feature_df)
					tmp2$TimeStamp[cur_row:nrow(tmp2)] <- seq(current_time, length.out = length(cur_row:nrow(tmp2)), by = delta_time*delta_index)
					tmp2$TimeStamp[1:cur_row] <- rev(seq(current_time, length.out = length(1:cur_row), by = -delta_time*delta_index))
					
					#print(tmp2$TimeStamp)
					#print(nrow(tmp2))
					timeStamp_ = tmp2$TimeStamp[nrow(tmp2)]
					#print(sprintf("#timeStamp_:%s", timeStamp_))
				}
				
				sum <- sum(RUL_hist_tmp$z1[1:nrow(RUL_hist_tmp)])
				prob <- c()
				text = sprintf("%s (%.2f%%)", timeStamp_, 100*0.0001)
				if ( abs(sum) > 0.0001)
				{
					prob <- RUL_hist_tmp$z1[1:nrow(RUL_hist_tmp)]/sum
					prob <- cumsum(prob[1:z1max])
					
					probMax <- prob[length(prob)] + (1/(1 + 0.25*failure_time50p_s[2])^2)
					text = sprintf("%s (%.2f%%)", timeStamp_, 100*min(0.999,probMax))
				}else
				{
					text = ""
				}

				#looked_var_plt <- looked_var_plt + scale_x_continuous(breaks = break_pos, labels = labels)
				y = max(RUL_hist_tmp$z1) - (max(RUL_hist_tmp$z1) - min(RUL_hist_tmp$z1))*0.05
				if ( RUL_hist_tmp$time_index[z1max] < current_time_index - delta_index)
				{
					text = "+Inf"
				}
				if ( text != "" )
				{
					looked_var_plt <- looked_var_plt + annotate("text", x=RUL_hist_tmp$time_index[z1max], y=y, label=text, colour="blue")
				}
				
				#plt_s[[1]] <- plt_s[[1]] + scale_x_continuous(breaks = break_pos, labels = labels)
				#plt_s[[2]] <- plt_s[[2]] + scale_x_continuous(breaks = break_pos, labels = labels)
				#plt_s[[3]] <- plt_s[[3]] + scale_x_continuous(breaks = break_pos, labels = labels)
				
				RUL_hist_pre <- RUL_hist_tmp
				
				RUL_hist_tmp <- NULL
				rm(RUL_hist_tmp)
				freeram()
			}
		}
		
		if ( T )
		{
			add_time_length = max_prediction_length
			#if ( failure_time_set )
			#{
			#	add_time_length = max(c(max_prediction_length, max(c(failure_time50p_s[2],failure_time_s[2],failure_time2_s[2]) )))
			#}
		
#/////////////////////////////////////////////
		
			x <- rev(seq(current_time_index, length.out = nrow(feature_df), by=-delta_index))
			x <- seq(x[1], length.out = nrow(feature_df)+max_prediction_length, by = delta_index)

			tmp2 <- data.frame(time_index=x)
			tmp2$TimeStamp <- as.POSIXct(tmp2$time_index, tz="UTC", origin="2024-01-01")

			cur_row = nrow(feature_df)
			tmp2$TimeStamp[cur_row:nrow(tmp2)] <- seq(current_time, length.out = length(cur_row:nrow(tmp2)), by = delta_time*delta_index)
			tmp2$TimeStamp[1:cur_row] <- rev(seq(current_time, length.out = length(1:cur_row), by = -delta_time*delta_index))
			print(str(tmp2))

			step <- nrow(tmp2)/4
			break_pos <- c()
			labels <- c()
			for ( i in 1:5 )
			{
				k = (i-1)*step+1
				if ( k > nrow(tmp2))
				{
					k = nrow(tmp2)
				}
				break_pos <- c(break_pos, tmp2$time_index[k])
				labels <- c(labels, as.character(tmp2$TimeStamp[k]))
			}
			looked_var_plt <- looked_var_plt + scale_x_continuous(breaks = break_pos, labels = labels)
			plt_s[[1]] <- plt_s[[1]] + scale_x_continuous(breaks = break_pos, labels = labels)
			plt_s[[2]] <- plt_s[[2]] + scale_x_continuous(breaks = break_pos, labels = labels)
			plt_s[[3]] <- plt_s[[3]] + scale_x_continuous(breaks = break_pos, labels = labels)
			#tmp2 <- NULL
			#rm(tmp2)
			#freeram()
#/////////////////////////////////////////////
		}
				
		if ( is.null(tracking_feature))
		{
			if ( !is.null(rul_hist_plt) )
			{
				layout1 <- rbind(	c(6, 6, 1),
									c(2, 2, 1),
				                 	c(3, 4, 5))
				plt0 <- plt0 + 
				theme(axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank())
				plt <- gridExtra::grid.arrange(plt0, looked_var_plt, plt_s[[1]], plt_s[[2]], plt_s[[3]],rul_hist_plt, 
				layout_matrix = layout1, top = current_time, heights=c(0.5,2,1))
			}else
			{
				#https://id.fnshr.info/2016/10/10/gridextra/
				layout1 <- rbind(c(2, 2, 1),
				                 c(3, 4, 5))
				plt0 <- plt0 + 
				theme(axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank())
				plt <- gridExtra::grid.arrange(plt0, looked_var_plt, plt_s[[1]], plt_s[[2]], plt_s[[3]], layout_matrix = layout1, top = current_time)
			}
		}else
		{
			if ( !is.null(rul_hist_plt) )
			{
				layout1 <- rbind(	c(5, 5, 5),
									c(1, 1, 1),
				                 	c(2, 3, 4))
				plt <- gridExtra::grid.arrange( looked_var_plt, plt_s[[1]], plt_s[[2]], plt_s[[3]],rul_hist_plt, 
				layout_matrix = layout1, top = current_time, heights=c(1,2,1))
			}else
			{
				layout1 <- rbind(c(1, 1, 1),
				                 c(2, 3, 4))
				plt <- gridExtra::grid.arrange( looked_var_plt, plt_s[[1]], plt_s[[2]], plt_s[[3]], layout_matrix = layout1, top = current_time)
			}
		}
		ggsave(file = paste(putpng_path, result_png, sep=""), plot = plt, dpi = 130, width = 14*1.2, height = 6.8*1.0)
		rm(plt)
		freeram()
		
		print(feature_param)
		#i = i + 1
		#print(i)
	}
	#sink()
}

time_out <- 60*3
get_csvfile <- function()
{
	cmdstr='cmd /c dir '
	cmdstr=paste(cmdstr, "\"", getwd(), sep="")
	cmdstr=paste(cmdstr,"\\Untreated\\*.csv\" /b /od", sep="")

	start <- Sys.time()

	files <- NULL
	while( TRUE )
	{
		tryCatch({
				files = system(cmdstr, intern=T)
				print(files)
				if ( files[1] != "ファイルが見つかりません")
				{
					break
				}
			},error = function(e)
			{
				files <- NULL
			},finally = { 
	    	},silent = TRUE
    	)
		end <- Sys.time()
		diff <- as.numeric(end - start)
		if ( diff > time_out )
		{
			print("time out!!")
			return (NULL)
		}
	}
	
	return (files)
}

delete_csvfile <- function(i)
{
	print(files[i])
	file.copy(paste("Untreated\\",files[i],sep=""), "Processed")
	file.remove(paste("Untreated\\",files[i],sep=""))
}


appedAll_csv <- function( dir, outfile )
{
	curdir <- getwd()

	setwd(dir)
	getwd()

	csv_list <- list.files(pattern = "*.csv")
	cat("csv_list")
	print(csv_list)

	df <- do.call(rbind, lapply(csv_list, function(x) read.csv(x, header=TRUE, stringsAsFactors = FALSE,fileEncoding =csv_encoding)))
	#df <- do.call(rbind, lapply(csv_list, function(x) fread(x, na.strings=c("", "NULL"), header = TRUE, stringsAsFactors = TRUE)))
	
	#df <- do.call(dplyr::bind_rows, lapply(csv_list, function(x) fread(x, na.strings=c("", "NULL"), header = TRUE, stringsAsFactors = TRUE)))
	df <- as.data.frame(df)


	try(write.csv(df, outfile, row.names = F), silent = FALSE)
	
	setwd(curdir)

	return(df)
}
#appedAll_csv(dir='./vibration_data', outfile ='vibration_data_all.csv')


