options(encoding = "utf-8")
options(digits.secs=3)
library(ggplot2)
library(gridExtra)
library(zoo)
library(scales)

library(energy)
library(minerva)

library(lightgbm)
library(ggrepel)

#dataset <- NULL


#dataset<-NULL

distance_covariance_high_corr_sample_max <- 1000
maximal_information_coefficient_high_corr_sample_max <- 1000

distance_covariance_sample_max <- 100
maximal_information_coefficient_sample_max <- 100

detection_precursor_phenomena_train_sample_max <- 2500

# install.packages("zoo")
library(zoo)


if( file.exists("./src/SpeedMeter.r"))
{
	source("./src/SpeedMeter.r")
}
if( file.exists("../src/SpeedMeter.r"))
{
	source("../src/SpeedMeter.r")
}

moving_mean_smooth2 <- function(df, timeStamp, window_size, slide) 
{
	print("========== moving_mean_smooth start ===============")
	if ( window_size <= slide )
	{
		print("Error window_size <= slide")
		return(NULL)
	}
	
	print(str(df))
	print(timeStamp)
	print(window_size)
	time_index_sv <- NULL
	timestamp_sv <- NULL
	maintenance_sv <- NULL
	
	if (  length(grep("time_index", colnames(df))) > 0)
	{
		time_index_sv <- df$time_index
		df$time_index <- NULL
	}
	if (  length(grep(timeStamp, colnames(df))) > 0)
	{
		timestamp_sv <- df[,timeStamp]
		df[,timeStamp] <- NULL
	}
	if (  length(grep("maintenance", colnames(df))) > 0)
	{
		maintenance_sv <- df$maintenance
		df$maintenance <- NULL
	}

	smoothed_df <- as.data.frame(lapply(df, function(col) {
		if (is.numeric(col)) {
			if (  length(col) == length(unique(col)))
			{
				rollapply(col, width = window_size, by = slide, FUN = function(x) tail(x, 1), fill = NA, align = "right")
			}else
			{
				rollapply(col, width = window_size, by = slide, FUN =  function(x) mean(x, na.rm = TRUE), fill = NA, align = "right")
				#rollapply(col, width = window_size, by = slide, FUN = function(x) weighted.mean(x, w = seq_along(x)), fill = NA, align = "right")
				
				#lambda <- 0.1
				#rollapply(col, width = window_size, by = slide, FUN = function(x) {
                #               weights <- exp(lambda * seq_along(x))
                #               weighted.mean(x, w = weights)
                #             }, fill = NA, align = "right")

			}
		} else {
			rollapply(col, width = window_size, by = slide, FUN = function(x) tail(x, 1), fill = NA, align = "right")
		}
	}))
	
	smoothed_df <- smoothed_df[rowSums(!is.na(smoothed_df)) > 0,, drop=F ]
	cat("smoothed_df")
	print(str(smoothed_df))
	
	n <- nrow(df)
	time_index2 <- rep(NA, n)
	timestamp2 <- rep(NA, n)
	maintenance2 <- rep(NA,n)
	
	nn <- nrow(smoothed_df)
	#cat("nn")
	#print(nn)
	
	if (!is.null(timestamp_sv))
	{
		timestamp_sv <- as.POSIXct(timestamp_sv, tz="UTC", origin="1970-01-01")
	}
	
	j <- window_size 
	for (i in 1:nn) 
	{
		if (!is.null(time_index_sv))
		{
			time_index2[i] <- time_index_sv[j]
		}
		if (!is.null(timestamp_sv))
		{
			timestamp2[i] <- timestamp_sv[j]
		}
		if (!is.null(maintenance_sv))
		{
			if ( any(maintenance_sv[(j-slide):j]) == 1 )
			{
				maintenance2[i] <- 1
			}else
			{
				maintenance2[i] <- 0
			}
		}
		j <- j + slide
	}

	if (!is.null(time_index_sv))
	{
		smoothed_df$time_index <- time_index2[1:nn]
	}
	if (!is.null(timestamp_sv))
	{
		smoothed_df[,timeStamp] <- as.POSIXct(timestamp2[1:nn], tz="UTC", origin="1970-01-01")
	}
	if (!is.null(maintenance_sv))
	{
		smoothed_df$maintenance <- maintenance2[1:nn]
	}

	print(str(smoothed_df))
	print("========== moving_mean_smooth end ===============")
	if ( nn < 10 )
	{
		print("Error Smoothing parameters are too large")
		return(NULL)
	}
	return(smoothed_df)
}


moving_mean_smooth <- function(df, timeStamp, window_size) 
{
	print("========== moving_mean_smooth start ===============")
	print(str(df))
	print(timeStamp)
	print(window_size)
	time_index_sv <- NULL
	timestamp_sv <- NULL
	maintenance_sv <- NULL
	
	if (  length(grep("time_index", colnames(df))) > 0)
	{
		time_index_sv <- df$time_index
		df$time_index <- NULL
	}
	if (  length(grep(timeStamp, colnames(df))) > 0)
	{
		timestamp_sv <- df[,timeStamp]
		df[,timeStamp] <- NULL
	}
	if (  length(grep("maintenance", colnames(df))) > 0)
	{
		maintenance_sv <- df$maintenance
		df$maintenance <- NULL
	}

	smoothed_df <- as.data.frame(lapply(df, function(col) {
		if (is.numeric(col)) {
			if (  length(col) == length(unique(col)))
			{
				rollapply(col, width = window_size, FUN = function(x) tail(x, 1), fill = NA, align = "right")
			}else
			{
				rollapply(col, width = window_size, FUN = mean, fill = NA, align = "right")
				#rollapply(col, width = window_size, FUN = function(x) weighted.mean(x, w = seq_along(x)), fill = NA, align = "right")
				
				#lambda <- 0.1
				#rollapply(col, width = window_size, FUN = function(x) {
                #               weights <- exp(lambda * seq_along(x))
                #               weighted.mean(x, w = weights)
                #             }, fill = NA, align = "right")

			}
		} else {
			rollapply(col, width = window_size, FUN = function(x) tail(x, 1), fill = NA, align = "right")
		}
	}))
	cat("smoothed_df")
	print(str(smoothed_df))
	
	n <- nrow(df)
	time_index2 <- rep(NA, n)
	timestamp2 <- rep(NA, n)
	maintenance2 <- rep(NA,n)
	
	for (i in window_size:n) {
	
		if (!is.null(time_index_sv))
		{
			time_index2[i] <- time_index_sv[i]
		}
		if (!is.null(timestamp_sv))
		{
			timestamp2[i] <- timestamp_sv[i]
		}
		if (!is.null(maintenance_sv))
		{
			maintenance2[i] <- maintenance_sv[i]
		}
	}
	if (!is.null(time_index_sv))
	{
		smoothed_df$time_index <- time_index2
	}
	if (!is.null(timestamp_sv))
	{
		smoothed_df[,timeStamp] <- as.POSIXct(timestamp2, tz="UTC", origin="1970-01-01")
	}
	if (!is.null(maintenance_sv))
	{
		smoothed_df$maintenance <- maintenance2
	}

	smoothed_df <- smoothed_df[(window_size+1):nrow(smoothed_df),]
	print(str(smoothed_df))
	print("========== moving_mean_smooth end ===============")
	return(smoothed_df)
}


get_corr <- function(df, x, y, method="spearman") 
{
	n <- length(x)
	if ( method == "spearman" )
	{
		corr_values[i] <- cor(x, y)
	}
	if ( method == "dcor" )
	{
		sample_indices <- sample(1:n, min(distance_covariance_sample_max, n))
		X_sample <- x[sample_indices]
		Y_sample <- x[sample_indices]		
		corr_values[i] <- dcor(X_sample, Y_sample)
	}
	if ( method == "MIC" )
	{
		sample_indices <- sample(1:n, min(maximal_information_coefficient_sample_max, n))
		X_sample <- x[sample_indices]
		Y_sample <- y[sample_indices]		
		corr_values[i] <- mine(X_sample, Y_sample)$MIC
	}			
	return(corr_values)
}

#About 68% of all data fall within the mean ± 1 × standard deviation (±1σ)
#About 95% of the data fall within the mean ± 2 × standard deviation (±2σ)
#About 99.7% of the data fall within the mean ± 3 × standard deviation

get_high_corr_pairs <- function(df, corr_threshold, method="spearman")
{
	print("========== get_high_corr_pairs start ================")
	
	#cat("ncol(df)")
	#print(ncol(df))
	df$time_index <- NULL

	n <- ncol(df)
	initial_corr <- matrix(, nrow = n, ncol = n)

	if ( method == "spearman" )
	{
		#initial_corr <- cor(df, method = "spearman")*0
		for (i in 1:(n-1)) {
		  for (j in (i+1):n) {
		    initial_corr[i,j] <- cor(df[[i]], df[[j]])
		  }
		}
	}
	
	if ( method == "dcor" )
	{
		#initial_corr <- cor(df, method = "spearman")
		
		n_large <- nrow(df)
		
		# Compute matrix of distance correlations
		sample_indices <- sample(1:n_large, min(distance_covariance_high_corr_sample_max, n_large))
		for (i in 1:(n-1)) {
		  for (j in (i+1):n) {
			x_ <- df[[i]]
			y_ <- df[[j]]
			X_sample <- x_[sample_indices]
			Y_sample <- y_[sample_indices]		
		  
		    initial_corr[i,j] <- dcor(X_sample, Y_sample)
		    cat(sprintf("              \r%d/%d %d/%d", i,(n-1),j,n))
		  }
		}
	}
	if ( method == "MIC" )
	{
		#initial_corr <- cor(df, method = "spearman")
		
		n_large <- nrow(df)
		
		# Compute matrix of distance correlations
		sample_indices <- sample(1:n_large, min(maximal_information_coefficient_high_corr_sample_max, n_large))
		for (i in 1:(n-1)) {
		  for (j in (i+1):n) {
			x_ <- df[[i]]
			y_ <- df[[j]]
			X_sample <- x_[sample_indices]
			Y_sample <- y_[sample_indices]		
		  
		    initial_corr[i,j] <- mine(X_sample, Y_sample)$MIC
		    cat(sprintf("              \r%d/%d %d/%d", i,(n-1),j,n))
		  }
		}
	}
	#cat("corr_threshold")
	#print(corr_threshold)
	cat("initial_corr")
	print(initial_corr)

	# Extract only pairs with high absolute correlation coefficients (e.g., 0.7 or higher)
	high_corr_pairs <- which(abs(initial_corr) > corr_threshold & abs(initial_corr) < 0.85 & upper.tri(initial_corr), arr.ind = TRUE)
	
	#cat("high_corr_pairs")
	#print(high_corr_pairs)
	print("========== get_high_corr_pairs end ================")

	return (list(high_corr_pairs, initial_corr))
}


Only_required_fields <- function(sensor_data, timestamp)
{
	print("======= Only_required_fields start ==========")
	cat("input sensor_data")
	print(str(sensor_data))
	
	time_stamp_sv <- NULL
	maintenance_sv <- NULL
	time_index_sv <- NULL
	#print(colnames(sensor_data))
	if (  length(grep("maintenance", colnames(sensor_data))) > 0)
	{
		maintenance_sv <- sensor_data$maintenance
		sensor_data$maintenance <- NULL
	}
	if (  length(grep("time_index", colnames(sensor_data))) > 0)
	{
		time_index_sv <- sensor_data$time_index
		sensor_data$time_index <- NULL
	}
	if ( !is.null(timestamp) && timestamp != "")
	{
		#print("timestamp")
		#print(timestamp)
		if (  length(grep(timestamp, colnames(sensor_data))) > 0)
		{
			time_stamp_sv <- sensor_data[,timestamp]
			sensor_data[,timestamp] <- NULL
		}
	}
	
	total_length <- nrow(sensor_data)
	sensor_data <- sensor_data[, sapply(sensor_data, function(x) is.numeric(x) != 0), drop=FALSE]
	cat("(1)")
	print(str(sensor_data))
	sensor_data <- sensor_data[, sapply(sensor_data, function(x) sd(x, na.rm = TRUE) != 0), drop=FALSE]
	cat("(2)")
	print(str(sensor_data))
	if ( ncol(sensor_data) > 2 )
	{
		sensor_data <- sensor_data[, sapply(sensor_data, function(x) length(unique(x)) > 0.1*total_length), drop=FALSE]
		cat("(3)")
		print(str(sensor_data))
	}
	
	# Exclude index columns (integer columns increasing consecutively by 1)
	is_index_col <- function(x) {
	  all(diff(x) == 1) && is.integer(x)
	}

	# Detect and exclude index-like columns
	#sensor_data <- sensor_data[, !sapply(sensor_data, is_index_col)]
	sensor_data <- sensor_data[, !sapply(sensor_data, function(x) is_index_col(x)), drop=FALSE]
	
	is_index_n_col <- function(x) {
	  all(diff(x) == 1) && is.integer(x)
	}

	remove_index_columns <- function(df, tol = .Machine$double.eps^0.5) {
	  is_index <- sapply(df, function(x) {
	    if (is.numeric(x) && length(x) > 1) {
	      d <- diff(x)
	      return((max(d) - min(d)) < tol)
	    } else {
	      return(FALSE)
	    }
	  })
	  df[ , !is_index, drop = FALSE]
	}
	sensor_data <- remove_index_columns(sensor_data)
	cat("(4)")
	print(str(sensor_data))
	
	if ( !is.null(time_index_sv))
	{
		sensor_data$time_index <- time_index_sv
	}
	if ( !is.null(maintenance_sv))
	{
		sensor_data$maintenance <- maintenance_sv
	}
	if ( !is.null(time_stamp_sv))
	{
		sensor_data[,timestamp] <- time_stamp_sv
	}
	
	cat("output sensor_data")
	print(str(sensor_data))
	print("======= Only_required_fields end ==========")
	
	return(sensor_data)
}

smape__ <- function(actual, predicted) {
  return(mean(2 * abs(predicted - actual) / (abs(actual) + abs(predicted))) * 100)
}

make_lgbmodel <- function(initial_data, selected_pairs, idx)
{
	sensor_X <- selected_pairs$sensor_X[idx]
	sensor_Y <- selected_pairs$sensor_Y[idx]

	data <- data.frame(X=initial_data[[sensor_X]], Y=initial_data[[sensor_Y]])
	train_row <- as.integer(nrow(data)*0.7+0.5)
	train_data <- data[1:train_row,]
	valid_data <- data[-train_row, ]
	
	dir1 = 1
	dtrain <- lgb.Dataset(data = as.matrix(train_data[, "X", drop = FALSE]),
	                      label = train_data$Y)
	dvalid <- lgb.Dataset(data = as.matrix(valid_data[, "X", drop = FALSE]),
	                      label = valid_data$Y)
              
	#nrounds = 100〜500
	#learning_rate = 0.05〜0.1
	#max_depth = 3〜6
	
	params <- list(
	  objective = "regression",
	  boosting = "gbdt",
	  learning_rate = 0.05,
	  num_leaves = 31,
	  max_depth = -1,
	  min_data_in_leaf = 20,
	  feature_fraction = 0.8,
	  bagging_freq = 5,
	  min_child_samples = 100,
	  verbose = -1,
	  #metric = "rmse"
	  #metric = "l1"
	  metric = "mape"
	)

	model1 <- lgb.train(
	  params = params,
	  data = dtrain,
	  nrounds = 100,
	  valids = list(validation = dvalid),
	  early_stopping_rounds = 20,
	  verbose = -1
	)
	pred <- predict(model1, newdata = as.matrix(valid_data[, "X", drop = FALSE]))
	r1 <- smape__(valid_data$Y, pred)
	err1 <- pred - valid_data$Y
	
	
	dir2 = -1
	dtrain <- lgb.Dataset(data = as.matrix(train_data[, "Y", drop = FALSE]),
	                      label = train_data$X)
	dvalid <- lgb.Dataset(data = as.matrix(valid_data[, "Y", drop = FALSE]),
	                      label = valid_data$X)
	model2 <- lgb.train(
	  params = params,
	  data = dtrain,
	  nrounds = 100,
	  valids = list(validation = dvalid),
	  early_stopping_rounds = 20,
	  verbose = -1
	)
	pred <- predict(model2, newdata = as.matrix(valid_data[, "Y", drop = FALSE]))
	r2 <- smape__(valid_data$X, pred)
	err2 <- pred - valid_data$X
	
	if ( r1 < r2 )
	{
		print(sprintf("%s -> %s error rate:%f %% < %f %%", sensor_X, sensor_Y, r1, r2))
		return( list(model1, r1, err1, dir1, idx))
	}
	print(sprintf("%s <- %s error rate:%f %% < %f %%", sensor_X, sensor_Y, r2, r1))
	return( list(model2, r2, err2, dir2, idx))
}

predict_lgbmodel <- function(model_list, initial_data, selected_pairs, idx)
{
	sensor_X <- selected_pairs$sensor_X[idx]
	sensor_Y <- selected_pairs$sensor_Y[idx]

	model <- model_list[[idx]][[1]]
	dir <- model_list[[idx]][[4]]
	
	r <- 0
	err <- 0

	valid_data <- NULL
	tryCatch(
		{
			valid_data <- data.frame(X=initial_data[[sensor_X]], Y=initial_data[[sensor_Y]])
		},
		error = function(e)
		{
			#print(e)
			return(NULL)
		},
		silent = TRUE
	)
	#cat("valid_data")
	#print(str(valid_data))
	if (is.null(valid_data))
	{
		return (NULL)
	}
		
	if ( dir > 0 )
	{
		dir1 = 1
		dvalid <- lgb.Dataset(data = as.matrix(valid_data[, "X", drop = FALSE]),
		                      label = valid_data$Y)
              
		pred <- predict(model, newdata = as.matrix(valid_data[, "X", drop = FALSE]))
		err <- pred - valid_data$Y
	}else
	{
		dvalid <- lgb.Dataset(data = as.matrix(valid_data[, "Y", drop = FALSE]),
		                      label = valid_data$X)

		pred <- predict(model, newdata = as.matrix(valid_data[, "Y", drop = FALSE]))
		err <- pred - valid_data$X
	}
	
	return(err)
}

Detection_precursor_phenomena_train <- function(df, target_colnames, timeStamp, percent=c(0.80, 0.95, 0.99), window_size = 30, slide = 1, corr_threshold=0.38, scorTopN=6, method="spearman")
{
	print("========= Detection_precursor_phenomena_train start ========")
	#print(str(df))
	
	cat("target_colnames")
	print(target_colnames)
	maintenance_sv <- NULL
	time_index_sv <- NULL
	
	if (  length(grep("time_index", colnames(df))) > 0)
	{
		time_index_sv <- df$time_index
	}
	if (  length(grep("maintenance", colnames(df))) > 0)
	{
		maintenance_sv <- df$maintenance
	}
	if (  length(grep(timeStamp, colnames(df))) > 0)
	{
		df[,timeStamp] <- NULL
	}
	existing_columns <- intersect(target_colnames, colnames(df))
	sensor_data <- df[, existing_columns, drop = FALSE]

	total_length <- nrow(sensor_data)

	#cat("total_length")
	#print(total_length)

	rate = 0.70
	if ( total_length * rate > 100 )
	{
		# rate% of the first half of the data is used as reference data
		split_point <- floor(total_length * rate)
	}else
	{
		split_point <- total_length
	}

	# Standard data for the first rate%.
	initial_data <- sensor_data[1:split_point,, drop = FALSE ]
	
	time_index_sv <- time_index_sv[1:split_point]
	if ( !is.null(maintenance_sv))
	{
		maintenance_sv <- maintenance_sv[1:split_point]
	}
	
	#cat("time_index")
	#print(time_index_sv)
	
	if ( !is.null(maintenance_sv))
	{
		initial_data$maintenance <- NULL
	}
	initial_data$time_index <- NULL
	cat("initial_data")
	print(str(initial_data))

	sigma <- qnorm( 1 - (1 - percent) / 2 )
	cat("sigma:")
	print(sigma)

	selected_pairs <- NULL
	mean_base <- 0
	sd_base   <- 0
	median_base <- 0
	mad_base <- 0
	zscore_base <- NULL
	zscore_base_mean <- 0
	zscore_base_sd <- 0
	
	cat("ncol(initial_data)")
	print(ncol(initial_data))
	if ( ncol(initial_data) == 1)
	{
		mean_base <- mean(initial_data[,1], na.rm=TRUE)
		sd_base   <- sd(initial_data[,1], na.rm=TRUE)
		median_base <- median(initial_data[,1], na.rm=TRUE)
		mad_base <- median(abs(initial_data[,1] - median_base), na.rm = TRUE)
		
		zscore_base <- abs(initial_data[,1] - mean_base)/sd_base
		zscore_base_mean <- mean(zscore_base, na.rm=TRUE)
		zscore_base_sd <- sd(zscore_base, na.rm=TRUE)
		
		if ( abs(mad_base) < 1.0e-10 )
		{
			mad_base <- 1.0e-10
		}
		if ( abs(sd_base) < 1.0e-10 )
		{
			sd_base <- 1.0e-10
		}
		y <- zscore_base_mean + max(sigma)*zscore_base_sd

		cat("sum(abs(zscore_base) > y)/length(zscore_base)")
		print(sum(abs(zscore_base) > y)/length(zscore_base))
		print(sum(abs(zscore_base) > y))
		
		print(sprintf("mean_base:%f sd_base:%f median_base:%f mad_base:%f",
		mean_base, sd_base, median_base, mad_base))
	}
	prm = c(mean_base, sd_base, median_base, mad_base,  
			zscore_base_mean, zscore_base_sd)
	prm_list <- list()
	prm_list[[1]] <- prm
	prm_list[[2]] <- zscore_base
	if (is.null(zscore_base))
	{
		prm_list[2] <- list(zscore_base)
	}
	
	#cat("ncol(initial_data)")
	#print(ncol(initial_data))
	if ( ncol(initial_data)> 1)
	{
		high_corr_pairs_lst <- try(get_high_corr_pairs(initial_data, corr_threshold, method), silent=T)
		if ( is.null(high_corr_pairs_lst))
		{
			print(sprintf("Error:corr() error"))
			return (NULL)
		}
		high_corr_pairs <- high_corr_pairs_lst[[1]]
		initial_corr <- high_corr_pairs_lst[[2]]
		
		#cat("#high_corr_pairs")
		#print(high_corr_pairs)
		#cat("#initial_corr")
		#print(initial_corr)
		if ( nrow(high_corr_pairs) == 0 )
		{
			print(sprintf("Error:Too big corr_threshold:%f", corr_threshold))
			return (NULL)
		}
		
		#print(colnames(initial_data))
		# Organize extraction results in a data frame
		selected_pairs <- data.frame(
		  sensor_X = colnames(initial_data)[high_corr_pairs[,1]],
		  sensor_Y = colnames(initial_data)[high_corr_pairs[,2]],
		  correlation = initial_corr[high_corr_pairs]
		)
		if ( nrow(selected_pairs) == 0 )
		{
			print(sprintf("Error:Too big corr_threshold:%f", corr_threshold))
			return (NULL)
		}

		#print(selected_pairs)
		#Top
		selected_pairs <- selected_pairs[order(-abs(selected_pairs$correlation)), ][1:min(nrow(selected_pairs),scorTopN), ]
	}
	print(selected_pairs)
	
	if ( is.null(selected_pairs) )
	{
		print("========= Detection_precursor_phenomena_train end ========")
		return (list(NULL, prm_list, initial_data, NULL, target_colnames))
	}
	model_list <- list()
	if ( !is.null(selected_pairs) && nrow(selected_pairs) >= 1 )
	{
		for ( i in 1:nrow(selected_pairs))
		{
			model_list[[i]] <- make_lgbmodel(initial_data, selected_pairs, i)
		}
	}
	
	if ( !is.null(maintenance_sv))
	{
		initial_data$maintenance <- maintenance_sv
	}
	initial_data$time_index <- time_index_sv
	#cat("initial_data$time_index")
	#print(initial_data$time_index)
	
	#Sort by fitting accuracy
	if ( F )
	{
		model_list <- model_list[order(sapply(model_list, function(x) x[[2]]))]
		selected_pairs_sv <- selected_pairs
		for ( i in 1:nrow(selected_pairs))
		{
			#print(model_list[[i]][[2]])
			#print(model_list[[i]][[5]])
			#print(selected_pairs_sv[model_list[[i]][[5]],1])
			#print(selected_pairs_sv[model_list[[i]][[5]],2])
			
			selected_pairs[i,] <- selected_pairs_sv[model_list[[i]][[5]],]
		}
	}
	print("========= Detection_precursor_phenomena_train end ========")
	
	return (list(selected_pairs, prm_list, initial_data, model_list, target_colnames))
}

probability_loess <- function(df, maintenance_sv)
{
	if ( nrow(df) < 100)
	{
		return(df)
	}
	min <- min(df$posterior_abnormal)
	max <- max(df$posterior_abnormal)
	
	rescale <- F
	if ( max - min > 1.0e-10 )
	{
		rescale <- T
		df$posterior_abnormal <- exp(10*df$posterior_abnormal + 1)
	}
	
	loess <- predict(loess(posterior_abnormal ~ time, data = df, span = 0.4))

	if ( rescale )
	{
		#loess <- log(loess) - 1
		loess <- max*loess/max(loess)
	}
	
	loess <- ifelse(loess < 0, 0, loess)
	loess <- ifelse(loess >= 1, 0.999, loess)
	
	count_start <- 1
	if( !is.null(maintenance_sv))
	{
		for ( k in 1:length(loess))
		{
			if ( maintenance_sv[k] == 1 )
			{
				#loess[k] = 0
				count_start <- k
			}
		}
	}
	
	df$posterior_abnormal <- loess
	
	return (df)
}


posterior_probabilities_Bayes <- function(x, mean_nromal, sd_normal, mean_abnormal, sd_abnormal, prior_normal, prior_abnormal)
{
	likelihood_normal <- dnorm(x, mean = mean_nromal, sd = sd_normal)
	likelihood_abnormal <- dnorm(x, mean = mean_abnormal, sd = sd_abnormal)


	if ( abs(likelihood_normal) < 1.0e-10 &&  abs(likelihood_abnormal) < 1.0e-10 )
	{
		likelihood_normal <- 1.0e-10
		likelihood_abnormal <- 1.0e-10
	}
	
	# Calculating posterior probabilities based on Bayes' theorem
	w <- (likelihood_normal * prior_normal + likelihood_abnormal * prior_abnormal)
	if ( abs(w) < 1.0e-12 )
	{
		w <- 1.0e-12
	}
	posterior_abnormal <- (likelihood_abnormal * prior_abnormal) / w
	
	if ( posterior_abnormal > 1.0 )
	{
		posterior_abnormal <- 1.0
	}
	
	return( posterior_abnormal)
}


Detection_precursor_phenomena_test <- function(df, timeStamp, dpp_model, percent=c(0.80, 0.95, 0.99), window_size = 30, slide = 1, method="spearman")
{
	print("====== Detection_precursor_phenomena_test start =======")

	
	#cat("nrow(sensor_data) ")
	#print(nrow(sensor_data))
	sigma <- qnorm( 1 - (1 - percent) / 2 )
	cat("sigma:")
	print(sigma)
	
	selected_pairs <- dpp_model[[1]]
	mean_base <- dpp_model[[2]][[1]][1]
	sd_base <- dpp_model[[2]][[1]][2]
	median_base<- dpp_model[[2]][[1]][3]
	mad_base <- dpp_model[[2]][[1]][4]
	zscore_base <- dpp_model[[2]][[2]]
	zscore_base_mean <- dpp_model[[2]][[1]][5]
	zscore_base_sd <- dpp_model[[2]][[1]][6]
	

	train_data <- dpp_model[[3]]
	model_list <- dpp_model[[4]]
	
	target_colnames <- dpp_model[[5]]

	maintenance_sv <- NULL
	time_index_sv <- NULL
	
	posterior_abnormal <- NULL
	
	if (  length(grep("time_index", colnames(df))) > 0)
	{
		time_index_sv <- df$time_index
	}
	if (  length(grep("maintenance", colnames(df))) > 0)
	{
		maintenance_sv <- df$maintenance
	}
	existing_columns <- intersect(target_colnames, colnames(df))
	sensor_data <- df[, existing_columns, drop = FALSE]


	if ( ncol(sensor_data) == 1 )
	{
		print(sprintf("mean_base:%f sd_base:%f median_base:%f mad_base:%f",
		mean_base, sd_base, median_base, mad_base))
	}
	
	if (!is.null(maintenance_sv))
	{
		sensor_data$maintenance <- maintenance_sv
	}
	sensor_data$time_index <- time_index_sv

	cat("selected_pairs:")
	print(selected_pairs)
	
	anomaly_cunt <- 0
	anomaly_max <- 0

	posterior_abnormal_list <- list()
	plots <- list()
	num_plt <- 0
	if ( !is.null(selected_pairs) && nrow(selected_pairs) >= 1 )
	{

		for ( i in 1:nrow(selected_pairs))
		{
			sensor_X <- selected_pairs$sensor_X[i]
			sensor_Y <- selected_pairs$sensor_Y[i]

			model_dir <- model_list[[i]][[4]]
			model_err <- model_list[[i]][[3]]
			model_errrate <- model_list[[i]][[2]]
			mean_anomaly <- mean(model_err, na.rm=TRUE)

			Y = sensor_Y
			if ( model_dir < 0 )
			{
				Y = sensor_X
			}
			pred_residuals_err <- predict_lgbmodel(model_list, sensor_data, selected_pairs, i)
			if ( is.null(pred_residuals_err))
			{
				next
			}

			###### Bayesian estimation
			
			# Setting the prior probability
			prior_normal <- 0.999
			prior_abnormal <- (1 - prior_normal)
						
			mean_nromal <- mean(model_err,na.rm=T)
			sd_normal <- sd(model_err,na.rm=T)
			 
			mean_abnormal <- mean_nromal + 1.2*sd_normal
			sd_abnormal <- 1.2*sd_normal

			posterior_abnormal <- pred_residuals_err*0
			for ( k in 1:length(pred_residuals_err))
			{
				x <- pred_residuals_err[k]
				
				posterior_abnormal[k] <- posterior_probabilities_Bayes(x, mean_nromal, sd_normal, mean_abnormal, sd_abnormal, prior_normal, prior_abnormal)

				prior_abnormal <- posterior_abnormal[k]
				prior_normal <- (1 - posterior_abnormal[k])
			}
			
			# pre_anomaly_d
			#
			#Discrepancy between predictions and observed values 
			#(the average of the discrepancy between predictions and observed values under normal conditions is set to 0)
			
			result_df <- data.frame(
			  time = sensor_data$time_index,
			  Y = sensor_data[[Y]],
			  pre_anomaly_d = (pred_residuals_err),
			  posterior_abnormal = posterior_abnormal^0.2
			)
			if (!is.null(maintenance_sv))
			{
				result_df$maintenance <- sensor_data$maintenance
			}
			
			result_df <- probability_loess(result_df, maintenance_sv)
			#result_df <- moving_mean_smooth2(result_df, timeStamp, 4, 1) 

			delta_ix <- result_df$time[nrow(result_df)]- result_df$time[nrow(result_df)-1]
			result_df$TimeStamp <- rev(seq(current_time, length.out = length(1:nrow(result_df)), by = -delta_time*delta_ix))
			result_df$TimeStamp <- as.POSIXct(result_df$TimeStamp, tz='UTC', origin="1970-01-01")
			
			threshold_upper <- c(0.3, 0.6, 0.9)
			
			
			mean_res <- mean(model_err)
			sd_res <- sd(model_err)
			p_values <- dnorm(result_df$pre_anomaly_d, mean = mean_res, sd = sd_res)
			result_df$pre_anomaly_probability <-  1 - p_values

			
			ecdf_normal <- ecdf(model_err)
			result_df$pre_anomaly_probability <-  1 - ecdf_normal(result_df$pre_anomaly_d)

			
			# Set the judgment flag
			result_df$pre_anomaly_probability1 <- (result_df$posterior_abnormal <= threshold_upper[1] & result_df$posterior_abnormal > 0)

			anomaly_cunt <- sum(result_df$pre_anomaly1)
			anomaly_max <- max(abs(result_df$pre_anomaly_d - threshold_upper[1]), na.rm = TRUE)
			
			result_df$pre_anomaly_probability2 <- (result_df$posterior_abnormal > 10000000)
			result_df$pre_anomaly_probability3 <- (result_df$posterior_abnormal > 10000000)
			if ( length(percent) > 1 )
			{
				result_df$pre_anomaly_probability2 <- (result_df$posterior_abnormal > threshold_upper[1] & result_df$posterior_abnormal <= threshold_upper[2])
			}
			if ( length(percent) > 2 )
			{
				result_df$pre_anomaly_probability3 <- (result_df$posterior_abnormal > threshold_upper[2] & result_df$posterior_abnormal <= threshold_upper[3])
				result_df$pre_anomaly_probability4 <- (result_df$posterior_abnormal > threshold_upper[3] )
			}
			
			
			#print("result_df")
			#print(str(result_df))
			#quit()
			
			if ( !is.null(result_df$pre_anomaly_probability1))
			{			
				breaks_vec <- seq(min(result_df$TimeStamp), max(result_df$TimeStamp), length.out = 5)
				
				#result_df$plotY <- result_df$pre_anomaly_probability
				result_df$plotY <- result_df$posterior_abnormal
				#result_df$plotY <- result_df$Y
								
				errR <-  model_errrate/100
				alp <- 1.0
				if ( errR > 0.1 ) alp <- 0.5
				if ( errR > 0.3 ) alp <- 0.2
				if ( errR > 0.5 ) alp <- 0.1
				 
				#Plot the error against the upper and lower statistical thresholds for the error.
				#Plot the points where the error probability exceeds the error probability
				# thresholds with the points overlaid with a color according to the probability.

				#Transparency is modified according to the accuracy of the model.
				#More reliable models are plotted as opaque, 
				#while less reliable models are plotted with higher transparency.
				
				plt <- ggplot(result_df, aes(x = TimeStamp, y = plotY)) +
				  geom_line(color = "steelblue", linewidth = 0.8, alpha = alp) +
				  geom_hline(yintercept = (threshold_upper), color = "red", linetype = "dashed")
				  #geom_hline(yintercept = min(cor_threshold_lower), color = "red", linetype = "dashed")
				  #geom_hline(yintercept = 0, color = "orange", linetype = "dashed")
				  #geom_hline(yintercept = c(0,1), color = "red", linetype = "dashed")
				  
				  if (  length(grep("maintenance", colnames(result_df))) > 0)
				  {
				  	plt <- plt +  geom_vline(data = result_df[result_df$maintenance == 1, ],
				  			aes(xintercept = TimeStamp), color = "green", linetype = "solid" , alpha = 0.5)
				  }				  
				  
				  #plt <- plt + geom_line(aes(x=TimeStamp, y=count), color = "red", linewidth = 0.5, alpha = 0.5)

				  if ( T )
				  {
						pal <- c("steelblue", "#3B9AB2", "#56A6BA", "#71B3C2", "#9EBE91", "#D1C74C",
						         "#E8C520", "#E4B80E", "#E29E00", "#EA5C00", "#F21A00","#fc0082")
						plt <- plt + geom_point(aes(x=TimeStamp, y=plotY, color=plotY,alpha = 0.8), size=1) + 
						scale_color_gradientn(colors = pal, name="probability", guide = "none", limits = c(0, 1)) + theme(legend.position = "none") +
						geom_point(data = subset(result_df, pre_anomaly_probability4), aes(x = TimeStamp, y = plotY),
						            color = "#fc0082", size = 2, alpha=1)
				  }else
				  {
					  plt <- plt +
					  geom_point(data = subset(result_df, pre_anomaly_probability1), aes(x = TimeStamp, y = plotY),
					            color = "#f5d742", size = 1, alpha=alp/2) +
					  geom_point(data = subset(result_df, pre_anomaly_probability2), aes(x = TimeStamp, y = plotY),
					            color = "#f59942", size = 1, alpha=alp/2) +
					  geom_point(data = subset(result_df, pre_anomaly_probability3), aes(x = TimeStamp, y = plotY),
					            color = "#f55d42", size = 2, alpha=alp/2) +
					  geom_point(data = subset(result_df, pre_anomaly_probability4), aes(x = TimeStamp, y = plotY),
					            color = "#fc0082", size = 2, alpha=alp/2)
				  }
				            
		  		  plt <- plt +  geom_text_repel(data = subset(result_df,plotY> 0.60),
		  		   aes(label = sprintf("%.2f%%",plotY*100)),vjust = -0.5, size = 3, 
		  		   force = 2, max.iter = 1000, box.padding = 0.7, point.padding = 0.5) 
				  
				  plt <- plt + labs(title=paste(sensor_X, "-", sensor_Y),
		         	y = "Anomaly probability", x = paste("time [current:",current_time,"]", sep="")) +
		          scale_x_datetime(breaks = breaks_vec)+
		          coord_cartesian(clip = "off") +
				  theme_minimal() + theme(legend.position = "none")
				  
				plt <- SpeedMeter(plt, current_speed = result_df$plotY[nrow(result_df)]*100, title_text = "", x = 0.73, y = 0.8, width = 0.35, height = 0.35)
			}else
			{
				errR <-  model_errrate/100
				alp <- 1.0
				if ( errR > 0.1 ) alp <- 0.5
				if ( errR > 0.3 ) alp <- 0.2
				if ( errR > 0.5 ) alp <- 0.1

				breaks_vec <- seq(min(result_df$TimeStamp), max(result_df$TimeStamp), length.out = 5)
				
				plt <- ggplot(result_df, aes(x = TimeStamp, y = pre_anomaly_d)) +
				  geom_line(color = "steelblue", linewidth = 0.8, alpha = alp) +
				  geom_hline(yintercept = max(threshold_upper), color = "red", linetype = "dashed") +
				  geom_hline(yintercept = min(cor_threshold_lower), color = "red", linetype = "dashed") +
				  geom_hline(yintercept = 0, color = "orange", linetype = "dashed") +
				  labs(title=paste(sensor_X, "-", sensor_Y),
		         	y = "Anomaly probability", x = paste("time [current:",current_time,"]", sep="")) +
		          scale_x_datetime(breaks = breaks_vec)+
		          coord_cartesian(clip = "off") +
				  theme_minimal()
			}
			
			plots[[i]] <- plt
			posterior_abnormal_list[[i]] <- list(paste(sensor_X, "-", sensor_Y, "Anomaly probability"), result_df$posterior_abnormal)

			num_plt <- num_plt + 1
		}

		if ( length(plots) >= 1 )
		{					
			plt <- gridExtra::grid.arrange(grobs=plots, nrow=nrow(selected_pairs))
		}
	}else
	{
  		
		anomaly_max <- 0	
		if ( abs(mad_base) < 1.0e-10 )
		{
			return (NULL)
		}
		print("Z-score")
		# Z-score
		z_scores <- (sensor_data[,1] - mean_base) / sd_base

		# Modified Z-score
		#print("Modified Z-score")
		#z_scores <- 0.6745 *(sensor_data[,1] - median_base) / mad_base
		
		
		z_score_df <- data.frame( z_score = z_scores)

		colnames(z_score_df) <- c("z_score")
		#cat("colnames(z_scores) ")
		#print(colnames(z_scores))


		colnam <- colnames(train_data)[1]
		print(colnam)
		print(colnames(sensor_data))
		ecdf_normal <- ecdf(zscore_base)
		
		
		###### Bayesian estimation
		
		# Setting the prior probability
		prior_normal <- 0.999
		prior_abnormal <- (1 - prior_normal)
		
		mean_nromal <- zscore_base_mean
		sd_normal <- zscore_base_sd
		 
		mean_abnormal <- mean_nromal + 3*sd_normal
		sd_abnormal <- 1.2*sd_normal
		
		posterior_abnormal <- z_scores*0
		for ( k in 1:length(z_scores))
		{
			x <- z_scores[k]
			
			posterior_abnormal[k] <- posterior_probabilities_Bayes(x, mean_nromal, sd_normal, mean_abnormal, sd_abnormal, prior_normal, prior_abnormal)

			prior_abnormal <- posterior_abnormal[k]
			prior_normal <- (1 - posterior_abnormal[k])
		}
				
		
		
		
		df_plot <- data.frame(
		  time = sensor_data$time_index,
		  value = c(sensor_data[,colnam]),
		  z_score = abs(z_score_df$z_score),
		  #pre_anomaly_probability = 1 - pnorm(sensor_data[,colnam], mean = mean_base, sd = sd_base),
		  pre_anomaly_probability = 1 - ecdf_normal(abs(z_score_df$z_score)),
		  posterior_abnormal = posterior_abnormal^0.2 
		)
		if (!is.null(maintenance_sv))
		{
			df_plot$maintenance <- sensor_data$maintenance
		}
		
		df_plot <- probability_loess(df_plot, maintenance_sv)
		#df_plot <- moving_mean_smooth2(df_plot, timeStamp, 4, 1) 

		posterior_abnormal <- df_plot$posterior_abnormal
		
		cat("df_plot")
		str(df_plot)
		df_plot <- df_plot[, sapply(df_plot, is.numeric)]
		
		threshold_upper <- c(0.3, 0.6, 0.9)
		
		
		df_plot$pre_anomaly_probability1 <- ((df_plot$posterior_abnormal) > threshold_upper[1] & (df_plot$posterior_abnormal) > 0)

		anomaly_max <- quantile((df_plot$pre_anomaly_probability1-(threshold_upper[1])),c(0.6, 0.90), na.rm = TRUE)[2]

		df_plot$pre_anomaly_probability2 <- ((df_plot$posterior_abnormal) > 10000000)
		df_plot$pre_anomaly_probability3 <- ((df_plot$posterior_abnormal) > 10000000)
		if ( length(percent) > 1 )
		{
			df_plot$pre_anomaly2 <- ((df_plot$posterior_abnormal) > threshold_upper[1] & (df_plot$posterior_abnormal) <= threshold_upper[2])
		}
		if ( length(sigma) > 2 )
		{
			df_plot$pre_anomaly_probability3 <- ((df_plot$posterior_abnormal) > threshold_upper[2] & (df_plot$posterior_abnormal) <= threshold_upper[3])
			df_plot$pre_anomaly_probability4 <- ((df_plot$posterior_abnormal) > threshold_upper[3])
		}
		
		
		delta_ix <- df_plot$time[nrow(df_plot)]- df_plot$time[nrow(df_plot)-1]
		
		#print("delta_ix")
		#print(delta_ix)
		#print("delta_time")
		#print(delta_time)
		#print("current_time")
		#print(current_time)
		
		df_plot$TimeStamp <- rev(seq(current_time, length.out = length(1:nrow(df_plot)), by = -delta_time*delta_ix))
		#print(str(df_plot))
		df_plot$TimeStamp <- as.POSIXct(df_plot$TimeStamp, tz='UTC', origin="1970-01-01")
		
		
		breaks_vec <- seq(min(df_plot$TimeStamp), max(df_plot$TimeStamp), length.out = 5)
		
		#df_plot$plotY <- df_plot$pre_anomaly_probability
		#df_plot$plotY <- df_plot$z_score
		df_plot$plotY <- df_plot$posterior_abnormal
		
		
		plt <- ggplot(df_plot, aes(x=TimeStamp, y=plotY)) +
		  geom_line(color="steelblue") +
		  geom_hline(yintercept=c( threshold_upper), color="red", linetype="dashed", alpha = 0.5 )
		  
		  if (  length(grep("maintenance", colnames(df_plot))) > 0)
		  {
		  	plt <- plt +  geom_vline(data = df_plot[df_plot$maintenance == 1, ],
		  			aes(xintercept = TimeStamp), color = "green", linetype = "solid", alpha = 0.5 )
		  }				  
		  #plt <- plt + geom_line(aes(x=TimeStamp, y=count), color = "red", linewidth = 0.5, alpha = 0.5)

		  if ( T )
		  {
				pal <- c("steelblue", "#3B9AB2", "#56A6BA", "#71B3C2", "#9EBE91", "#D1C74C",
				         "#E8C520", "#E4B80E", "#E29E00", "#EA5C00", "#F21A00","#fc0082")
				plt <- plt + geom_point(aes(x=TimeStamp, y=plotY, color=plotY,alpha = 0.8), size=1) + 
				scale_color_gradientn(colors = pal, name="probability", guide = "none",limits = c(0, 1))	+ theme(legend.position = "none")+
				geom_point(data = subset(df_plot, pre_anomaly_probability4), aes(x = TimeStamp, y = plotY),
				             color = "#fc0082", size = 2, alpha=1)
		  }else
		  {
		  
			  plt <- plt + geom_point(data = subset(df_plot, pre_anomaly_probability1), aes(x = TimeStamp, y = plotY),
			             color = "#f5d742", size = 1, alpha=0.7) +
			  geom_point(data = subset(df_plot, pre_anomaly_probability2), aes(x = TimeStamp, y = plotY),
			             color = "#f59942", size = 1, alpha=0.7) +
			  geom_point(data = subset(df_plot, pre_anomaly_probability3), aes(x = TimeStamp, y = plotY),
			             color = "#f55d42", size = 2, alpha=0.8)
			  geom_point(data = subset(df_plot, pre_anomaly_probability4), aes(x = TimeStamp, y = plotY),
			             color = "#fc0082", size = 2, alpha=1)
		  }
		  plt <- plt +  geom_text_repel(data = subset(df_plot,plotY> 0.60), aes(label = sprintf("%.2f%%",plotY*100)),vjust = -0.5, size = 3) 

		  plt <- plt +  labs(title=paste(colnam," :Anomaly probability",sep=""), y="Anomaly probability", x=paste("time [current:",current_time,"]", sep="")) +
          scale_x_datetime(breaks = breaks_vec) + coord_cartesian(clip = "off") +
		  theme_minimal() + theme(legend.position = "none")
		  
		  pr <- df_plot$plotY[nrow(df_plot)]
		  pr_text <- sprintf("%.2f%%", pr*100)
		  plt <- SpeedMeter(plt, current_speed = pr*100, title_text = pr_text, x = 0.75, y = 0.8, width = 0.35, height = 0.35)
		  
	  	posterior_abnormal_list[[1]] <- list(paste(colnam,":Anomaly probability",sep=""), c(df_plot$posterior_abnormal))
	  	
		plt <- gridExtra::grid.arrange(grobs=list(plt), nrow=nrow(1))
		num_plt <- num_plt + 1
		#plt
	}
	print("====== Detection_precursor_phenomena_test end =======")
	
	return( list(plt, plots, num_plt, anomaly_cunt, anomaly_max, posterior_abnormal_list) )
}

Detection_precursor_phenomena <- function(df, timeStamp, dpp_model=NULL, 
			corr_threshold=0.38, scorTopN=6, percent=c(0.80, 0.95, 0.99), window_size = 30, slide = 1, method="spearman")
{
	#print(method)
	nrow_limit_max <- 1000000
	n <- nrow(df)
	#if ( n > nrow_limit_max )
	#{
	#	interval <- floor(n / nrow_limit_max)
	#	sampled_data <- df[seq(1, n, by = interval), ]
	#	sampled_data <- sampled_data[1:nrow_limit_max, ]
	#	df <- sampled_data
	#}
	
	if ( n > nrow_limit_max )
	{
		train <- df[1:(nrow_limit_max*0.1),]
		test <- df[max(n*0.9, n-nrow_limit_max):n,]
	}else
	{
		train <- df[1:(n*0.4),]
		test <- df[(n*0.6):n,]
	}
	if ( nrow(test) < 1000 )
	{
		train <- df
		test <- df
	}
	
	if (is.null(dpp_model))
	{
		df_tmp <- Only_required_fields(df, timeStamp)
		
		dpp_model <- Detection_precursor_phenomena_train(train, colnames(df_tmp), timeStamp, percent, window_size, slide, corr_threshold, scorTopN, method)
		
		if ( is.null(dpp_model))
		{
			return (NULL)
		}
		selected_pairs <- dpp_model[[1]]
		mean_base  <- dpp_model[[2]][[1]][1]
		sd_base    <- dpp_model[[2]][[1]][2]
		median_base<- dpp_model[[2]][[1]][3]
		mad_base   <- dpp_model[[2]][[1]][4]
		train_data <- dpp_model[[3]]
	}
	cat("str(test)")
	print(str(test))
	plt <- Detection_precursor_phenomena_test(test, timeStamp, dpp_model, percent, window_size, slide, method)
	anomaly_cunt <- plt[[4]]
	anomaly_max <- plt[[5]]
	rate <- anomaly_cunt/nrow(test)
	cat("train error rate")
	print(rate)
	posterior_abnormal <-  plt[[6]]

	if ( is.null(plt)) 
	{
		return(NULL)
	}
	return ( list(plt,dpp_model, posterior_abnormal) )
}


TestFuc <- function(dataset, timeStamp, window = 30, slide=7, index_number=0)
{
	if ( Detection_precursor_phenomenaTest_Test )
	{
		corr_threshold <- 0.38
		scorTopN <- 6
		percent= c(0.80, 0.95, 0.99)
		window_size=window
		slide = slide
		method="spearman"
		#method="dcor"
		#method="MIC"

		cat("dataset")
		print(dataset)
		cat("timeStamp")
		print(timeStamp)
		cat("window_size")
		print(window_size)
		cat("slide")
		print(slide)
		#quit()
		dpp_model <- NULL
		if ( !is.null(dataset))
		{
			sensor_data <- read.csv(dataset, fileEncoding = "UTF-8")
			cat("sensor_data")
			print(str(sensor_data))
			sensor_data[, timeStamp] <- as.POSIXct(sensor_data[,timeStamp], tz="UTC", origin="2024-01-01")
			current_time <<- sensor_data[nrow(sensor_data), timeStamp]
			delta_time <<- difftime(current_time , sensor_data[(nrow(sensor_data)-1), timeStamp])
			if ( delta_time == 0 )
			{
				delta_time <<- difftime(sensor_data[(nrow(sensor_data)-1), timeStamp] , sensor_data[(nrow(sensor_data)-2), timeStamp])
			}
			cat("current_time")
			print(current_time)
			cat("sensor_data[(nrow(sensor_data)), timeStamp]")
			print(sensor_data[(nrow(sensor_data)), timeStamp])

			cat("sensor_data[(nrow(sensor_data)-1), timeStamp]")
			print(sensor_data[(nrow(sensor_data)-1), timeStamp])
			cat("delta_time")
			print(delta_time)
			print(sensor_data[1:20, timeStamp])

			window_size = window_size*1
			
			#test
			#sensor_data <- sensor_data[, "sensor2", drop = FALSE]
			#sensor_data <- sensor_data[, "AE_spindle", drop = FALSE]
			
			if ( ncol(sensor_data) == 1 ) scorTopN = 1
			str(sensor_data)
			
			sensor_data$time_index <- c(1:nrow(sensor_data))
			df <- sensor_data

			df_tmp <- moving_mean_smooth2(df, timeStamp, window_size, slide)
			if ( is.null(df_tmp))
			{
				df_tmp <- moving_mean_smooth2(df, timeStamp, window_size/2, slide/2)
			}
			if ( is.null(df_tmp))
			{
				return(NULL)
			}
			df <- df_tmp

			dpp <- Detection_precursor_phenomena(df, timeStamp, dpp_model,
				corr_threshold=corr_threshold,
				scorTopN=scorTopN, percent=percent, window_size, slide, method=method)

			plt <- dpp[[1]]
			dpp_model <- dpp[[2]]
			num_plt <- dpp[[1]][[3]]

			plt[[1]]
			
			curdir = getwd()
			detect_putpng_path= paste(curdir, "/images/Detect/", sep="")
			
			#print(detect_putpng_path)
			index_number = index_number
			detect_png <- sprintf("detection_%06d.png", index_number)
			ggsave(file = paste(detect_putpng_path, detect_png, sep=""), plot = plt[[1]], dpi = 130, width = 10, height = num_plt*1.5)
			
		}
	}
	
	return(plt)
}



