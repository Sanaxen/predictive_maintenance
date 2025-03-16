options(encoding = "utf-8")
options(digits.secs=3)
library(ggplot2)
library(gridExtra)
library(zoo)
library(scales)

library(energy)
library(minerva)

library(lightgbm)

#dataset <- NULL


#dataset<-NULL

distance_covariance_high_corr_sample_max <- 1000
maximal_information_coefficient_high_corr_sample_max <- 1000

distance_covariance_sample_max <- 100
maximal_information_coefficient_sample_max <- 100

detection_precursor_phenomena_train_sample_max <- 2500

# 必要に応じてzooパッケージをインストールしてください
# install.packages("zoo")
library(zoo)

moving_mean_smooth <- function(df, timeStamp, window_size) 
{
	print("========== moving_mean_smooth start ===============")
	print(str(df))
	print(timeStamp)
	time_index_sv <- df$time_index
	timestamp_sv <- NULL
	maintenance_sv <- NULL
	
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
	df$time_index <- NULL

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
	
		time_index2[i] <- time_index_sv[i]
		if (!is.null(timestamp_sv))
		{
			timestamp2[i] <- timestamp_sv[i]
		}
		if (!is.null(maintenance_sv))
		{
			maintenance2[i] <- maintenance_sv[i]
		}
	}
	smoothed_df$time_index <- time_index2
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


Only_required_fields <- function(sensor_data)
{
	print("======= Only_required_fields start ==========")
	print(str(sensor_data))

	total_length <- nrow(sensor_data)
	sensor_data <- sensor_data[, sapply(sensor_data, function(x) is.numeric(x) != 0), drop=FALSE]
	#print(str(sensor_data))
	sensor_data <- sensor_data[, sapply(sensor_data, function(x) sd(x, na.rm = TRUE) != 0), drop=FALSE]
	#print(str(sensor_data))
	sensor_data <- sensor_data[, sapply(sensor_data, function(x) length(unique(x)) > total_length/200), drop=FALSE]
	#print(str(sensor_data))
	
	# Exclude index columns (integer columns increasing consecutively by 1)
	is_index_col <- function(x) {
	  all(diff(x) == 1) && is.integer(x)
	}

	# Detect and exclude index-like columns
	#sensor_data <- sensor_data[, !sapply(sensor_data, is_index_col)]
	sensor_data <- sensor_data[, !sapply(sensor_data, function(x) is_index_col(x)), drop=FALSE]
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

	valid_data <- data.frame(X=initial_data[[sensor_X]], Y=initial_data[[sensor_Y]])
		
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

Detection_precursor_phenomena_train <- function(df, corr_threshold=0.6, scorTopN=4, method="spearman")
{
	print("========= Detection_precursor_phenomena_train start ========")
	#print(str(df))
	
	maintenance_sv <- NULL
	time_index_sv <- df$time_index
	#cat("time_index")
	#print(time_index_sv)
	if (  length(grep("maintenance", colnames(df))) > 0)
	{
		maintenance_sv <- df$maintenance
	}

	sensor_data <- Only_required_fields(df)
	total_length <- nrow(sensor_data)

	#cat("total_length")
	#print(total_length)

	rate = 0.6
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
	#cat("initial_data")
	#print(str(initial_data))

	selected_pairs <- NULL
	mean_base <- NULL
	sd_base   <- NULL
	median_base <- NULL
	mad_base <- NULL
	
	cat("ncol(initial_data)")
	print(ncol(initial_data))
	if ( ncol(initial_data) == 1)
	{
		mean_base <- mean(initial_data[,1], na.rm=TRUE)
		sd_base   <- sd(initial_data[,1], na.rm=TRUE)
		median_base <- median(initial_data[,1], na.rm=TRUE)
		mad_base <- median(abs(initial_data[,1] - median_base), na.rm = TRUE)
		
		if ( abs(mad_base) < 1.0e-10 )
		{
			mad_base <- 1.0e-10
		}
		if ( abs(sd_base) < 1.0e-10 )
		{
			sd_base <- 1.0e-10
		}
		
		print(sprintf("mean_base:%f sd_base:%f median_base:%f mad_base:%f",
		mean_base, sd_base, median_base, mad_base))
	}
	prm = c(mean_base,sd_base,median_base,mad_base)
	#print(prm)
	
	#cat("ncol(initial_data)")
	#print(ncol(initial_data))
	if ( ncol(initial_data)> 1)
	{
		high_corr_pairs_lst <- get_high_corr_pairs(initial_data, corr_threshold, method)
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
		return (list(NULL, prm, initial_data, NULL))
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
	
	return (list(selected_pairs, prm, initial_data, model_list))
}


Detection_precursor_phenomena_test <- function(df, dpp_model, percent=0.9, method="spearman")
{
	print("====== Detection_precursor_phenomena_test start =======")

	maintenance_sv <- NULL
	if (  length(grep("maintenance", colnames(df))) > 0)
	{
		maintenance_sv <- df$maintenance
	}

	time_index_sv <- df$time_index
	sensor_data <- Only_required_fields(df)
	sensor_data$time_index <- NULL
	if (  length(grep("maintenance", colnames(sensor_data))) > 0)
	{
		sensor_data$maintenance <- NULL
	}
	
	#cat("nrow(sensor_data) ")
	#print(nrow(sensor_data))
	
	selected_pairs <- dpp_model[[1]]
	mean_base <- dpp_model[[2]][1]
	sd_base <- dpp_model[[2]][2]
	median_base<- dpp_model[[2]][3]
	mad_base <- dpp_model[[2]][4]
	train_data <- dpp_model[[3]]
	model_list <- dpp_model[[4]]
	
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
	sigma <- qnorm( 1 - (1 - percent) / 2 )
	cat("sigma:")
	print(sigma)

	cat("selected_pairs:")
	print(selected_pairs)
	

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


			# pre_anomaly_d
			#
			#Discrepancy between predictions and observed values 
			#(the average of the discrepancy between predictions and observed values under normal conditions is set to 0)
			
			result_df <- data.frame(
			  time = sensor_data$time_index,
			  Y = sensor_data[[Y]],
			  pre_anomaly_d = pred_residuals_err - mean_anomaly
			)
			if (!is.null(maintenance_sv))
			{
				result_df$maintenance <- sensor_data$maintenance
			}


			delta_ix <- result_df$time[nrow(result_df)]- result_df$time[nrow(result_df)-1]
			result_df$TimeStamp <- rev(seq(current_time, length.out = length(1:nrow(result_df)), by = -delta_time*delta_ix))
			result_df$TimeStamp <- as.POSIXct(result_df$TimeStamp, tz='UTC', origin="1970-01-01")

			cor_threshold_upper <-  sigma*sd(model_err, na.rm=TRUE) + 0.01*model_errrate
			cor_threshold_lower <- -sigma*sd(model_err, na.rm=TRUE) - 0.01*model_errrate

			mean_res <- mean(pred_residuals_err)
			sd_res <- sd(pred_residuals_err)
			p_values <- dnorm(pred_residuals_err, mean = mean_res, sd = sd_res)
			
			result_df$pre_anomaly <-  (-log(p_values))
			
			z_threshold <- qnorm(percent)			#z-values corresponding to (1-percent)% each area in the two-tailed test
			d_threshold <- z_threshold * sd_res		#Residual Threshold
			p_threshold <- dnorm(d_threshold, mean = mean_res, sd = sd_res)		#Probability density at threshold
			S_threshold <- -log(p_threshold)		#Calculate Abnormality Score Threshold
			
			# Set the judgment flag
			result_df$pre_anomaly1 <- (result_df$pre_anomaly > S_threshold[1] & (result_df$pre_anomaly_d > cor_threshold_upper[1] | result_df$pre_anomaly_d < cor_threshold_lower[1]))
			result_df$pre_anomaly2 <- (result_df$pre_anomaly > 10000000)
			result_df$pre_anomaly3 <- (result_df$pre_anomaly > 10000000)
			if ( length(percent) > 1 )
			{
				result_df$pre_anomaly2 <- (result_df$pre_anomaly > S_threshold[2] & (result_df$pre_anomaly_d > cor_threshold_upper[2] | result_df$pre_anomaly_d < cor_threshold_lower[2]))
			}
			if ( length(percent) > 2 )
			{
				result_df$pre_anomaly3 <- (result_df$pre_anomaly > S_threshold[3] & (result_df$pre_anomaly_d > cor_threshold_upper[3] | result_df$pre_anomaly_d < cor_threshold_lower[3]))
			}
			
			
			#print("result_df")
			#print(str(result_df))
			#quit()
			
			if ( !is.null(result_df$pre_anomaly1))
			{
				breaks_vec <- seq(min(result_df$TimeStamp), max(result_df$TimeStamp), length.out = 5)
				
				result_df$plotY <- result_df$pre_anomaly_d
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
				  geom_hline(yintercept = max(cor_threshold_upper), color = "red", linetype = "dashed") +
				  geom_hline(yintercept = min(cor_threshold_lower), color = "red", linetype = "dashed") +
				  geom_hline(yintercept = 0, color = "orange", linetype = "dashed")
				  
				  if (  length(grep("maintenance", colnames(result_df))) > 0)
				  {
				  	plt <- plt +  geom_vline(data = result_df[result_df$maintenance == 1, ],
				  			aes(xintercept = TimeStamp), color = "green", linetype = "solid" , alpha = 0.5)
				  }				  
				  
				  plt <- plt +
				  geom_point(data = subset(result_df, pre_anomaly1), aes(x = TimeStamp, y = plotY),
				             color = "#f5d742", size = 2, alpha=alp/2) +
				  geom_point(data = subset(result_df, pre_anomaly2), aes(x = TimeStamp, y = plotY),
				             color = "#f59942", size = 2, alpha=alp/2) +
				  geom_point(data = subset(result_df, pre_anomaly3), aes(x = TimeStamp, y = plotY),
				             color = "#f55d42", size = 2, alpha=alp/2) +
				  labs(title=paste(sensor_X, "-", sensor_Y),
		         	y = "Anomaly Score", x = paste("time [current:",current_time,"]", sep="")) +
		          scale_x_datetime(breaks = breaks_vec)+
				  theme_minimal()
			}else
			{
				breaks_vec <- seq(min(result_df$TimeStamp), max(result_df$TimeStamp), length.out = 5)
				
				plt <- ggplot(result_df, aes(x = TimeStamp, y = pre_anomaly_d)) +
				  geom_line(color = "steelblue", linewidth = 0.8, alpha = alp) +
				  geom_hline(yintercept = max(cor_threshold_upper), color = "red", linetype = "dashed") +
				  geom_hline(yintercept = min(cor_threshold_lower), color = "red", linetype = "dashed") +
				  geom_hline(yintercept = 0, color = "orange", linetype = "dashed") +
				  labs(title=paste(sensor_X, "-", sensor_Y),
		         	y = "Anomaly Score", x = paste("time [current:",current_time,"]", sep="")) +
		          scale_x_datetime(breaks = breaks_vec)+
				  theme_minimal()
			}
			plots[[i]] <- plt
			num_plt <- num_plt + 1
		}

							
		plt <- gridExtra::grid.arrange(grobs=plots, nrow=nrow(selected_pairs))
	}else
	{
		if ( abs(mad_base) < 1.0e-10 )
		{
			return (NULL)
		}
		print("Z-score")
		# Z-score
		#z_scores <- (sensor_data[,1] - mean_base) / sd_base

		# Modified Z-score
		z_scores <- 0.6745 *(sensor_data[,1] - median_base) / mad_base
		
		
		z_score_df <- data.frame( z_score = z_scores)

		colnames(z_score_df) <- c("z_score")
		#cat("colnames(z_scores) ")
		#print(colnames(z_scores))


		df_plot <- data.frame(
		  time = sensor_data$time_index,
		  value = c(sensor_data[,1]),
		  z_score = z_score_df$z_score,
		  prob = 2*(1 - pnorm(abs(z_score_df$z_score)))
		)
		if (!is.null(maintenance_sv))
		{
			df_plot$maintenance <- sensor_data$maintenance
		}
		str(df_plot)
		df_plot <- df_plot[, sapply(df_plot, is.numeric)]
		
		df_plot$pre_anomaly1 <- (abs(df_plot$z_score) > sigma[1])
		df_plot$pre_anomaly2 <- (abs(df_plot$z_score) > 10000000)
		df_plot$pre_anomaly3 <- (abs(df_plot$z_score) > 10000000)
		if ( length(percent) > 1 )
		{
			df_plot$pre_anomaly2 <- (abs(df_plot$z_score) > sigma[2])
		}
		if ( length(sigma) > 2 )
		{
			df_plot$pre_anomaly3 <- (abs(df_plot$z_score) > sigma[3])
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
		
		plt <- ggplot(df_plot, aes(x=TimeStamp, y=z_score)) +
		  geom_line(color="steelblue") +
		  geom_hline(yintercept=c(-sigma[2],0, sigma[2]), color="red", linetype="dashed") +
		  geom_point(data = subset(df_plot, pre_anomaly1), aes(x = TimeStamp, y = z_score),
		             color = "#f5d742", size = 2, alpha=0.5) +
		  geom_point(data = subset(df_plot, pre_anomaly2), aes(x = TimeStamp, y = z_score),
		             color = "#f59942", size = 2, alpha=0.5) +
		  geom_point(data = subset(df_plot, pre_anomaly3), aes(x = TimeStamp, y = z_score),
		             color = "#f55d42", size = 2, alpha=0.5)
		             
		  if (  length(grep("maintenance", colnames(df_plot))) > 0)
		  {
		  	plt <- plt +  geom_vline(data = df_plot[df_plot$maintenance == 1, ],
		  			aes(xintercept = TimeStamp), color = "green", linetype = "solid", alpha = 0.5 )
		  }				  

		  plt <- plt +  labs(title="Anomaly Score(Z-score)", y="z_score", x="time") +
          scale_x_datetime(breaks = breaks_vec)+
		  theme_minimal()
		  
		plt <- gridExtra::grid.arrange(grobs=list(plt), nrow=nrow(1))
		num_plt <- num_plt + 1
		#plt
		  
	}
	print("====== Detection_precursor_phenomena_test end =======")
	
	return( list(plt, plots, num_plt) )
}

Detection_precursor_phenomena <- function(df, dpp_model=NULL,
			corr_threshold=0.6, scorTopN=4, percent=c(0.75, 0.9), method="spearman")
{
	#print(method)
	nrow_limit_max <- 1000000
	n <- nrow(df)
	if ( n > nrow_limit_max )
	{
		interval <- floor(n / nrow_limit_max)
		sampled_data <- df[seq(1, n, by = interval), ]
		sampled_data <- sampled_data[1:nrow_limit_max, ]
		df <- sampled_data
	}
	
	if (is.null(dpp_model))
	{
		dpp_model <- Detection_precursor_phenomena_train(df, corr_threshold, scorTopN, method)
		
		if ( is.null(dpp_model))
		{
			return (NULL)
		}
		selected_pairs <- dpp_model[[1]]
		mean_base <- dpp_model[[2]][1]
		sd_base <- dpp_model[[2]][2]
		median_base<- dpp_model[[2]][3]
		mad_base <- dpp_model[[2]][4]
		train_data <- dpp_model[[3]]
	}
	plt <- Detection_precursor_phenomena_test(df, dpp_model, percent, method)
	
	if ( is.null(plt)) 
	{
		return(NULL)
	}
	return ( list(plt,dpp_model) )
}


TestFuc <- function(dataset, timestamp, index_number=0)
{
	if ( Detection_precursor_phenomenaTest_Test )
	{
		corr_threshold <- 0.39
		scorTopN <- 6
		percent= c(0.90, 0.95, 0.99)
		window_size=30
		method="spearman"
		#method="dcor"
		#method="MIC"


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

			window_size = as.integer(nrow(sensor_data)/72)
			
			#test
			#sensor_data <- sensor_data[, "sensor2", drop = FALSE]
			#sensor_data <- sensor_data[, "AE_spindle", drop = FALSE]
			
			if ( ncol(sensor_data) == 1 ) scorTopN = 1
			str(sensor_data)

			sensor_data$time_index <- c(1:nrow(sensor_data))
			df <- sensor_data

			df <- moving_mean_smooth(df, timeStamp, window_size)

			dpp <- Detection_precursor_phenomena(df,dpp_model,
				corr_threshold=corr_threshold,
				scorTopN=scorTopN, percent=percent, method=method)

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



