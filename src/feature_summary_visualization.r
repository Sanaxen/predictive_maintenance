

# library(patchwork)
library(cowplot)


find_closest_factors <- function(n) {
  sqrt_n <- floor(sqrt(n))
  for (i in sqrt_n:1) {
    if (n %% i == 0) {
      return(c(i, n/i))
    }
  }
}

#Smoothness
calculate_roughness <- function(y) {
  diff_y <- diff(y, differences = 2)
  roughness <- sum(diff_y^2)
  return(roughness)
}
#  Detect inflection points
detect_inflection_points <- function(x, y) {
  # Calculation of first derivatives
  dy <- diff(y) / diff(x)
  # Calculating second derivatives
  ddy <- diff(dy) / diff(x[-1])
  # Detects points where the sign changes
  inflection_points <- which(diff(sign(ddy)) != 0) + 1
  return(inflection_points)
}
# Peak (local maximum) detection
find_peaks <- function(y) {
  peaks <- c()
  for (i in 2:(length(y) - 1)) {
    if (y[i] > y[i - 1] && y[i] > y[i + 1]) {
      peaks <- c(peaks, i)
    }
  }
  return(peaks)
}

# Trough (local minimum) detection
find_troughs <- function(y) {
  troughs <- c()
  for (i in 2:(length(y) - 1)) {
    if (y[i] < y[i - 1] && y[i] < y[i + 1]) {
      troughs <- c(troughs, i)
    }
  }
  return(troughs)
}
peaks_count <- function(x, y)
{
	# Data smoothing using LOESS
	span <- 0.3  # Set span parameters
	loess_model <- loess(y ~ x, span = span)

	# Get predicted value of smoothed data
	y_smooth <- predict(loess_model, x)
	
	peaks <- find_peaks(y_smooth)
	troughs <- find_troughs(y_smooth)

	num_peaks <- length(peaks)
	num_troughs <- length(troughs)

	return( c(num_peaks,num_troughs))
}

freeram <- function(...) invisible(gc(...))


maintenance_interval <- function(df, start_idx=1)
{
	ss = start_idx
	if ( df$maintenance[start_idx] == 1 )
	{
		#Skip consecutive 1s.
		for ( sss in start_idx:nrow(df))
		{
			ss = sss
			if ( df$maintenance[sss] == 1 ) next
			break
		}
	}
	print(sprintf("skipp maintenance==1 -> %d\n", ss))
	
	s = ss
	#Skip consecutive zeros
	for ( sss in s:nrow(df))
	{
		ss = sss
		if ( df$maintenance[sss] == 0 ) next
		break
	}
	print(sprintf("skipp maintenance==0 -> %d\n", ss))

	st = ss
	#Skip consecutive 1s.
	for ( sss in st:nrow(df))
	{
		ss = sss
		if ( df$maintenance[sss] == 1 ) next
		break
	}
	print(sprintf("skipp maintenance==1 -> %d\n", ss))

	st = start_idx
	ed = ss + as.integer((ss - st)/4)
	if ( ed > nrow(df))
	{
		ed = nrow(df)
	}
	print(sprintf("st:%d  ed:%d\n", st, ed))
	
	return( list(st, ed))
}

base_name <<- ''
feature_summary_visualization <- function( csvfile, timeStamp , summary=FALSE)
{
	sigin <<- 1

	feature_summary_visualization_start <- Sys.time()

	print(getwd())
	csvfile <<- '../all.csv'
	
	if ( file.exists('all.csv'))
	{
		df <- fread('all.csv', na.strings=c("", "NULL"), header = TRUE, stringsAsFactors = F)
		df <- as.data.frame(df)
	}else
	{
		df <- appedAll_csv(dir='./Untreated', outfile = csvfile)
	}
	# Fill in missing values in numerical columns with average values
	df_filled <- df %>%
	  mutate(across(where(is.numeric), ~ ifelse(is.na(.), mean(., na.rm = TRUE), .)))
	df <- df_filled

	print(sprintf("nrow:%d ncol:%d\n", nrow(df), ncol(df)))
	print(head(df))
	print(str(df))
	flush.console()
	
	print(timeStamp)
	print(smooth_window )
	print(smooth_window_slide )
	print(smooth_window2)
	print(smooth_window_slide2)
	print(lookback)
	print(lookback_slide) 

	print(getwd())

	maintenance_index = which("maintenance" == colnames(df))
	if (  length(maintenance_index) > 0 && nrow(df) > 3000)
	{
		df$maintenance[is.na(df$maintenance)] <- 0
	
	
		if ( length(df[df$maintenance==1,]) >= 2 )
		{
			st = 1
			if ( df$maintenance[st] == 1 )
			{
				#Skip consecutive 1s.
				for ( sss in st:nrow(df))
				{
					ss = sss
					if ( df$maintenance[sss] == 1 ) next
					break
				}
			}
			interval <- maintenance_interval(df, start_idx=st)
			st = interval[[1]]
			ed = interval[[2]]
			df_ <- df[st:ed,]

			cat("nrow(df_)")
			print(nrow(df_))			
			if ( nrow(df_) < 300 )
			{
				interval <- maintenance_interval(df, start_idx=ed)
				st = interval[[1]]
				ed = interval[[2]]
				df_ <- df[st:ed,]
				cat("nrow(df_)")
				print(nrow(df_))			
			}
			if ( nrow(df_) < 300 )
			{
				cat("nrow(df_) < 300 error")
				#quit()
			}else
			{
				df <- df_
			}
			write.csv(df, './all.csv', row.names = F)
			print(head(df))
			print(nrow(df))
			print(str(df))
		}
	}
	
	#if (  length(maintenance_index) < 1 && nrow(df) > 30000)
	#{
	#	df <- df[1:as.integer(nrow(df)/3),]
	#	df <- as.data.frame(df)
	#}
	maxrows <- 10000
	if ( nrow(df) > maxrows*50 )
	{
		w <- as.integer(nrow(df)/(maxrows))
		cat("w")
		print(w)
		flush.console()

		sampling=TRUE
		if ( w >= 5 )
		{
			smooth_window <<- as.integer(w)
			smooth_window_slide <<-  max(1,as.integer(smooth_window/25))
			print(sprintf("moving_average smooth_window=%d smooth_window_slide=%d", smooth_window, smooth_window_slide))
			flush.console()
			N=nrow(df)
			df <- moving_average(sampling, df, lookback=smooth_window, slide_window=smooth_window_slide)
			
			n <- window_moving_size(N, smooth_window, smooth_window_slide)
			print(sprintf("N:%d -> nrow(df):%d n:%d", N, nrow(df), n))

			csvfile2 <<- 'all2.csv'
			try(write.csv(df, csvfile2, row.names = F), silent = FALSE)
			print(sprintf("nrow:%d ncol:%d\n", nrow(df), ncol(df)))
			flush.console()
		}else
		{
			print(sprintf("ERROR nrow:%d ncol:%d\n", nrow(df), ncol(df)))
			flush.console()
			exit()
		}
	}else
	{
		csvfile2 <<- './all.csv'
	}
	rm(df)
	freeram()
	
	print(getwd())

	print(csvfile2)

	#df2 <- read.csv( csvfile, header=T, stringsAsFactors = F, na.strings = c("", "NA"))
	df2 <- get_data_frame(csvfile2, timeStamp)

	str(df2)
	timeCol <- df2[,timeStamp]
	df2[,timeStamp] <- NULL
	df2$time_index <- c(1:(nrow(df2)))
	
	#Data reduction for debugging
	#df2 <- df2[,c("time_index", "maintenance","xxxx")]
	#df2 <- as.data.frame(df2)

	cat("df2")
	print(str(df2))
	flush.console()

	if ( TRUE )
	{
		x <- reshape2::melt(df2, id.vars=c("time_index"), variable.name="key",value.name="target")

		p <- x %>% 
		ggplot(aes(x = time_index, y = target))+facet_wrap( ~ key, scales = "free")+
		geom_line(linewidth =0.4, color="#191970")
		#ggsave(filename=paste(base_name, "_input.png", sep=''), p, limitsize=F, width = 16*2, height = 9*2)
		ggsave(filename=paste(base_name, "_input.png", sep=''), p, limitsize=F, width = 16*1, height = 9*1)

		p <- ggplotly(p)
		print(p)
		htmlwidgets::saveWidget(as_widget(p), paste(base_name,"_input.html",sep=''), selfcontained = F)

		mahalanobis_train <- df2[1:(nrow(df2))*0.8,]
		m_mahalanobis <<- anomaly_detection_train(mahalanobis_train)

		mahalanobis_test <- anomaly_detection_test(m_mahalanobis, mahalanobis_train)
		plot(mahalanobis_test[[2]], type="l")
		mahalanobis_df <- data.frame(
				time_index=c(1:length(mahalanobis_test[[2]])), 
				Abnormality=as.vector(mahalanobis_test[[2]]))

		threshold_ = max(mahalanobis_df["Abnormality"])
		print(sprintf("threshold:%f", threshold_))
		flush.console()
		plt_abnormality <- mahalanobis_df %>% ggplot(aes(x=time_index,y=Abnormality)) + geom_line()
		plt_abnormality
		ggsave(filename=paste(base_name,"_abnormality.png",sep=''), plt_abnormality, limitsize=F, width = 16, height = 9)
		print("ggsave")
		flush.console()
	}
	
	feature_train <- df2[(nrow(df2) - (nrow(df2))*0.9):nrow(df2),]
	#feature_df <- feature(feature_train, lookback=lookback)
	#print(sprintf("%d/%d nrow(feature_df):%d", i, as.integer(nrow(df)/one_input),nrow(feature_df)))

	lookback_max = nrow(df2)/10
	lookback_list = c(  19200, 9600, 4800, 3840, 2400, 1920, 960, 600, 480, 300, 240, 120, 60, 48, 24, 12, 8)
	#lookback_list = c(   120)
	#lookback_list = c( 24)
	cat("lookback_list")
	print(lookback_list)
	flush.console()

	plt_list <- c(1:length(lookback_list))
	xxxx <- NULL
	for ( iii in 1:length(lookback_list))
	{
		start <- Sys.time()

		lookback <<- lookback_list[iii]
		lookback_slide <<- as.integer(lookback/4)
		#lookback_slide <<- as.integer(lookback*0.16)
		
	
		print(sprintf("%d lookback:%d lookback_slide:%d", iii, lookback, lookback_slide))
		flush.console()
		#if ( lookback_max < lookback )
		#{
		#	print(sprintf("lookback_max:%d lookback:%d", lookback_max, lookback))
		#	next
		#}
		N=nrow(df2)
		#n <- window_moving_size(N, lookback, lookback_slide)
		#n <- window_moving_size(n, smooth_window2, smooth_window_slide2)

		#if ( n < 5 )
		#{
		#	print(sprintf("nrow:%d -> %d", N, n))
		#	flush.console()
		#	#next
		#}
		if ( nrow(df2) < lookback*2 )
		{
			print(sprintf("nrow:%d lookback*2:%d", nrow(df2), lookback*2))
			flush.console()
			next
		}
		feature_df <- try(
		feature(df2, lookback=lookback, slide_window = lookback_slide),silent=F)
		if ( class(feature_df) == "try-error" )
		{
			print("class(feature_df) == \"try-error\"")
			flush.console()
			next
		}
		if ( is.null(feature_df))
		{
			next
		}
		#feature_df <- feature(df2, lookback=lookback, slide_window = lookback_slide)
		#print("feature_df")
		#print(colnames(feature_df))
		feature_df_org <- feature_df
		
		end <- Sys.time()
		diff <- as.numeric(difftime(end, start, units = "sec"))

		print(sprintf("get feature Time:%f sec( %f min)( %f hour)", diff, diff/60, diff/(60*60)))
		flush.console()

		smooth_window2 <<-  as.integer(lookback/4)
		smooth_window_slide2 <<- 1
		print(sprintf("nrow(feature_df):%d smooth_window2:%d smooth_window_slide2:%d", nrow(feature_df), smooth_window2, smooth_window_slide2))
		
		if ( smooth_window2 > 1 )
		{
			feature_df <- try(smooth(feature_df, smooth_window = smooth_window2, smooth_window_slide=smooth_window_slide2),silent=F)
			if ( class(feature_df) == "try-error" )
			{
				print("class(feature_df smooth) == \"try-error\"")
				flush.console()
				next
				#str(feature_df)
			}
			if ( is.null(feature_df))
			{
				next
			}
			if ( nrow(feature_df) < 3)
			{
				cat("nrow(feature_df)")
				print(nrow(feature_df))
				next
			}
			write.csv(feature_df, "./feature_df.csv", row.names = F)
			cat("feature_df")
			print(nrow(feature_df))
			sprintf("nrow:%d ncol:%d", nrow(feature_df),ncol(feature_df))
		}
		
		end <- Sys.time()
		diff <- as.numeric(difftime(end, start, units = "sec"))

		print(sprintf("get feature Time:%f sec( %f min)( %f hour)", diff, diff/60, diff/(60*60)))
		flush.console()
		
		if ( summary )
		{
			xx <- reshape2::melt(feature_df_org, id.vars=c("time_index"), variable.name="key",value.name="target")
			x <- reshape2::melt(feature_df, id.vars=c("time_index"), variable.name="key",value.name="target")

			p <- x %>% 
			ggplot(aes(x = time_index, y = target))+facet_wrap( ~ key, scales = "free")+
			geom_line(data=xx, aes(x = xx$time_index, y = xx$target), linewidth =0.2, color="#191970")+
			geom_line(data=x,linewidth =0.4, color="#ff4500")
			
			#filename <- sprintf("%s_feature_df_window=%d_slide=%d.png", base_name, lookback, lookback_slide)
			filename <- sprintf("%s_feature_df.png", base_name)

			ggsave(filename=filename, p, limitsize=F, width = 16*2, height = 9*2)

			p <- ggplotly(p)
			print(p)
			htmlwidgets::saveWidget(as_widget(p), paste(base_name,"_feature_df.html",sep=''), selfcontained = F)

			#Calculation of monotonicity for each feature
			fm <- feature_monotonicity(feature_df, monotonicity_num=nrow(feature_df))
			fm <- rbind(fm, c(1:ncol(fm)))

			#Parameter sorting for each feature
			print("fm")
			print(colnames(fm))
			f1 <- data.frame(matrix(colnames(fm)),ncol=1)[,1]
			f2 <- cbind(f1, data.frame(as.numeric(fm[1,]),ncol=1))[,1:2]
			f2 <- cbind(f2, data.frame(as.numeric(fm[2,]),ncol=1))[,1:3]
			colnames(f2) <- c("feature", "monotonicity", "index")
			print(f2)

			#Bar chart for each monotonicity
			xlab=sprintf("feature [total number of features:%d]", nrow(f2)-1)
			plt0 <- f2 %>% ggplot(aes(x = reorder(feature,-monotonicity), y = monotonicity, fill = feature))+ geom_bar(stat = "identity")+ theme(legend.position = 'none')+xlab(xlab)+ theme(axis.text.x = element_blank())
			plt0
			ggsave(filename=paste(base_name,"_monotonicity.png",sep=''), plt0, limitsize=F, width = 16, height = 9)
			
			#Sort in descending order of monotonicity
			leave_num = min(nrow(f2),25)
			if ( sigin > 0 )
			{
				fm2 <- f2[order(f2$monotonicity, decreasing=T),][1:leave_num,]
			}else
			{
				fm2 <- f2[order(f2$monotonicity, decreasing=F),][1:leave_num,]
			}
			print("fm2")
			print(colnames(fm2))
			flush.console()
			

			fm22 <- rbind(fm2, f2[f2$feature=="mahalanobis",])
			fm22[fm22$feature=="mahalanobis",]$feature = "abnormality"
			#Bar chart for each monotonicity
			plt1 <- fm22 %>% ggplot(aes(x = reorder(feature,-monotonicity), y = monotonicity, fill = feature))+ geom_bar(stat = "identity")+xlab(sprintf("feature top %d & abnormality",leave_num))+
			geom_text(aes(label = ifelse(monotonicity > 0.001 ,as.integer(monotonicity*1000)/1000,monotonicity)), size = 4, hjust = 0.5, vjust = 2, position = "stack") 
			ggsave(filename=paste(base_name,"_monotonicity2.png",sep=''), plt1, limitsize=F, width = 16, height = 9)
			
			plt1
			p <- ggplotly(plt1)
			print(p)
			htmlwidgets::saveWidget(as_widget(p), paste(base_name,"_monotonicity2.html",sep=''), selfcontained = F)
			
			tracking_feature <<- c()
			for ( i in 1:(leave_num) )
			{
				tracking_feature <<- c(tracking_feature,colnames(feature_df)[fm2$index[i]])
			}
			if ( !length(which("mahalanobis" == tracking_feature)))
			{
				tracking_feature <<- c(tracking_feature,"mahalanobis")
			}
			print(tracking_feature)
			
			plt2 <- features_plot(tracking_feature)
			ggsave(filename=paste(base_name,"_tracking_feature.png",sep=''), plt2, limitsize=F, width = 16, height = 9)
			p <- ggplotly(plt2)
			print(p)
			htmlwidgets::saveWidget(as_widget(p), paste(base_name,"_feature_summary_visualization1.html",sep=''), selfcontained = F)
			
			if ( is.null(tracking_feature))
			{
				if ( nrow(feature_df) > 2000 )
				{
					x <- feature_df[(nrow(feature_df)-1000):nrow(feature_df),]
				}else
				{
					x <- feature_df[1:nrow(feature_df),]
				}
				x <- reshape2::melt(x, id.vars=c("time_index"), measure.vars=colnames(x)[2:length(x)], 
								variable.name="key",value.name="target")
				p <- x %>% 
				  ggplot(aes(x = time_index, y = target, color=key))+
				  geom_line()
			}else
			{
				x <- feature_df[c("time_index",tracking_feature)]
				x <- reshape2::melt(x, id.vars=c("time_index"), measure.vars=colnames(x)[2:length(x)], 
								variable.name="key",value.name="target")
				p <- x %>% 
				  ggplot(aes(x = time_index, y = target, color=key))+
				  geom_line()
			}
			
			plot(p)
		}
		
		maintenance_flag_idx = which("maintenance" == colnames(df2))
		maintenance_flag_df <- NULL
		if ( length(maintenance_flag_idx) < 1 )
		{
			maintenance_flag_idx <- 0
			maintenance_flag_time_index = 0
		}else
		{
			maintenance_flag_df <- df2[,maintenance_flag_idx]
			maintenance_flag_time_index <- df2$time_index[df2[,maintenance_flag_idx]==1]
		}
		cat("maintenance_flag_idx")
		print(maintenance_flag_idx)
		
		smoother_span_list = c( 0.05 )
		for ( kkk in 1:length(smoother_span_list))
		{
			for ( i in 1:ncol(feature_df))
			{
				if ( colnames(feature_df)[i] == 'time_index' || colnames(feature_df)[i] == 'maintenance')
				{
					next
				}
				
				monotonicity_value = 0
				rmse1 = 999999.0
				rmse2 = 999999.0
				delta_abc1 = 2
				delta_abc2 = 2
				delta_abc = 2
				r_squared1 = 2
				r_squared2 = 2
				fvalue_p1 = 0
				fvalue_p2 = 0
				aic1 = 999999.0
				aic2 = 999999.0
				high_freq_magnitude1 = 999999
				high_freq_magnitude2 = 999999
				peaks_num = 999999
				if ( maintenance_flag_idx > 0 )
				{
					idx <- which(feature_df$maintenance == 1)
					if ( length(idx) > 0 )
					{
						monotonicity_value1 = 0
						monotonicity_value2 = 0
						rmse1 = 999999.0
						rmse2 = 999999.0
						
						tmp <- feature_df[1:idx[1],]
						if ( nrow(tmp) > 2 )
						{
							monotonicity_value1 <- monotonicity(tmp[,i], nrow(tmp), eps = 0.0)
							tmpdf <- data.frame(x=c(1:nrow(tmp)),y=(tmp[,i]-min(tmp[,i]))/(max(tmp[,i])-min(tmp[,i])))
							tmpdf <- na.omit(tmpdf)
							if ( nrow(tmpdf) > 2 )
							{
								lm1 <- try(lm(y ~ x, data=tmpdf, na.action=na.exclude), silent = FALSE)
								if (class(lm1) == "try-error") {
									lm1 <- NULL
							  	}
								if ( !is.null(lm1) && coef(lm1)[2] * monotonicity_value1 > 0 && abs(monotonicity_value1) > 0.1 && abs(coef(lm1)[2]) > 0.0001)
								{
									f_statistic <- summary(lm1)$fstatistic
									fvalue_p1 <- pf(f_statistic[1], f_statistic[2], f_statistic[3], lower.tail = FALSE)
									r_squared1 <- summary(lm1)$r.squared
									#sse <- sum((lm1$residuals)^2)
									#rmse1 <- sse
									rmse1 <- sqrt(mean((tmpdf$y -  predict(lm1, tmpdf))^2))
									aic1 = AIC(lm1)
									
									#magnitude <- Mod(fft(tmpdf$y))
									#high_freq_magnitude1 <- mean(magnitude[(length(magnitude) / 2):length(magnitude)])
									high_freq_magnitude1 <- calculate_roughness(tmpdf$y)
									
									peaks <- try(peaks_count(tmpdf$x,tmpdf$y), silent = T)
									if (class(peaks) == "try-error") {
										peaks <- c(0,0)
								  	}
									peaks_num <- peaks[1]+peaks[2]
								}
							}
						  	#print(rmse1)
							#flush.console()
							#delta_abc1 = abs(max(tmpdf$y)-min(tmpdf$y))
							delta_abc1 = abs(max(tmp[,i])-min(tmp[,i]))
						}
						

						tmp <- feature_df[idx[1]:nrow(feature_df),]
						if ( nrow(tmp) > 2 )
						{
							monotonicity_value2 <- monotonicity(tmp[,i], nrow(tmp), eps = 0.0)

							tmpdf <- data.frame(x=c(1:nrow(tmp)),y=(tmp[,i]-min(tmp[,i]))/(max(tmp[,i])-min(tmp[,i])))
							tmpdf <- na.omit(tmpdf)
							if ( nrow(tmpdf) > 2 )
							{
								lm2 <- try(lm(y ~ x, data=tmpdf, na.action=na.exclude), silent = FALSE) 
								if (class(lm2) == "try-error") {
									lm2 <- NULL
							  	}
							  	if ( monotonicity_value1 > 0 )
							  	{
									if ( !is.null(lm2) && coef(lm2)[2] < 0 && abs(monotonicity_value2) > 0.1 && abs(coef(lm2)[2]) > 0.0001)
									{
										f_statistic <- summary(lm2)$fstatistic
										fvalue_p2 <- pf(f_statistic[1], f_statistic[2], f_statistic[3], lower.tail = FALSE)
										r_squared2 <- summary(lm2)$r.squared
										#sse <- sum((lm2$residuals)^2)
										#rmse2 <- sse
										rmse2 <- sqrt(mean((tmpdf$y -  predict(lm2, tmpdf))^2))
										aic2 = AIC(lm2)

										#magnitude <- Mod(fft(tmpdf$y))
										#high_freq_magnitude2 <- mean(magnitude[(length(magnitude) / 2):length(magnitude)])
										high_freq_magnitude2 <- calculate_roughness(tmpdf$y)
									}
								}else
								{
									if ( !is.null(lm2) && coef(lm2)[2] > 0 && abs(monotonicity_value2) > 0.1 && abs(coef(lm2)[2]) > 0.0001)
									{
										f_statistic <- summary(lm2)$fstatistic
										fvalue_p2 <- pf(f_statistic[1], f_statistic[2], f_statistic[3], lower.tail = FALSE)
										r_squared2 <- summary(lm2)$r.squared
										#sse <- sum((lm2$residuals)^2)
										#rmse2 <- sse
										rmse2 <- sqrt(mean((tmpdf$y -  predict(lm2, tmpdf))^2))
										aic2 = AIC(lm2)

										#magnitude <- Mod(fft(tmpdf$y))
										#high_freq_magnitude2 <- mean(magnitude[(length(magnitude) / 2):length(magnitude)])
										high_freq_magnitude2 <- calculate_roughness(tmpdf$y)
									}
								}
							  	#print(rmse2)
								#flush.console()
								#quit()
								#delta_abc2 = abs(max(tmpdf$y)-min(tmpdf$y))
								delta_abc2 = abs(max(tmp[,i])-min(tmp[,i]))
							}
						}
						
						if ( monotonicity_value1 == 0 || monotonicity_value2 == 0 )
						{
							monotonicity_value <- monotonicity(feature_df[,i], length(feature_df[,i]), eps = 0.0)
						}else
						{
							if ( monotonicity_value1 > 0 )
							{
								monotonicity_value = monotonicity_value1 - monotonicity_value2
							}else
							{
								monotonicity_value = -monotonicity_value1 + monotonicity_value2
							}
						}
					}
				}else
				{
					monotonicity_value <- monotonicity(feature_df[,i], length(feature_df[,i]), eps = 0.0)
					monotonicity_value1 = monotonicity_value
					tmpdf <- data.frame(x=c(1:nrow(feature_df)),y=(feature_df[,i]-min(feature_df[,i]))/(max(feature_df[,i])-min(feature_df[,i])))
					tmpdf <- na.omit(tmpdf)
					if ( nrow(tmpdf) > 2 )
					{
						lm1 <- try(lm(y ~ x, data=tmpdf, na.action=na.exclude), silent = FALSE) 
						if (class(lm1) == "try-error") {
							lm1 <- NULL
					  	}
						if ( !is.null(lm) && coef(lm1)[2] * monotonicity_value1 > 0  && abs(monotonicity_value1) > 0.1 && abs(coef(lm1)[2]) > 0.0001)
						{
							f_statistic <- summary(lm1)$fstatistic
							fvalue_p1 <- pf(f_statistic[1], f_statistic[2], f_statistic[3], lower.tail = FALSE)
							r_squared1 <- summary(lm1)$r.squared
							#sse <- sum((lm1$residuals)^2)
							#rmse1 <- sse
							rmse1 <- sqrt(mean((tmpdf$y -  predict(lm1, tmpdf))^2))
							rmse2 <- 0
							aic1 = AIC(lm1)
							aic2 = 0

							#magnitude <- Mod(fft(tmpdf$y))
							#high_freq_magnitude1 <- mean(magnitude[(length(magnitude) / 2):length(magnitude)])
							high_freq_magnitude1 <- calculate_roughness(tmpdf$y)
							high_freq_magnitude2 <- 0
							
							peaks <- try(peaks_count(tmpdf$x,tmpdf$y), silent = T)
							if (class(peaks) == "try-error") {
								peaks <- c(0,0)
						  	}
							peaks_num <- peaks[1]+peaks[2]
							
						}
						#delta_abc1 = abs(max(tmpdf$y)-min(tmpdf$y))
						delta_abc1 = abs(max(feature_df[,i])-min(feature_df[,i]))
						delta_abc2 = 0
					}
				}
				if ( delta_abc2 > delta_abc1 && delta_abc2 > 0.0000001)
				{
					delta_abc = delta_abc1/delta_abc2
				}
				if ( delta_abc1 > delta_abc2 && delta_abc1 > 0.0000001)
				{
					delta_abc = delta_abc2/delta_abc1
				}
				delta_abc = (1 - delta_abc)*0.1
				if ( delta_abc < 0 )
				{
					delta_abc = 999999.0
				}
				
				monotonicity_value_sigin = 0
				if ( monotonicity_value1 < 0 )
				{
					monotonicity_value_sigin = -1*sigin
				}else
				{
					monotonicity_value_sigin = 1*sigin
				}
								
				z <- lowess(feature_df$time_index,  feature_df[,i], f = smoother_span_list[kkk])$y
				
				ylable <- sprintf("%s window=%d_slide=%d_smooth_window=%d_smooth_window_slide=%d_smooth_window2=%d_smooth_window_slide2=%d_smoother_span:%f", colnames(feature_df)[i], lookback, lookback_slide,smooth_window, smooth_window_slide, smooth_window2, smooth_window_slide2,smoother_span_list[kkk])

				cat("colnames(feature_df)[i]")
				print(colnames(feature_df)[i])
				col <- strsplit(colnames(feature_df)[i], "[.]")
				col <- col[[1]][1]
				
				#cat("col")
				#print(col)
				flush.console()
				p1 <- NULL
				p2 <- NULL
				if ( col == "mahalanobis")
				{
					p1 <- feature_df %>% 
					  ggplot(aes(x = time_index, y = feature_df[,i]*monotonicity_value_sigin))+
					  geom_line(linewidth =1.0)+ylab(colnames(feature_df)[i])+
					  geom_line(aes(x = time_index, y = z*monotonicity_value_sigin),linewidth =1.2, color ="red")
				}else
				{
					p1 <- feature_df %>% 
					  ggplot(aes(x = time_index, y = feature_df[,i]*monotonicity_value_sigin))+
					  geom_line(linewidth =1.0)+
					  geom_line(aes(x = time_index, y = z*monotonicity_value_sigin),linewidth =1.2, color ="red")
					  
					#p2 <- df2 %>% 
					#  ggplot(aes(x = time_index, y = df2[,col]))+
					#  geom_line(linewidth =1.0, color ="gray")+ ggtitle(colnames(feature_df)[i])
				}
			
				if ( length(maintenance_flag_time_index) >= 1 && maintenance_flag_time_index > 0)
				{
					for ( s in 1:length(maintenance_flag_time_index))
					{
						p1 <- p1 + geom_vline(xintercept =  maintenance_flag_time_index[s],linewidth =1.0, color ="#191970")
					}
				}else
				{
						p1 <- p1 + geom_vline(xintercept =  feature_df$time_index[nrow(feature_df)-1],linewidth =1.0, color ="#191970")
				}

				p1 <- p1 + ylab(colnames(feature_df)[i])+ ggtitle(ylable)
				
				p1 <- p1 + annotate("text",x=mean(range(feature_df$time_index)),y=-Inf,label=sprintf("monotonicity_value:%f",monotonicity_value),vjust=-.4)

				print(p1)
				
				num = 0
				if ( is.null(xxxx))
				{
					num = 0
				}else 
				{
					num = nrow(xxxx)
				}
				
				base = sprintf("%06d_%s_feature(%s)", num, base_name, colnames(feature_df)[i])
				filename <- sprintf("../images/%s.png", base)
				filename_r <- sprintf("../images/%s.r", base)

				if (!is.null(p2))
				{
					p <- gridExtra::grid.arrange(p1, p2, nrow = 2)
				}else
				{
					p <- p1
				}
				ggsave(filename=filename, p, limitsize=F, width = 16, height = 9)
				plot(p)
	
				threshold_target = max(feature_df[,i]*monotonicity_value_sigin)-0.05*(max(feature_df[,i]*monotonicity_value_sigin)-min(feature_df[,i]*monotonicity_value_sigin))
				
				sink(filename_r)
				cat(sprintf("lookback = %d\n", lookback))
				cat(sprintf("lookback_slide = %d\n", lookback_slide))
				cat(sprintf("smooth_window = %d\n", smooth_window))
				cat(sprintf("smooth_window_slide = %d\n", smooth_window_slide))
				cat(sprintf("smooth_window2 = %d\n", smooth_window2))
				cat(sprintf("smooth_window_slide2 = %d\n", smooth_window_slide2))
				cat(sprintf("smoother_span = %f\n", smoother_span_list[kkk]))
				cat(sprintf("sigin = %s\n", monotonicity_value_sigin))
				
				cat(sprintf("threshold_target = %f\n", threshold_target))
				cat(sprintf("RMSE12 = %f\n", rmse1+rmse2 + 10*delta_abc + 10*high_freq_magnitude1+high_freq_magnitude2))
				cat(sprintf("RMSE1 = %f\n", rmse1))+
				cat(sprintf("RMSE2 = %f\n", rmse2))
				cat(sprintf("delta_abc = %f\n", delta_abc))
				cat(sprintf("r_squared1 = %f\n", r_squared1))
				cat(sprintf("r_squared2 = %f\n", r_squared2))
				cat(sprintf("fvalue_p1 = %f\n", fvalue_p1))
				cat(sprintf("fvalue_p2 = %f\n", fvalue_p2))
				cat(sprintf("AIC1 = %f\n", aic1))
				cat(sprintf("AIC2 = %f\n", aic2))
				cat(sprintf("high_freq_magnitude1  = %f\n", high_freq_magnitude1))
				cat(sprintf("high_freq_magnitude2  = %f\n", high_freq_magnitude2))
				cat(sprintf("peaks_num  = %f\n", peaks_num))
				
				sink()
				
				rowm <- data.frame(
					id 					= c(num),
					monotonicity		= c(abs(monotonicity_value)),
					feature				= c(colnames(feature_df)[i]),
					lookback			= c(lookback),
					lookback_slide		= c(lookback_slide),
					smooth_window		= c(smooth_window),
					smooth_window_slide	= c(smooth_window_slide),
					smooth_window2		= c(smooth_window2),
					smooth_window_slide2= c(smooth_window_slide2),
					sigin				= c(monotonicity_value_sigin),
					max					= max((feature_df)[i]),
					min					= min((feature_df)[i]),
					image				= sprintf("=HYPERLINK(\"%s.png\")", base),
					filename_r			= c(base),
					rmse12				= c(rmse1+rmse2 + 10*delta_abc + 10*high_freq_magnitude1+high_freq_magnitude2)
				)

				plt_list[num+1] <- p
				if ( is.null(xxxx))
				{
					xxxx <- rowm
				}else
				{
					xxxx <- rbind(xxxx,  rowm)
				}
				print(nrow(xxxx))
			}
		}
		if ( summary ) break
	}
	xxxx <- xxxx[order((xxxx$rmse12), decreasing=F),]
	#xxxx <- xxxx[order((xxxx$monotonicity), decreasing=T),]
	write.csv(xxxx, '../images/feature_summarys.csv', row.names = F, fileEncoding = "CP932")
	

	if ( summary )
	{
		x <- feature_df[1:nrow(feature_df),]
		x <- reshape2::melt(x, id.vars=c("time_index"), measure.vars=colnames(x)[2:length(x)], 
						variable.name="key",value.name="target")
		
		pltlist <- NULL	
		for ( i in 1:length(colnames(df2)))
		{
			if ( colnames(df2)[i] == "time_index" )
			{
				next
			}
			ptn <- paste("^", colnames(df2)[i],sep='') 
			p <- x %>% filter(str_detect(key,ptn)) %>%
			  ggplot(aes(x = time_index, y = target, color=key))+
			  geom_line()
			pltlist <- c(pltlist, list(p))
		}

		n = find_closest_factors(length(pltlist))
		if ( n[1] == 1 && length(pltlist) > 1) n = find_closest_factors(length(pltlist)+1)
		
		plt <- plot_grid(plotlist = pltlist, nrows = n[1])

		print(plt)
		ggsave(filename=paste(base_name,"_tracking_feature2.png",sep=''), plt, limitsize=F, width = 16, height = 9)
		n = find_closest_factors(length(pltlist))
		if ( n[1] == 1 && length(pltlist) > 1) n = find_closest_factors(length(pltlist)+1)
		gg_plotly <- plotly::subplot(pltlist, nrows = n[2])
		#gg_plotly <- plotly::subplot(pltlist, nrows = length(pltlist))
		print(gg_plotly)
		htmlwidgets::saveWidget(as_widget(gg_plotly), paste(base_name,"_feature_summary_visualization2.html",sep=''), selfcontained = F)
	}

	feature_summary_visualization_end <- Sys.time()
	feature_summary_visualization_start_end <- as.numeric(difftime(feature_summary_visualization_end, feature_summary_visualization_start, units = "sec"))
	print(sprintf("feature_summary_visualization Time:%f sec( %f min)( %f hour)", feature_summary_visualization_start_end, feature_summary_visualization_start_end/60, feature_summary_visualization_start_end/(60*60)))
}

