options(encoding = "utf-8")
options(digits.secs=3)

if ("ggplot2" %in% .packages()) {
  message("ggplot2 is already loaded")
} else {
	library(ggplot2)
}
if ("zoo" %in% .packages()) {
  message("zoo is already loaded")
} else {
	library(zoo)
}
if ("plotly" %in% .packages()) {
  message("plotly is already loaded")
} else {
	library(plotly)
}
if ("dplyr" %in% .packages()) {
  message("dplyr is already loaded")
} else {
	library(dplyr)
}
if ("gridExtra" %in% .packages()) {
  message("gridExtra is already loaded")
} else {
	library(gridExtra)
}


setwd( paste(getwd(), "/work", sep=""))

if (!exists("base_name")) {
	source("parameters.r")
}

conv_unit_name <- function(u)
{
	if ( u == "day" ) units = "days"
	if ( u == "h" ) units = "hours"
	if ( u == "min" ) units = "mins"
	if ( u == "sec" ) units = "secs"
	
	return( units)
}

conv_unit_name2 <- function(u)
{
	if ( u == "day" ) units = "hours"
	if ( u == "h" ) units = "mins"
	if ( u == "min" ) units = "secs"
	if ( u == "sec" ) units = "secs"
	
	return( units)
}

rul_curve_plot <- function(index_number=-1)
{
	# Get .txt files_list in the specified folder
	folder_path <- "../images"
	files_list <- list.files(
	  path = folder_path,
	  pattern = "\\.txt$",      # For files_list ending in .txt
	  full.names = TRUE         # Get full path
	)

	# Get file information
	info <- file.info(files_list)

	# Sort by file creation date and time (ctime) from oldest to newest.
	files_sorted <- files_list[order(info$ctime)]

	## Show results
	#files_sorted

	rul_csv <- paste("../", base_name,"_RUL.csv", sep="")
	sink(rul_csv, split = TRUE)
	cat("cycle, timestmp, unit, percent5, percent50, percent95\n")
	# Process each file
	
	valid_unit <- ""
	for (file in files_list) {
		## Print the file name (for debugging or verification)
		#message("Processing: ", file)

		# Read a text file line by line
		lines <- readLines(file, encoding = "UTF-8")
		
		cycle <- ""
		timestmp <- ""
		unit <- ""
		p05 <- ""
		p50 <- ""
		p95 <- ""

		split_result2 <- strsplit(lines[1], ",")
		cycle <- split_result2[[1]][1]
		timestmp <- split_result2[[1]][2]
		
		pattern <- "\\s|\\[|\\]"
		for ( i in 2:length(lines))
		{
			if ( lines[i] == "" ) 
			{
				next
			}
			if ( lines[i] == "------------------------" ) 
			{
				next
			}

			res <- grepl("step 5%", lines[i])
			if ( res )
			{
				split_result2 <- strsplit(lines[i], pattern)

				#unit
				unit <- split_result2[[1]][5]
				if ( unit != "" && !is.na(unit))
				{
					valid_unit <- unit
				}

				#5% RUL
				p05 <- split_result2[[1]][4]
			}
			res <- grepl("50%", lines[i])
			if ( res )
			{
				split_result2 <- strsplit(lines[i], pattern)
				#50% RUL
				p50 <- split_result2[[1]][2]
			}
			res <- grepl("95%", lines[i])
			if ( res )
			{
				split_result2 <- strsplit(lines[i], pattern)
				#95% RUL
				p95 <- split_result2[[1]][6]
			}
		}
		cat(sprintf("%s,%s,%s,%s,%s,%s\n", cycle, timestmp, unit, p05, p50, p95))
	}
	sink()

	cur_rul_df <- read.csv(rul_csv, header = TRUE, stringsAsFactors = FALSE)
	cur_rul_df$timestmp <- as.POSIXct(cur_rul_df$timestmp, tz="UTC", origin="2024-01-01")
	cur_rul_df$unit <- valid_unit
	if ( sum(!is.na(cur_rul_df$percent5)) < 2)
	{
		#cat("sum(!is.na(cur_rul_df$percent5))")
		#print(sum(!is.na(cur_rul_df$percent5)))
		return(1)
	}
	#cat("---sum(!is.na(cur_rul_df$percent5))")
	#print(sum(!is.na(cur_rul_df$percent5)))
	cat("valid_unit:")
	print(valid_unit)
	if ( valid_unit == "" ) 
	{
		return(1)
	}
	#print(valid_unit)
	
	cols_to_fill <- c("percent5", "percent50", "percent95")
	cur_rul_df[cols_to_fill] <- lapply(cur_rul_df[cols_to_fill], function(x) {
	  na.approx(x, na.rm = FALSE)
	})
	print(cur_rul_df)

	cur_rul_df$percent5_timestmp <- cur_rul_df$timestmp
	cur_rul_df$percent50_timestmp <- cur_rul_df$timestmp
	cur_rul_df$percent95_timestmp <- cur_rul_df$timestmp

	for ( i in 1:nrow(cur_rul_df))
	{
		if ( !is.na(cur_rul_df$percent5[i]) )
		{
			u <- cur_rul_df$unit[i]
			t <- as.numeric(cur_rul_df$percent5[i])
			#print(u)
			#print(t)
			#print(conv_unit_name(u))
			#print(cur_rul_df$timestmp[i])
			cur_rul_df$percent5_timestmp[i] <- cur_rul_df$timestmp[i] + as.difftime(t, units = conv_unit_name(u))
			#print(cur_rul_df$percent5_timestmp[i])
		}else
		{
			cur_rul_df$percent5_timestmp[i] <- NA
		}
		if ( !is.na(cur_rul_df$percent50[i]) )
		{
			u <- cur_rul_df$unit[i]
			t <- as.numeric(cur_rul_df$percent50[i])
			cur_rul_df$percent50_timestmp[i] <- cur_rul_df$timestmp[i] + as.difftime(t, units = conv_unit_name(u))
		}else
		{
			cur_rul_df$percent50_timestmp[i] <- NA
		}
		if ( !is.na(cur_rul_df$percent95[i]) )
		{
			u <- cur_rul_df$unit[i]
			t <- as.numeric(cur_rul_df$percent95[i])
			cur_rul_df$percent95_timestmp[i] <- cur_rul_df$timestmp[i] + as.difftime(t, units = conv_unit_name(u))
		}else
		{
			cur_rul_df$percent95_timestmp[i] <- NA
		}
	}
	cur_rul_df$percent5_timestmp <- as.POSIXct(cur_rul_df$percent5_timestmp, tz="UTC", origin="2024-01-01")
	cur_rul_df$percent50_timestmp <- as.POSIXct(cur_rul_df$percent50_timestmp, tz="UTC", origin="2024-01-01")
	cur_rul_df$percent95_timestmp <- as.POSIXct(cur_rul_df$percent95_timestmp, tz="UTC", origin="2024-01-01")


	cur_rul_plt <- NULL
	if ( sum(!is.na(cur_rul_df$percent5)) >= 2)
	{
		cur_rul_plt <- ggplot( cur_rul_df, aes(x=cycle))
		cur_rul_plt <- cur_rul_plt + geom_ribbon(aes(x=cycle, ymin = percent5, ymax = percent95), fill="lightblue", alpha = 0.3,na.rm = TRUE)

		cur_rul_plt <- cur_rul_plt + geom_line(aes(y=percent5, color="percent5"),linewidth =1.0,na.rm = TRUE)
		cur_rul_plt <- cur_rul_plt + geom_line(aes(y=percent50, color="percent50"),linewidth =1.0,na.rm = TRUE)
		cur_rul_plt <- cur_rul_plt + geom_line(aes(y=percent95, color="percent95"),linewidth =1.0,na.rm = TRUE)
		cur_rul_plt <- cur_rul_plt + labs(y = paste("RUL [ ", valid_unit, " ]", sep=""))
		cur_rul_plt <- cur_rul_plt + geom_hline(yintercept = 0, color = "red", size = 1)

		#cur_rul_plt <- cur_rul_plt + geom_text(aes(x=cycle,y=percent5,label = percent5, color="percent5"), vjust = -0.25, size = 2.5 ,show.legend = FALSE, na.rm = TRUE)
		#cur_rul_plt <- cur_rul_plt + geom_text(aes(x=cycle,y=percent50,label = percent50, color="percent50"), vjust = -0.25, size = 2.5 ,show.legend = FALSE, na.rm = TRUE)
		#cur_rul_plt <- cur_rul_plt + geom_text(aes(x=cycle,y=percent95,label = percent95, color="percent95"), vjust = -0.25, size = 2.5 ,show.legend = FALSE, na.rm = TRUE)
		
		cur_rul_plt

		n = 3
		step <- nrow(cur_rul_df)/n
		break_pos <- c()
		labels <- c()
		for ( i in 1:(n+1) )
		{
			k = (i-1)*step+1
			if ( k > nrow(cur_rul_df))
			{
				k = nrow(cur_rul_df)
			}
			break_pos <- c(break_pos, cur_rul_df$cycle[k])
			labels <- c(labels, as.character(cur_rul_df$timestmp[k]))
		}

		cur_rul_plt <- cur_rul_plt +  scale_x_continuous(breaks = break_pos, labels = labels)
		cur_rul_plt <- cur_rul_plt + theme(axis.title.x = element_text(size = 28))
		cur_rul_plt <- cur_rul_plt + theme(axis.title.y = element_text(size = 28))
		cur_rul_plt <- cur_rul_plt + theme(axis.text.x = element_text(size = 18))
		cur_rul_plt <- cur_rul_plt + theme(axis.text.y = element_text(size = 18))
		cur_rul_plt

		file = paste("../", base_name, "_RUL.png", sep="")
		ggsave(file = file, plot = cur_rul_plt, dpi = 130, width = 14*1.5, height = 6.8*1.4)
		if ( index_number > 0 )
		{
			if (!dir.exists("../images/RUL")) {
			  dir.create("../images/RUL")
			}
			file <- sprintf("../images/RUL/%s_RUL%06d.png", base_name, index_number)
			ggsave(file = file, plot = cur_rul_plt, dpi = 130, width = 14*1.5, height = 6.8*1.4)
			
		}else
		{
			cur_rul_pltly <- ggplotly(cur_rul_plt)
			print(cur_rul_pltly)
			htmlwidgets::saveWidget(as_widget(cur_rul_pltly), paste("./",base_name,"_RUL.html",sep=''), selfcontained = F)
		}
	}

	rul_csv <- paste("../", base_name,"_RUL_output.csv", sep="")
	write.csv(cur_rul_df, rul_csv, row.names = FALSE)
# ---------------------------------------------------------------------------------------------	
	
	if (is.null(cur_rul_plt))
	{
		return(1)
	}
	
	pred_min_time_stamp <- min(cur_rul_df$percent5_timestmp,na.rm = TRUE)
	pred_max_time_stamp <- max(cur_rul_df$percent95_timestmp,na.rm = TRUE)
	
	print(pred_min_time_stamp)
	print(pred_max_time_stamp)
	
	len <- difftime(pred_max_time_stamp, pred_min_time_stamp, units = conv_unit_name2(valid_unit))
	ds <- seq(as.POSIXlt(pred_min_time_stamp), by = conv_unit_name2(valid_unit), length.out = len+1)
	
	rul_hist <- data.frame(time=ds, probability=numeric(length(ds)))
	
	rul_hist
	
	for ( i in 1:length(ds) )
	{
		row1 = -1
		row2 = -1
		if ( !is.na(cur_rul_df$percent5_timestmp[i]))
		{
			diff <- difftime(rul_hist$time, cur_rul_df$percent5_timestmp[i], units = conv_unit_name(valid_unit))
		
			row1 =  which.min( abs(diff))
		}
		if ( !is.na(cur_rul_df$percent95_timestmp[i]))
		{
			diff <- difftime(rul_hist$time, cur_rul_df$percent50_timestmp[i], units = conv_unit_name(valid_unit))
		
			row2 =  which.min( abs(diff))
		}
		if ( row1 > 0 && row2 > 0 )
		{
			rul_hist$probability[row1:row2] <- rul_hist$probability[row1:row2] + 1
		}
	}
	

	
	fmt = "%Y-%m-%d %H:%M:%S"
	if ( valid_unit == "day" )
	{
		fmt = "%Y-%m-%d"
	}
	if ( valid_unit == "h" )
	{
		fmt = "%Y-%m-%d %H"
	}
	if ( valid_unit == "min" )
	{
		fmt = "%Y-%m-%d %H:%M"
	}

	n = 5
	step <- nrow(rul_hist)/n
	break_pos <- c()
	labels <- c()
	for ( i in 1:(n+1) )
	{
		k = (i-1)*step+1
		if ( k > nrow(rul_hist))
		{
			k = nrow(rul_hist)
		}
		break_pos <- c(break_pos, rul_hist$time[k])
		labels <- c(labels, as.character(format(rul_hist$time[k], fmt)))
	}
	#break_pos <- (as.factor(break_pos))
	#rul_hist$time <-(as.factor(rul_hist$time))
	
	rul_hist_org <- rul_hist
	rul_hist$probability <- 100*rul_hist$probability/sum(rul_hist$probability)
	
	hist_plt <- ggplot(rul_hist, aes(x=time, y=probability))
	hist_plt <- hist_plt + geom_bar(stat = "identity")
	
	hist_plt <- hist_plt +  scale_x_continuous(breaks = break_pos, labels = labels)
	#hist_plt <- hist_plt +  scale_x_discrete(breaks = break_pos, labels = labels)
	
	hist_plt <- hist_plt + ylab("probability[%]")
	hist_plt <- hist_plt + theme(axis.title.x = element_text(size = 20))
	hist_plt <- hist_plt + theme(axis.title.y = element_text(size = 20))
	hist_plt <- hist_plt + theme(axis.text.x = element_text(size = 10)) 
	hist_plt <- hist_plt + theme(axis.text.y = element_text(size = 10))

	hist_plt1 <- hist_plt

	rul_hist <- rul_hist[order(rul_hist$probability, decreasing = TRUE), ]
	rul_hist$order <- c(1:nrow(rul_hist))
	
	n = 5
	step <- nrow(rul_hist)/n
	break_pos <- c()
	labels <- c()
	for ( i in 1:(n+1) )
	{
		k = (i-1)*step+1
		if ( k > nrow(rul_hist))
		{
			k = nrow(rul_hist)
		}
		break_pos <- c(break_pos, rul_hist$order[k])
		labels <- c(labels, as.character(format(rul_hist$time[k], fmt)))
	}	
	hist_plt <- ggplot(rul_hist, aes(x=order, y=probability))
	hist_plt <- hist_plt + geom_bar(stat = "identity")
	
	hist_plt <- hist_plt +  scale_x_continuous(breaks = break_pos, labels = labels)
	
	hist_plt <- hist_plt + ylab("probability[%]")
	hist_plt <- hist_plt + theme(axis.title.x = element_text(size = 20))
	hist_plt <- hist_plt + theme(axis.title.y = element_text(size = 20))
	hist_plt <- hist_plt + theme(axis.text.x = element_text(size = 10)) 
	hist_plt <- hist_plt + theme(axis.text.y = element_text(size = 10))
	hist_plt
	hist_plt2 <- hist_plt
	
	rul_hist2 <- rul_hist_org[rul_hist_org$probability > max(rul_hist_org$probability)-0.2*(max(rul_hist_org$probability)-min(rul_hist_org$probability)),]
	rul_hist2 <- rul_hist_org[order(rul_hist_org$probability, decreasing = TRUE), ]
	
	rul_hist3 <- NULL
	for ( i in 5:nrow(rul_hist2))
	{
		rul_hist3 <- rul_hist2[1:i,]
		if ( length(unique(rul_hist3$probability)) > 3 ) break
	}
	
	rul_hist3$time <- as.factor(format(rul_hist3$time, "%Y-%m-%d"))
	while ( length(unique(rul_hist3$time)) > 6 )
	{
		if ( nrow(rul_hist3) > 6 )
		{
			rul_hist3 <- rul_hist3 %>% slice_tail(n = nrow(rul_hist3)-2)
		}else
		{
			break
		}	
	}
	
	hist_plt <- ggplot(rul_hist3, aes(x=time))
	hist_plt <- hist_plt + geom_histogram(stat="count")
	hist_plt <- hist_plt + theme(axis.title.x = element_text(size = 20))
	hist_plt <- hist_plt + theme(axis.title.y = element_text(size = 20))
	hist_plt <- hist_plt + theme(axis.text.x = element_text(size = 10)) 
	hist_plt <- hist_plt + theme(axis.text.y = element_text(size = 10))
	hist_plt
	hist_plt3 <- hist_plt
	
	layout1 <- rbind(c(1, 1, 1),
	                 c(2, 3, 4))
	plt <- gridExtra::grid.arrange(cur_rul_plt, hist_plt1, hist_plt2, hist_plt3, layout_matrix = layout1, top = "--")
	if ( index_number > 0 )
	{
		if (!dir.exists("../images/RUL")) {
		  dir.create("../images/RUL")
		}
		file <- sprintf("../images/RUL/%s_RUL%06d.png", base_name, index_number)
		ggsave(file = file, plot = plt, dpi = 130, width = 14*1.5, height = 6.8*1.4)
	}else
	{
		#plt_pltly <- ggplotly(cur_rul_plt)
		#print(plt_pltly)
		#htmlwidgets::saveWidget(as_widget(plt_pltly), paste("./",base_name,"_RUL.html",sep=''), selfcontained = F)
	}
	file = paste("../", base_name, "_RUL.png", sep="")
	ggsave(file = file, plot = plt, dpi = 130, width = 14*1.5, height = 6.8*1.4)
	
}


rul_curve_plot(-1)
