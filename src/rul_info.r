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
		return
	}
	cat("valid_unit:")
	print(valid_unit)

	cols_to_fill <- c("percent5", "percent50", "percent95")
	cur_rul_df[cols_to_fill] <- lapply(cur_rul_df[cols_to_fill], function(x) {
	  na.approx(x, na.rm = FALSE)
	})

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


	if ( sum(!is.na(cur_rul_df$percent5)) >= 2)
	{
		cur_rul_plt <- ggplot( cur_rul_df, aes(x=cycle))
		cur_rul_plt <- cur_rul_plt + geom_line(aes(y=percent5, color="percent5"),linewidth =1.0,na.rm = TRUE)
		cur_rul_plt <- cur_rul_plt + geom_line(aes(y=percent50, color="percent50"),linewidth =1.0,na.rm = TRUE)
		cur_rul_plt <- cur_rul_plt + geom_line(aes(y=percent95, color="percent95"),linewidth =1.0,na.rm = TRUE)
		cur_rul_plt <- cur_rul_plt + labs(y = paste("RUL [ ", unit, " ]", sep=""))
		cur_rul_plt <- cur_rul_plt + geom_hline(yintercept = 0, color = "red", size = 1)

		cur_rul_plt <- cur_rul_plt + geom_text(aes(x=cycle,y=percent5,label = percent5, color="percent5"), vjust = -0.25, size = 2.5 ,show.legend = FALSE, na.rm = TRUE)
		cur_rul_plt <- cur_rul_plt + geom_text(aes(x=cycle,y=percent50,label = percent50, color="percent50"), vjust = -0.25, size = 2.5 ,show.legend = FALSE, na.rm = TRUE)
		cur_rul_plt <- cur_rul_plt + geom_text(aes(x=cycle,y=percent95,label = percent95, color="percent95"), vjust = -0.25, size = 2.5 ,show.legend = FALSE, na.rm = TRUE)
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
			cur_rul_plt <- ggplotly(cur_rul_plt)
			print(cur_rul_plt)
			htmlwidgets::saveWidget(as_widget(cur_rul_plt), paste("./",base_name,"_RUL.html",sep=''), selfcontained = F)
		}
	}

	rul_csv <- paste("../", base_name,"_RUL_output.csv", sep="")
	write.csv(cur_rul_df, rul_csv, row.names = FALSE)
}


rul_curve_plot(-1)
