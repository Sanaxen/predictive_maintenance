options(encoding = "utf-8")
options(digits.secs=3)

library(ggplot2)
library(zoo)
library(plotly)

#parameters <- paste("./", base_name,"parameters.r", sep="")
source("./work/parameters.r")

# Get .txt files in the specified folder
folder_path <- "./images"
files <- list.files(
  path = folder_path,
  pattern = "\\.txt$",      # For files ending in .txt
  full.names = TRUE         # Get full path
)

# Get file information
info <- file.info(files)

# Sort by file creation date and time (ctime) from oldest to newest.
files_sorted <- files[order(info$ctime)]

## Show results
#files_sorted

rul_csv <- paste("./", base_name,"_RUL.csv", sep="")
sink(rul_csv, split = TRUE)
cat("cycle, timestmp, unit, percent5, percent50, percent95\n")
# Process each file
for (file in files) {
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

df <- read.csv(rul_csv, header = TRUE, stringsAsFactors = FALSE)
df$timestmp <- as.POSIXct(df$timestmp, tz="UTC", origin="2024-01-01")
df$unit <- unit

cols_to_fill <- c("percent5", "percent50", "percent95")
df[cols_to_fill] <- lapply(df[cols_to_fill], function(x) {
  na.approx(x, na.rm = FALSE)
})

df$percent5_timestmp <- df$timestmp
df$percent50_timestmp <- df$timestmp
df$percent95_timestmp <- df$timestmp


unit_name <- function(u)
{
	if ( u == "day" ) units = "days"
	if ( u == "h" ) units = "hours"
	if ( u == "min" ) units = "mins"
	if ( u == "sec" ) units = "secs"
	
	return( units)
}

for ( i in 1:nrow(df))
{
	if ( !is.na(df$percent5[i]) )
	{
		u <- df$unit[i]
		t <- as.numeric(df$percent5[i])
		#print(u)
		#print(t)
		#print(unit_name(u))
		#print(df$timestmp[i])
		df$percent5_timestmp[i] <- df$timestmp[i] + as.difftime(t, units = unit_name(u))
		#print(df$percent5_timestmp[i])
	}else
	{
		df$percent5_timestmp[i] <- NA
	}
	if ( !is.na(df$percent50[i]) )
	{
		u <- df$unit[i]
		t <- as.numeric(df$percent50[i])
		df$percent50_timestmp[i] <- df$timestmp[i] + as.difftime(t, units = unit_name(u))
	}else
	{
		df$percent50_timestmp[i] <- NA
	}
	if ( !is.na(df$percent95[i]) )
	{
		u <- df$unit[i]
		t <- as.numeric(df$percent95[i])
		df$percent95_timestmp[i] <- df$timestmp[i] + as.difftime(t, units = unit_name(u))
	}else
	{
		df$percent95_timestmp[i] <- NA
	}
}
df$percent5_timestmp <- as.POSIXct(df$percent5_timestmp, tz="UTC", origin="2024-01-01")
df$percent50_timestmp <- as.POSIXct(df$percent50_timestmp, tz="UTC", origin="2024-01-01")
df$percent95_timestmp <- as.POSIXct(df$percent95_timestmp, tz="UTC", origin="2024-01-01")


plt <- ggplot( df, aes(x=cycle))
plt <- plt + geom_line(aes(y=percent5, color="percent5"),linewidth =1.0,na.rm = TRUE)
plt <- plt + geom_line(aes(y=percent50, color="percent50"),linewidth =1.0,na.rm = TRUE)
plt <- plt + geom_line(aes(y=percent95, color="percent95"),linewidth =1.0,na.rm = TRUE)
plt <- plt + labs(y = paste("RUL [ ", unit, " ]", sep=""))
plt <- plt + geom_hline(yintercept = 0, color = "red", size = 1)

plt <- plt + geom_text(aes(x=cycle,y=percent5,label = percent5, color="percent5"), vjust = -0.25, size = 2.5 ,show.legend = FALSE, na.rm = TRUE)
plt <- plt + geom_text(aes(x=cycle,y=percent50,label = percent50, color="percent50"), vjust = -0.25, size = 2.5 ,show.legend = FALSE, na.rm = TRUE)
plt <- plt + geom_text(aes(x=cycle,y=percent95,label = percent95, color="percent95"), vjust = -0.25, size = 2.5 ,show.legend = FALSE, na.rm = TRUE)
plt

n = 3
step <- nrow(df)/n
break_pos <- c()
labels <- c()
for ( i in 1:(n+1) )
{
	k = (i-1)*step+1
	if ( k > nrow(df))
	{
		k = nrow(df)
	}
	break_pos <- c(break_pos, df$cycle[k])
	labels <- c(labels, as.character(df$timestmp[k]))
}

plt <- plt +  scale_x_continuous(breaks = break_pos, labels = labels)
plt <- plt + theme(axis.title.x = element_text(size = 28))
plt <- plt + theme(axis.title.y = element_text(size = 28))
plt <- plt + theme(axis.text.x = element_text(size = 18))
plt <- plt + theme(axis.text.y = element_text(size = 18))
plt


ggsave(file = paste("./", base_name, "_RUL.png", sep=""), plot = plt, dpi = 130, width = 14*1.5, height = 6.8*1.4)

plt <- ggplotly(plt)
print(plt)
htmlwidgets::saveWidget(as_widget(plt), paste("./work/",base_name,"_RUL.html",sep=''), selfcontained = F)


rul_csv <- paste("./", base_name,"_RUL_output.csv", sep="")
write.csv(df, rul_csv, row.names = FALSE)

