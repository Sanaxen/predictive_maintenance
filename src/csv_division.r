curdir = getwd()
.libPaths(c('./library',.libPaths()))
library(data.table)


read_csv_file <- function( csv, csv_encoding= "utf-8")
{
	print(sprintf("read_csv_file(%s)", file))
	print(sprintf("csv_encoding=%s", csv_encoding))
	
	df <- NULL
	if ( csv_encoding == "sjis" )
	{
		df <- try(
			read.csv( file, header=T, stringsAsFactors = F, na.strings = c("", "NA"), fileEncoding  = 'Shift_JIS')
		,silent=F)
		if ( class(df) == "try-error" || is.null(df) == T|| nrow(df) == 0)
		{
			df <- fread(file, na.strings=c("", "NULL"), header = TRUE, stringsAsFactors = TRUE)
		}
	}else
	{
		df <- try(
			fread(file, na.strings=c("", "NULL"), header = TRUE, stringsAsFactors = TRUE)
		,silent=F)
		print(df)
		if ( class(df) == "try-error" || is.null(df) == T || nrow(df) == 0)
		{
			df <- read.csv( file, header=T, stringsAsFactors = F, na.strings = c("", "NA"), fileEncoding  = 'utf-8')
		}
	}
	df <- as.data.frame(df)
	print(colnames(df))
	colnames(df) <- gsub("\\.", "_", colnames(df))
	print(colnames(df))

	print(head(df))
	print(sprintf("get_data_frame ncol(df):%d", ncol(df)))	
	flush.console()

	return (df)
}

csv_division <- function(csv, size, enc="utf-8")
{
	csv_encoding= enc

	if ( ! file.exists("files") )
	{
		dir.create("files")
	}

	if ( ! file.exists(csv) )
	{
		return (0)
	}
	FN <- list.files("./files", pattern="\\.csv$")
	
	if ( length(FN) > 0 )
	{
		file.remove(paste("./files/",FN,sep=""))
	}
	
	df <- read_csv_file(csv, csv_encoding=csv_encoding)
	
	sink("./files/Emulate.bat")
	cat("set cpy=cpoy-path-name\n")
	cat("set sleep=10\n\n\n")
	
	filelist <- NULL
	i = 1
	s = 1
	while( T )
	{	
		e = s + size
		
		if ( e > nrow(df))
		{
			e = nrow(df)
		}

		df2 <- df[s:e,]
		df2 <- as.data.frame(df2)

		file <- sprintf("./files/%d_%d-%d.csv", i, s, e)
		cat("copy ")
		cat(sprintf("%d_%d-%d.csv", i, s, e))
		cat(" %cpy% /v /y\n")
		cat("timeout /T %sleep%\n")
		
		#print(sprintf("%d->%d nrow:%d", ((i-1)*one_input+1), (i*one_input), nrow(df2)))
		if ( csv_encoding == "sjis" )
		{
			write.csv(df2, file, row.names = F, fileEncoding  = 'Shift_JIS')
		}else
		{
			write.csv(df2, file, row.names = F, fileEncoding  = 'utf-8')
		}
		s = e + 1
		if ( e == nrow(df))
		{
			break
		}
		i = i + 1
	}
	sink()
	
	return(1)
}


#csv_division(file, size)

