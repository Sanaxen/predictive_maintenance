
lib="C:/Users/yamato/Desktop/work/predictive_maintenance-main/predictive_maintenance-main/tmp/lib"
.libPaths( c(lib, .libPaths()))

library(R.matlab)

files="mill/mill.mat"
mat <- readMat( files)

df<-data.frame(mat)

df2 <- data.frame(df[[1]])
df3 <- data.frame(
	case=df2$case,
	run=df2$run,
	VB=df2$VB,
	time=df2$time,
	DOC=df2$DOC,
	feed=df2$feed,
	material=df2$material,
	smcAC=df2$smcAC,
	smcDC=df2$smcDC,
	vib.table=df2$vib.table,
	vib.spindle=df2$vib.spindle,
	AE.table=df2$AE.table,
	AE_spindle=df2$AE.table)
	
	df3$cutnumber <- 1
	write.csv(df3, sprintf("../dataset/miiling/cut_number/cut%d.csv", 1),row.names=F)
	print(sprintf("../dataset/miiling/cut_number/cut%d.csv", 1))

for ( i in 2:167 )
{
	df2 <- data.frame(df[[i]])

	df2 <- data.frame(
		case=df2$case,
		run=df2$run,
		VB=df2$VB,
		time=df2$time,
		DOC=df2$DOC,
		feed=df2$feed,
		material=df2$material,
		smcAC=df2$smcAC,
		smcDC=df2$smcDC,
		vib.table=df2$vib.table,
		vib.spindle=df2$vib.spindle,
		AE.table=df2$AE.table,
		AE_spindle=df2$AE.table)
	df2$cutnumber <- i
	
	write.csv(df2, sprintf("../dataset/miiling/cut_number/cut%d.csv", i),row.names=F)
	print(sprintf("../dataset/miiling/cut_number/cut%d.csv", i))
	
	if ( is.na(df2$VB[1]) ) next
	if ( i == 18 ) next
	if ( i == 95 ) next
	if ( i == 106 ) next
	df3 <- rbind(df3, df2)
	
	if ( i == 48 ) break

}


df3$maintenance <- 0
cutnumber = 0
print(head(df3))
write.csv(df3, "../dataset/miiling/milling_.csv",row.names=F)

for ( i in 1:nrow(df3) )
{
	if ( is.finite(df3$VB[i]) && df3$VB[i] <= 0.14 && cutnumber+3 <= df3$cutnumber[i])
	{
		df3$maintenance[i] <- 1
		cutnumber = df3$cutnumber[i]
	}
}
 
write.csv(df3, "../dataset/miiling/milling.csv",row.names=F)
