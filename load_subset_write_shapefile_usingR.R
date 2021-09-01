	## load shapefile and subset

library(rgdal)

shpin <- readOGR(dsn = "filepath", layer = "filename")
shpout <- subset(shpin,
	GCC_CODE11 == "5GPER" & 
	substr(SA4_NAME11,1,3) == "Per" & 
	!SA1_7DIG11 %in% c("5118819","5116525"))
	
writeOGR(shpout,dsn = "filepath", layer = "filename", driver = "ESRI Shapefile")