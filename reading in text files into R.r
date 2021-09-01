# This code is for looking at the first n rows of a large text file

setwd("X:/Folder 6 (HN - DoH) received 24082016/HMDC")
mordset <- read.table("201605.02_mor_output.txt", header=FALSE, nrows=10)


mordset <- read.table("201605.02_mor_output.txt", header=FALSE, nrows=1, skip=2)

setwd("X:/Folder 6 (HN - DoH) received 24082016/EDDC&other")
eddset <- read.table("201605.02_eme_output.txt", header=FALSE, nrows=1, skip=177301)



MHISdset <- read.table("mhis_geo_to_return.txt", header=FALSE, nrows=10)


MHISdset.colnames <- read.table("mhis_geo_to_return.txt", header=FALSE, nrows=1)
MHISdset.row1 <- read.table("mhis_geo_to_return.txt", header=FALSE, nrows=1, skip=1, sep="\t")
MHISdset.row2 <- read.table("mhis_geo_to_return.txt", header=FALSE, nrows=1, skip=2, sep="\t")
MHISdset.row3 <- read.table("mhis_geo_to_return.txt", header=FALSE, nrows=1, skip=3, sep="      ")