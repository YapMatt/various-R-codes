MasterList <- list()
zonationList <- list()
startTime <- c()
endTime <- c()
for (size in 1:9){
startTime[size] <- Sys.time()
nsupergrid.r <- size
nsupergrid.c <- size
ntinygrid.r <- 20
ntinygrid.c <- 20
 
i_index <- 1:(nsupergrid.r * nsupergrid.c * ntinygrid.r * ntinygrid.c)
MasterDF <- data.frame(
                i_index = i_index)
MasterDF$rowsmall <- ceiling(MasterDF$i_index / (nsupergrid.c * ntinygrid.c))
MasterDF$colsmall <- MasterDF$i_index %% (nsupergrid.c * ntinygrid.c)
MasterDF$colsmall[MasterDF$colsmall == 0] <- nsupergrid.r * ntinygrid.r
MasterDF$rowbig <- ceiling(MasterDF$rowsmall / ntinygrid.r)
MasterDF$colbig <- ceiling(MasterDF$colsmall / ntinygrid.c)
 
## define index matrix at small scale
i_index <- 1:(nsupergrid.r * nsupergrid.c * ntinygrid.r * ntinygrid.c)
MasterDF <- data.frame(
                i_index = i_index)
MasterDF$rowsmall <- ceiling(MasterDF$i_index / (nsupergrid.r * ntinygrid.r))
MasterDF$colsmall <- MasterDF$i_index %% (nsupergrid.r * ntinygrid.r)
MasterDF$colsmall[MasterDF$colsmall == 0] <- nsupergrid.r * ntinygrid.r
MasterDF$rowbig <- ceiling(MasterDF$rowsmall / ntinygrid.r)
MasterDF$colbig <- ceiling(MasterDF$colsmall / ntinygrid.c)
MasterDF$rowtiny <- MasterDF$rowsmall %% ntinygrid.r
MasterDF$rowtiny[MasterDF$rowtiny==0] <- ntinygrid.r
MasterDF$coltiny <- MasterDF$i_index %% ntinygrid.c
MasterDF$coltiny[MasterDF$coltiny==0] <- ntinygrid.c
 
 
nsmallgrid.r <- nsupergrid.r*ntinygrid.r
nsmallgrid.c <- nsupergrid.c*ntinygrid.c

tmp <- MasterDF; tmp$add1 <- MasterDF$i_index
tmp$rowsmall<- tmp$rowsmall-1
add1 <- subset(tmp[,c("add1","colsmall","rowsmall")], 
                rowsmall>=1 & rowsmall<=nsmallgrid.r & colsmall>=1 & colsmall<=nsmallgrid.c)
 
tmp <- MasterDF; tmp$add2 <- MasterDF$i_index
tmp$rowsmall<- tmp$rowsmall-1
tmp$colsmall<- tmp$colsmall-1
add2 <- subset(tmp[,c("add2","colsmall","rowsmall")], 
                rowsmall>=1 & rowsmall<=nsmallgrid.r & colsmall>=1 & colsmall<=nsmallgrid.c)
                
tmp <- MasterDF; tmp$add3 <- MasterDF$i_index
tmp$colsmall<- tmp$colsmall-1
add3 <- subset(tmp[,c("add3","colsmall","rowsmall")], 
                rowsmall>=1 & rowsmall<=nsmallgrid.r & colsmall>=1 & colsmall<=nsmallgrid.c)
 
tmp <- MasterDF; tmp$add4 <- MasterDF$i_index
tmp$rowsmall<- tmp$rowsmall+1
tmp$colsmall<- tmp$colsmall-1
add4 <- subset(tmp[,c("add4","colsmall","rowsmall")], 
                rowsmall>=1 & rowsmall<=nsmallgrid.r & colsmall>=1 & colsmall<=nsmallgrid.c)
                
tmp <- MasterDF; tmp$add5 <- MasterDF$i_index
tmp$rowsmall<- tmp$rowsmall+1
add5 <- subset(tmp[,c("add5","colsmall","rowsmall")], 
                rowsmall>=1 & rowsmall<=nsmallgrid.r & colsmall>=1 & colsmall<=nsmallgrid.c)             
 
tmp <- MasterDF; tmp$add6 <- MasterDF$i_index
tmp$rowsmall<- tmp$rowsmall+1
tmp$colsmall<- tmp$colsmall+1
add6 <- subset(tmp[,c("add6","colsmall","rowsmall")], 
                rowsmall>=1 & rowsmall<=nsmallgrid.r & colsmall>=1 & colsmall<=nsmallgrid.c)             
                
tmp <- MasterDF; tmp$add7 <- MasterDF$i_index
tmp$colsmall<- tmp$colsmall+1
add7 <- subset(tmp[,c("add7","colsmall","rowsmall")], 
                rowsmall>=1 & rowsmall<=nsmallgrid.r & colsmall>=1 & colsmall<=nsmallgrid.c)             
                
tmp <- MasterDF; tmp$add8 <- MasterDF$i_index
tmp$rowsmall<- tmp$rowsmall-1
tmp$colsmall<- tmp$colsmall+1
add8 <- subset(tmp[,c("add8","colsmall","rowsmall")], 
                rowsmall>=1 & rowsmall<=nsmallgrid.r & colsmall>=1 & colsmall<=nsmallgrid.c)             
                
MasterDF <- merge(MasterDF,add1,by=c("colsmall","rowsmall"), all.x=TRUE)
MasterDF <- merge(MasterDF,add2,by=c("colsmall","rowsmall"), all.x=TRUE)
MasterDF <- merge(MasterDF,add3,by=c("colsmall","rowsmall"), all.x=TRUE)
MasterDF <- merge(MasterDF,add4,by=c("colsmall","rowsmall"), all.x=TRUE)
MasterDF <- merge(MasterDF,add5,by=c("colsmall","rowsmall"), all.x=TRUE)
MasterDF <- merge(MasterDF,add6,by=c("colsmall","rowsmall"), all.x=TRUE)
MasterDF <- merge(MasterDF,add7,by=c("colsmall","rowsmall"), all.x=TRUE)
MasterDF <- merge(MasterDF,add8,by=c("colsmall","rowsmall"), all.x=TRUE)
                                
MasterDF <- MasterDF[order(MasterDF$i_index),]
MasterDF$pop <- round(runif(nrow(MasterDF))*1000)  

neighs <- c(1,3,5,7)	## This creates rook adjacency only
t_list <- list()
for (i in neighs){
t_list[[i]] <- MasterDF[!is.na(7+i),c(3,7+i)]
names(t_list[[i]]) <- c("i_index","neighbour")
}
adj.df <- do.call('rbind',t_list)
adj.df <- adj.df[!is.na(adj.df$neighbour),]
                
adj.df <- adj.df[order(adj.df$i_index),]


##################
##################
#
#
targetPop <- 10000
#### This is where I make a loop that creates polygons
## Define a pool of cells to choose from
adj.tmp <- adj.df
cellPool <- MasterDF$i_index
clusterNo <- 0
cellNo <- 0
clusterList <- list()
MasterDF2 <- MasterDF
MasterDF2$clusterNo <- 0
clusterPop <- c()

repeat{		## Start the loop which will go on until the pool of cells is empty
	## Pick a cell
	cellNo <- 0
	clusterNo 	<- clusterNo+1
	if (clusterNo==1){	cell <- sample(x=cellPool,size=1) }
	if (clusterNo!=1){	## This little chunk of code ensures that my next cluster starts adjacent to existing clusters
		candidates <- adj.df$neighbour[adj.df$i_index %in% unlist(clusterList)]
		candidates <- candidates[candidates %in% cellPool]
		cell <- sample(x=candidates,size=1) 
	}
	MasterDF2$clusterNo[MasterDF2$i_index==cell] <- clusterNo
	cellNo <- cellNo+1
	if (cellNo==1) {clusterList[[clusterNo]] <- cell}	## This could be changed to elseif statment if I knew how to do it
	if (cellNo==!1) {clusterList[[clusterNo]] <- c(clusterList[[clusterNo]],cell)}
	cellPool 	<- cellPool[cellPool != cell]	## remove that cell from the pool
	# nrow(adj.tmp)
	adj.tmp 	<- adj.tmp[adj.tmp$neighbour != cell,] ## remove that cell from future neighbour list
	# nrow(adj.tmp)
	cell.r 		<- MasterDF$rowsmall[MasterDF$i_index ==cell]
	cell.c 		<- MasterDF$colsmall[MasterDF$i_index ==cell]
	# if (nrow(adj.tmp)==0) {next}
	# if (sum(MasterDF$pop[MasterDF$i_index %in% clusterList[[clusterNo]]])>= targetPop ) {next}
	if (length(cellPool) == 0) {break}

	repeat{
		## identify that cell's neighbours
		tmpneigh <- adj.tmp$neighbour[adj.tmp$i_index==cell]; tmpneigh <- tmpneigh[tmpneigh %in% cellPool]
		
		adj.tmp <- adj.tmp[adj.tmp$i_index !=cell,] ## remove that cell from the pool in adjacency matrix also
		if (length(tmpneigh) == 0) {clusterPop[clusterNo] <- sum(MasterDF$pop[MasterDF$i_index %in% clusterList[[clusterNo]]]);break}
		
		### This is the part where the neighbours getting weighting according to how far the neighbours are from the cluster's origin
		tmpneigh.distr <- abs(MasterDF$rowsmall[MasterDF$i_index %in% tmpneigh]- cell.r) + 
						abs(MasterDF$colsmall[MasterDF$i_index %in% tmpneigh]- cell.c) 
		tmpneigh <- c(rep(tmpneigh, round(50/tmpneigh.distr^2))); tmpneigh[tmpneigh<0] <- 0
		
		## Now we pick a new cell and start again
		cell <- sample(x=tmpneigh,size=1)
		cellNo <- cellNo+1
		MasterDF2$clusterNo[MasterDF2$i_index==cell] <- clusterNo
		clusterList[[clusterNo]] <- c(clusterList[[clusterNo]],cell)
		cellPool 	<- cellPool[cellPool != cell]
		adj.tmp 	<- adj.tmp[adj.tmp$neighbour != cell,]
		cell.r 		<- MasterDF$rowsmall[MasterDF$i_index ==cell]
		cell.c 		<- MasterDF$colsmall[MasterDF$i_index ==cell]
		if (cellNo==1) {clusterList[[clusterNo]] <- cell}	## This could be changed to elseif statment if I knew how to do it
		if (cellNo==!1) {clusterList[[clusterNo]] <- c(clusterList[[clusterNo]],cell)}
		if (sum(MasterDF$pop[MasterDF$i_index %in% clusterList[[clusterNo]]])>= targetPop ) {clusterPop[clusterNo] <- sum(MasterDF$pop[MasterDF$i_index %in% clusterList[[clusterNo]]]);break}
		if (length(cellPool) == 0) {clusterPop[clusterNo] <- sum(MasterDF$pop[MasterDF$i_index %in% clusterList[[clusterNo]]]);break}
	} ### end of outer repeat loop (for cells within clusters)
#print(paste0("Finished cluster ",clusterNo," with ",cellNo," members")); flush.console()
if ((clusterNo %% 100)==0) {
	print(paste0("Big grids:",size,". Percent of cells completed: ",
			round(
				length(unlist(clusterList))*100/
				(nsupergrid.r*nsupergrid.c*ntinygrid.r*ntinygrid.c)
				)
			,"%")); flush.console()
}
} ### end of outer repeat loop (for clusters)
## MasterDF2$clusterNo[is.na(MasterDF2$clusterNo)] <- 0		## I don't think is/should be necessary

# ## visualise result
# p.df <- matrix(MasterDF2$clusterNo, nrow=nsupergrid.r*ntinygrid.r, ncol=nsupergrid.c*ntinygrid.c)
# plot(raster(t(p.df)))

# ### Save this step 
# holdresult1 <- MasterDF2
# MasterDF2 <- holdresult1 
############################################
######################
###########
### Now the task is to grab clusters which are small and join them to neighbouring clusters
## We can do this by order of size.
## The process will be: 
	## rank cells by ascending pop; 
	## identify their neighbouring clusters by pop rank; 
	## then add

## Easy part: rank clusters by pop
cluster.df <- data.frame(clusterNo=1:length(clusterPop), clusterPop)
cluster.df <- cluster.df[order(cluster.df$clusterPop),]
cluster.df$poprank <- c(1:length(clusterPop))
cluster.df <- cluster.df[order(cluster.df$clusterNo),]

## Hard part, identify neighbouring clusters
cluster.df2 <- cluster.df
rankNo <- 0
repeat {
	## grab cluster and members
clusterNo <- min(cluster.df2$clusterNo[cluster.df2$clusterPop == min(cluster.df2$clusterPop)])
clusMem <- clusterList[[clusterNo]]
	## Identify neighbouring cells
neighCells <- adj.df$neighbour[adj.df$i_index %in% clusMem]
	## Identify clusters of those cells
neighClus <- unique(MasterDF2$clusterNo[MasterDF2$i_index %in% neighCells])
neighClus <- neighClus[neighClus != clusterNo] ### I don't know why this is necessary but apparently it is
	## Get their pops
neighClus.pop <- cluster.df2$clusterPop[cluster.df2$clusterNo %in% neighClus]
	## rename the cluster according to the neighbouring cluster with the lowest pop
cluster.df2$clusterNo[cluster.df2$clusterNo==clusterNo] <- min(neighClus[neighClus.pop==min(neighClus.pop)])
MasterDF2$clusterNo[MasterDF2$clusterNo==clusterNo] <- min(neighClus[neighClus.pop==min(neighClus.pop)])
	##	repeat until done
cluster.df2 <- aggregate(cluster.df2$clusterPop ~ cluster.df2$clusterNo, FUN=sum)
names(cluster.df2) <- c("clusterNo", "clusterPop")
cluster.df2 <- cluster.df2[order(cluster.df2$clusterPop),]
cluster.df2$poprank <- c(1:nrow(cluster.df2))
cluster.df2 <- cluster.df2[order(cluster.df2$clusterNo),]
if (min(cluster.df2$clusterPop>=targetPop*0.8)) {break}
if ((clusterNo %% 100)==0){
	print(paste0("Big grids:",size,".Percent of cells completed: ",round(length(unlist(clusterList))/(nsupergrid.r * nsupergrid.c * ntinygrid.r * ntinygrid.c)*100),"%")); flush.console()
}
## print(paste0("Minimum pop is ",min(cluster.df2$clusterPop))); flush.console()
}
MasterList[[size]] <- MasterDF2
zonationList[[size]] <- cluster.df2
endTime[size] <- Sys.time()
}

timeTaken <- as.difftime(endTime-startTime)
size.v <- c(1:9)

# simple linear function to predict time taken for different scale of grids
exp.mod <- lm(log(timeTaken) ~ size.v)
summary(exp.mod)
predTime <- exp(predict(exp.mod,list(size.v)))
plot(timeTaken)
lines(size.v, predTime)

size.df <- data.frame(size=size.v, timeTaken=timeTaken)
size.df$prev <- c(0,size.df$timeTaken[1:(length(timeTaken)-1)])
size.df$compare <- size.df$timeTaken/size.df$prev


rand <- sample(1:length(unique(MasterDF2$clusterNo)),length(unique(MasterDF2$clusterNo)), replace=FALSE)
ranClusIDs <- data.frame(rand=rand, clusterNo=clusterIDs)
MasterDF3 <- merge(MasterDF2, ranClusIDs, by="clusterNo")

## visualise result
p.df <- matrix(MasterDF3$rand, nrow=nsupergrid.r*ntinygrid.r, ncol=nsupergrid.c*ntinygrid.c)
par(mfrow=c(2,1))
plot(raster(t(p.df)))
hist(log10(cluster.df2$clusterPop))

632.9*1.6^11/60/60/24
## 632 seconds for 9x9 from start to finish
## Whole process started at 3:00 and finished 12:59


kev_raster <- raster(t(p.df))
kev_df <- data.frame(id = 1:length(MasterDF2$clusterNo),polyid = MasterDF2$clusterNo)
set.seed(1)
temp_samp <- sample(1:1000000,max(kev_df$polyid),replace = FALSE)
kev_df$polyid2 <- temp_samp[kev_df$polyid]

kev_raster_p <- as(kev_raster,"SpatialPolygons")
library(broom)
kev_raster_p_df <- tidy(kev_raster_p)

library(ggplot2)
kev_raster_p_df2 <- merge(kev_raster_p_df,kev_df,by = "id")

kev_p <- ggplot() + 
	geom_polygon(
		data = kev_raster_p_df2,
		aes(
			x = long,y = lat,group = group,
			fill = as.numeric(polyid2)),
		size = 0)






