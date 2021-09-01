library(foreign) # read dbf file

# set directory
setwd("M:/MeiRuu Project Folder/PhD/Paper 1/zonation and scale/CHASM - AZTOOL 02092019")

#import dbf file
#list of SA1 (source) with its neighbors polygon
adj.df <- data.frame(read.dbf("M:/MeiRuu Project Folder/PhD/Paper 1/zonation and scale/CHASM - AZTOOL 02092019/neighbors.dbf", as.is = FALSE))


colnames(adj.df) <- tolower(colnames(adj.df))
		adj.df$i_index <- adj.df$src_sa1_7d
		adj.df$neighbour <- adj.df$nbr_sa1_7d
		adj.df$sourceid <- as.numeric(factor(paste0(adj.df$i_index))) # create unique id for each SA1

		
		adj.df <- adj.df[order(adj.df$neighbour),]
		adj.df$nbrid <- as.numeric(factor(paste0(adj.df$neighbour))) # create unique id for each neighbour polygon
		
		length(unique(adj.df$i_index)) # check how many unique SA1
		
# check the adj.dfaset, see if the source id match with the nbr id
subset(adj.df,i_index =="5103001")
subset(adj.df,i_index =="5103004")


# import longitude and latitude of each SA1
lat <- data.frame(read.dbf("M:/MeiRuu Project Folder/PhD/Paper 1/zonation and scale/CHASM - AZTOOL 02092019/longlat.dbf", as.is = FALSE))

	colnames (lat) [1] <-"i_index" #rename variable name
	
##import 2016 total population by SA1

library (haven)
		
dat_pop <-data.frame(read_sas("K:/Mei Ruu Archive/2012-27 Total Population Projections/Population file - Sep 2018/2011 geographical boundaries/by calendar year/sa1totalpop.sas7bdat"))

	## subset 2016 population
	dat_pop <- subset(dat_pop, year==2016)
	colnames (dat_pop) [1] <-"i_index"
	
	## merge the population file with the longitude and latitude 
	temp <- merge(x=dat_pop, y=lat, by="i_index", all = TRUE)
	
	### load ED distance file (nearest ED distance from each SA1)
	dist <- data.frame(read_sas("M:/MeiRuu Project Folder/PhD/Paper 1/zonation and scale/no compactness constraint 26072019/sa1ed.sas7bdat"))
	dist2 <- dist[!duplicated(dist$SA1_7DIG11), ]  # remove duplicate rows
	dist3 <- subset(dist2, select=c(SA1_7DIG11, same_ED_dist))  #keep only SA1_7dig11 and ED distance
		colnames (dist3) [1] <-"i_index" #rename variable name

	## merge distance file with the pop and lat long file
	## keep only SA1 within metropolitan region
	MasterDF <- merge(x = dist3, y = temp, by = "i_index", all.x = TRUE)
	
	# exclude the two islands - Rottness Island and Garden Island
	MasterDF <- subset(MasterDF, i_index!='5116525' & i_index!='5118819')
	colnames (MasterDF) [2] <-"dist.nearest"
	colnames (MasterDF) [5] <-"colsmall"
	colnames (MasterDF) [6] <-"rowsmall"
	
	# exclude population = 0
	MasterDF <- subset(MasterDF, pop!=0)

# remove all the source and neighbour SA1 with zero population
# final file only contain SA1 with population and their neighbour SA1 with pop.
MasterDF <- subset(MasterDF, !is.na(dist.nearest))
MasterDF$i_index <- as.numeric(MasterDF$i_index)
temp <- subset(adj.df, i_index %in% MasterDF$i_index)
adj.df <- subset(temp, neighbour %in% MasterDF$i_index)
adj.df <- adj.df[order(adj.df$i_index),]

	
# # run Matt Yap's CHASM_ZTOOL
source("Y:/Matt Yap Work folder/Code/R code CHASM_ztool distance log.R")
MasterDF_list <- CHASM_ztool(targetPop=500,num_zonations=5,logkeep = TRUE,logdir = "M:/MeiRuu Project Folder/PhD/Paper 1/zonation and scale/CHASM - AZTOOL 02092019/logfiles",opt="dist")

zonation <- 1			
tmp <- MasterDF_list[[zonation]]	## This is the output of the CHASM_ztool function. It is a dataframe
		tmp2 <- aggregate(tmp$pop ~ tmp$clusterNo, FUN=sum)
		names(tmp2) <- c('poly','pop')
		hist(log10(tmp2$pop))


# ### The following does the same thing as the function but prints out a log for every single step
		# targetPop <- 500
		# adj.tmp <- adj.df
		# cellPool <- MasterDF$i_index[!is.na(MasterDF$pop)]
		# clusterNo <- 0
		# cellNo <- 0
		# clusterList <- list()
		# MasterDF2 <- MasterDF
		# MasterDF2$clusterNo <- 0
		# clusterPop <- c()

		# set.seed(1)

		# repeat{		## Start the loop which will go on until the pool of cells is empty
		
			# if (logkeep == TRUE) {
				# cat(paste0("clusterNo: ",clusterNo,"\n"),file = logfile,append = TRUE)
			# }
		
			# ## Pick a cell
			# cellNo <- 0
			# clusterNo 	<- clusterNo+1
			# if (clusterNo==1){	cell <- sample(x=cellPool,size=1) }
			# if (clusterNo!=1){	## This little chunk of code ensures that my next cluster starts adjacent to existing clusters
				# candidates <- adj.df$neighbour[adj.df$i_index %in% unlist(clusterList)]
				# candidates <- candidates[candidates %in% cellPool]
				# if (length(cellPool) == 0) {break}
				# if (length(candidates)==0 & length(cellPool) != 0) {candidates <- cellPool} ## This handles islands
				# if (length(candidates)==0) {next} else {
					# if (length(candidates) == 1) {cell <- candidates} 
					# if (length(candidates) > 1) {cell <- sample(x=candidates,size=1) }
				# }
			# }
			# if (length(cellPool) == 1) {cell <- cellPool}
			# MasterDF2$clusterNo[MasterDF2$i_index==cell] <- clusterNo
			# cellNo <- cellNo+1
			# if (cellNo==1) {clusterList[[clusterNo]] <- cell} else {
				# clusterList[[clusterNo]] <- unlist(list(clusterList[[clusterNo]],cell))}
			# cellPool 	<- cellPool[cellPool != cell]	## remove that cell from the pool
			# # nrow(adj.tmp)
			# adj.tmp 	<- adj.tmp[adj.tmp$neighbour != cell,] ## remove that cell from future neighbour list
			# # nrow(adj.tmp)
			# cell.dist 		<- MasterDF$dist.nearest[MasterDF$i_index ==cell]
			# cell.r 		<- MasterDF$rowsmall[MasterDF$i_index ==cell]
			# cell.c 		<- MasterDF$colsmall[MasterDF$i_index ==cell]
			# # if (nrow(adj.tmp)==0) {next}
			# if (sum(MasterDF$pop[MasterDF$i_index %in% clusterList[[clusterNo]]])>= targetPop ) {
			# clusterPop[clusterNo] <- sum(MasterDF$pop[MasterDF$i_index %in% clusterList[[clusterNo]]])
			# next}
			# if (length(cellPool) == 0) {break}

						
			# repeat{
				# # print(paste0("cell is ", cell)); flush.console()
				# print(paste0("cell pool has ", length(cellPool))); flush.console()
				# ## identify that cell's neighbours
				# tmpneigh <- adj.tmp$neighbour[adj.tmp$i_index==cell]; tmpneigh <- tmpneigh[tmpneigh %in% cellPool]
				# adj.tmp <- adj.tmp[adj.tmp$i_index !=cell,] ## remove that cell from the pool in adjacency matrix also
				# if (length(tmpneigh) == 0) {clusterPop[clusterNo] <- sum(MasterDF$pop[MasterDF$i_index %in% clusterList[[clusterNo]]]);break}
				
				# ### This is the part where the neighbours getting weighting according to how far the neighbours are from the cluster's origin
				# if (opt=="dist") {
					# tmpneigh.distr <- abs(MasterDF$dist.nearest[MasterDF$i_index %in% tmpneigh] - cell.dist)
					# tmpneigh.distr[tmpneigh.distr==0] <- 0.9
					# tmpneigh <- rep(x=tmpneigh, times=round(50/(tmpneigh.distr)^2)); #tmpneigh[tmpneigh<0] <- 0
				# } else {
					# tmpneigh.distr <- abs(MasterDF$rowsmall[MasterDF$i_index %in% tmpneigh]- cell.r) + 
									# abs(MasterDF$colsmall[MasterDF$i_index %in% tmpneigh]- cell.c) 
					# tmpneigh <- rep(tmpneigh, round(50/tmpneigh.distr^2));# tmpneigh[tmpneigh<0] <- 0
				# }
				
				# ## Now we pick a new cell and start again
				# if (length(tmpneigh)==1) {cell <- tmpneigh}
				# if (length(tmpneigh)>1) {cell <- sample(x=tmpneigh,size=1)}
				
				# cellNo <- cellNo+1
				# MasterDF2$clusterNo[MasterDF2$i_index==cell] <- clusterNo
				
				# cellPool 	<- cellPool[cellPool != cell]
				# adj.tmp 	<- adj.tmp[adj.tmp$neighbour != cell,]
				# cell.r 		<- MasterDF$rowsmall[MasterDF$i_index ==cell]
				# cell.c 		<- MasterDF$colsmall[MasterDF$i_index ==cell]
				# if (cellNo==1) {clusterList[[clusterNo]] <- cell}	## This could be changed to elseif statment if I knew how to do it
				# if (cellNo != 1) {clusterList[[clusterNo]] <- unlist(list(clusterList[[clusterNo]],cell))}
				# if (sum(MasterDF$pop[MasterDF$i_index %in% clusterList[[clusterNo]]])>= targetPop ) {clusterPop[clusterNo] <- sum(MasterDF$pop[MasterDF$i_index %in% clusterList[[clusterNo]]]);break}
				# if (length(cellPool) == 0) {clusterPop[clusterNo] <- sum(MasterDF$pop[MasterDF$i_index %in% clusterList[[clusterNo]]]);break}
			# } ### end of outer repeat loop (for cells within clusters)

		# # if (logkeep==TRUE){
			# # if ((clusterNo %% 100)==0) {
				# # cat(paste0("Part A: Percent of cells completed:",
					# # round(length(unlist(clusterList))*100/(ngrid.r*ngrid.c)),
					# # "% \n"),file = logfile,append = TRUE)
			# # }
		# # }
		# # if ((clusterNo %% 100)==0) {
			# # print(paste0("Big grids:",size,". Percent of cells completed: ",
					# # round(
						# # length(unlist(clusterList))*100/
						# # (nsupergrid.r*nsupergrid.c*ntinygrid.r*ntinygrid.c)
						# # )
					# # ,"%")); flush.console()
		# }

# MasterDF2$clusterNo[is.infinite(MasterDF2$clusterNo)] <- 0		## I don't think is/should be necessary
		
		
		
	# if (opt=="dist") {
		# cluster.dftmp <- aggregate(MasterDF2$dist.nearest ~ MasterDF2$clusterNo, FUN=mean)
		# cluster.dftmp2 <- aggregate(MasterDF2$pop ~ MasterDF2$clusterNo, FUN=sum)
		# cluster.df2 <- merge(cluster.dftmp,cluster.dftmp2, by="MasterDF2$clusterNo")
		# names(cluster.df2) <- c("clusterNo", "dist.nearest","clusterPop")	} 	else {
			# cluster.df2 <- aggregate(MasterDF2$pop ~ MasterDF2$clusterNo, FUN=sum)
			# names(cluster.df2) <- c("clusterNo", "clusterPop")
			# }
	
	
	# # if (logkeep==TRUE){
		# # cat(paste0("up to zonation number: (part B)",curr_seed,"\n"),file = logfile,append = TRUE)
	# # }
	
		# repeat {
			# #vs1 <- min(cluster.df2$clusterPop)
			# ## grab cluster and members
			# clusNo <- cluster.df2$clusterNo[cluster.df2$clusterPop == min(cluster.df2$clusterPop)][1]
			
			# clusMem <- MasterDF2$i_index[MasterDF2$clusterNo==clusNo]
				# ## Identify neighbouring cells
			# neighCells <- adj.df$neighbour[adj.df$i_index %in% clusMem]
				# ## Identify clusters of those cells
			# neighClus <- unique(MasterDF2$clusterNo[MasterDF2$i_index %in% neighCells])
			# neighClus <- neighClus[neighClus != clusNo]	   ### I don't know why this is necessary but apparently it is
				# ## Get their pops
			# neighClus.pop <- cluster.df2$clusterPop[cluster.df2$clusterNo %in% neighClus]
				# ## rename the cluster according to the neighbouring cluster with the lowest pop
			# tmp <- neighClus[neighClus.pop==min(neighClus.pop)][1]
				# # print(paste0("Cluster number is: ",clusNo,". Pop is ",cluster.df2$clusterPop[cluster.df2$clusterNo==clusNo],", added to cluster: ",tmp)); flush.console()
			# cluster.df2$clusterNo[cluster.df2$clusterNo==clusNo] <- tmp
			# MasterDF2$clusterNo[MasterDF2$clusterNo==clusNo] <- tmp
				# ##	repeat until done
			# if (opt=="dist") {
			# cluster.dftmp <- aggregate(cluster.df2$dist.nearest ~ cluster.df2$clusterNo, FUN=mean)
			# cluster.dftmp2 <- aggregate(cluster.df2$clusterPop ~ cluster.df2$clusterNo, FUN=sum)
			# cluster.df2 <- merge(cluster.dftmp,cluster.dftmp2, by="cluster.df2$clusterNo")
			# names(cluster.df2) <- c("clusterNo", "dist.nearest","clusterPop")	} 
		# else {
		# cluster.df2 <- aggregate(cluster.df2$clusterPop ~ cluster.df2$clusterNo, FUN=sum)
		# names(cluster.df2) <- c("clusterNo", "clusterPop")
		# }
			# print(paste0("Pop of smallest cluster is: ",min(cluster.df2$clusterPop))); flush.console()
			# if (min(cluster.df2$clusterPop)>=targetPop*0.8) {break}
			# }	
			
tmp <- MasterDF_list[[1]]	## This is the output of the CHASM_ztool function. It is a dataframe
		tmp2 <- aggregate(tmp$pop ~ tmp$clusterNo, FUN=sum)
		names(tmp2) <- c('poly','pop')
		hist(log10(tmp2$pop))