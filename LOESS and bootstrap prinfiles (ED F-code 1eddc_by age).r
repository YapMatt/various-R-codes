library(foreign)
setwd("Z:/Yap Project folder/Completed/MentalED")
wholeprin<-read.dta("prinment_ed1eddc.dta")


losdata.age1 <- wholeprin[wholeprin$agecat==1 & wholeprin$atsi==0,]
losdata.age2 <- wholeprin[wholeprin$agecat==2 & wholeprin$atsi==0,]
losdata.age3 <- wholeprin[wholeprin$agecat==3 & wholeprin$atsi==0,]
losdata.age4 <- wholeprin[wholeprin$agecat==4 & wholeprin$atsi==0,]
losdata.age5 <- wholeprin[wholeprin$agecat==5 & wholeprin$atsi==0,]

library(KernSmooth)
library(boot)

myagegrp<-1

print(paste("...up to age group ",myagegrp,sep=""))
heading<-paste("ED F-code - agecat ",myagegrp,sep="")

bbs <- function(data, indices) 
	{
  	  d <- data[indices,] # allows boot to select sample
  	  day <- NA; rate <- NA; pdaysum <- NA;
  	  for (i in (1:3666)) 
		{  
 	  	  day[i] <- i
	  	  pdaysum[i] <- sum(eval(parse(text=paste("d$pday",i,sep=""))))
	  	  rate[i] <- sum(eval(parse(text=paste("d$hosptotday",i,sep="")))) / pdaysum[i]
		}
  	  fit <- loess(rate~day, span=1.4, weights=pdaysum)
  	  return(predict(fit, data.frame(day = seq(1,3666,1)), se = FALSE))
	}

abs <- function(data, indices) 
	{
  	  d <- data[indices,] # allows boot to select sample
  	  day <- NA; rate <- NA; pdaysum <- NA;
  	  for (i in (3696:6266)) 
		{  
 	  	  day[i] <- i
	  	  pdaysum[i] <- sum(eval(parse(text=paste("d$pday",i,sep=""))))
	  	  rate[i] <- sum(eval(parse(text=paste("d$hosptotday",i,sep="")))) / pdaysum[i]
		}
  	  fit <- loess(rate~day, span=1.4, weights=pdaysum)
  	  return(predict(fit, data.frame(day = seq(3696,6266,1)), se = FALSE))
	}

## Set number of bootstrap samples to use
	num.boots <- 1000
	
## Set the dataset we are using...
thedata <- eval(parse(text=paste("losdata.age",myagegrp,sep="")))

## First softcode the appropriate day references: for before CE we want rootref and pday and hosptotday 1-2207
cols.before <- match(c("rootnum","hosptotday1","hosptotday3666","pday1","pday3666"),colnames(thedata))
## Second softcode the appropriate day references: for after CE we want rootref and pday and hosptotday 2236-3711
cols.after <- match(c("rootnum","hosptotday3696","hosptotday6266","pday3696","pday6266"),colnames(thedata))
## Now we can run the bootstrap with LOESS modelling for these days...

	##################
	## Fancy Parallel bootstrapping courtesy of Matt T
	##
	
library(parallel)

cl <- makePSOCKcluster(20)

res_los_b <- boot(data=thedata[,
c(cols.before[1],cols.before[2]:cols.before[3],cols.before[4]:cols.before[5])], statistic=bbs, R=num.boots,
	parallel="snow",ncpus=5,cl=cl)
stopCluster(cl)
cl <- makePSOCKcluster(20)
	res_los_a<- boot(data=thedata[,
c(cols.after[1],cols.after[2]:cols.after[3],cols.after[4]:cols.after[5])], statistic=abs, R=num.boots,
	parallel="snow",ncpus=5,cl=cl)

stopCluster(cl)
	
	##
	##################

print("finished boot (before and after)")	



plot_los_b <- as.data.frame(cbind(day=seq(1,3666,1),fit=NA,lower=NA,upper=NA))
for (i in (1:3666))
	{
	  plot_los_b$fit[i]   <- res_los_b$t0[i] 
	  plot_los_b$lower[i] <- boot.ci(res_los_b,type="perc",index=i)$percent[,4] 
	  plot_los_b$upper[i] <- boot.ci(res_los_b,type="perc",index=i)$percent[,5]
	}


plot_los_a <- as.data.frame(cbind(day=seq(3696,6266,1),fit=NA,lower=NA,upper=NA))
for (i in (1:2571))
	{
	  plot_los_a$fit[i]   <- res_los_a$t0[i] 
	  plot_los_a$lower[i] <- boot.ci(res_los_a,type="perc",index=i)$percent[,4] 
	  plot_los_a$upper[i] <- boot.ci(res_los_a,type="perc",index=i)$percent[,5]
	}


######################################################################################################
# Concatenate before and after data and code plottime for consecutive age plots                      #
######################################################################################################

los <- as.data.frame(rbind(plot_los_b, plot_los_a))
dim(los)


los$plottime <- (los$day-3681)/365.25

######################################################################################################
# Plot loess estimate with 95% CI, including red reference lines for time of cardinal event          #
# and grey separators for each age category.  Optional tif or pdf output file.                       #
# Note: plot interval excludes days 1-30, 3651-3711, 5872 for consistency with manuscript.           #
######################################################################################################


######################################################################################################
# for pdf plot
######################################################################################################
head()

library(ggplot2)

los1 <- los[which(!(los$day<31 | los$day>6236 | (los$day>3650 & los$day<3712))),]


pdf(file=paste("ED F-code 1eddc age",myagegrp," non-aboriginal.pdf",sep=""))
 
  g0 <-  ggplot() + theme_bw() + 
         #ylim(0, 2) + ylab("ED presentations per year") +
          xlim(-10, 7) + xlab("Years relative to cardinal event") + 
	scale_x_continuous(breaks=seq(-10, 7, 2))+
	theme(plot.title = element_text(lineheight=5, size=22, face="bold")) +
         theme(axis.text.x=element_text(size=18,face="bold")) + 
         theme(axis.text.y=element_text(size=18,face="bold")) +
         theme(axis.title.x=element_text(size=18,face="bold")) + 
         theme(axis.title.y=element_text(angle=90,size=18,face="bold")) + 
         theme(axis.line = element_line(size = 1)) +
         geom_vline(aes(xintercept=0), colour="black", linetype="dashed") 
  g0 +  geom_linerange(data=los1, aes(x=los1$plottime, y=los1$fit*365.25, ymin=los1$lower*365.25, ymax=los1$upper*365.25), size=1, colour="grey") + 
        geom_point(data=los1, aes(x=los1$plottime, y=los1$fit*365.25), size=1, colour="black")+
		ggtitle(heading)
		

dev.off()

save.image(paste("Z:/Yap Project folder/Completed/MentalED/eddc agecat",myagegrp,"natsi.rdata", sep=""))

######################################################################################################
# CHECK Calculate N for each daya and merge with estimate plus 95% bootstrapped CI                         # 
######################################################################################################

# ns <- function(data) 
	# {
  	  # pdaysum <- NA;
  	  # for (i in (c(31:3650,3712:6236))) {pdaysum[i] <- sum(eval(parse(text=paste("data$pday",i,sep=""))))}
  	  # return(as.data.frame(cbind(day=c(1:6236), pdaysum)))
	# }

# ntotals <- ns(losdata)
# ntotals <- ntotals[which(!is.na(ntotals$pdaysum)),]
# dim(ntotals)

# los1$time <- round(los1$plottime, digits=1)
# los1$fitdays <- los1$fit*365.25
# los1$lowerdays <- los1$lower*365.25
# los1$upperdays <- los1$upper*365.25
# los1$fitdays <- round(los1$fitdays, digits=2)
# los1$lowerdays <- round(los1$lowerdays, digits=2)
# los1$upperdays <- round(los1$upperdays, digits=2)

# los1 <- merge(los1, ntotals, by="day")
# los2 <- los1[which(los1$day %in% c(31,396,761,1126,1491,1856,2221,2586,2951,3316,3650,3712,4047,4412,4777,5142,5507,5872,6236)),]


#########################################################################################
# TO EXPORT DATA FROM R TO EXCEL                                                                    #
######################################################################################################


		num.boots <- 1000

		# create a bootstrapped sample
	set.seed(500)
	library(doRNG)
	library(doParallel)
	
	matt.clus <- makeCluster(20)
	registerDoParallel(matt.clus)
	
	start.time <- Sys.time()
	
	mattboot <- foreach(i = 1:num.boots) %dorng% { 
		temp.bootindex <- sample(nrow(thedata),nrow(thedata),replace=TRUE)
		temp.bootsample <- thedata[temp.bootindex,]
		
			## get the rate
			
		temp.pdaysumvec <- colSums(temp.bootsample[,paste0("pday",1:6266)])
		temp.hosptotdaysumvec <- colSums(temp.bootsample[,paste0("hosptotday",1:6266)])
	
			## return
		return(unname(temp.hosptotdaysumvec/temp.pdaysumvec))
		
		# return(temp.bootsample)
	} ## end bootloop
	
	print(Sys.time() - start.time)
	stopCluster(matt.clus)
		
	mattboot3 <- do.call(rbind,mattboot)
	
		## cleanup
		
	rm(list=c("mattboot"))

# from the bootstrap matrix, sum each period, and get the 2.5th, 50th, 97.5th percentile for the sum of each period
bp1 <- mattboot3[1:1000,1126:3650]	#7yrs before
bp1.rowsums <- apply(bp1,1,sum) #This adds up a row
bp1.rowsums.quantiles <- quantile(bp1.rowsums,probs=c(0.025,0.5,0.975))
bp1.rowsums.quantiles$period <- c("7yrs before")

ap3 <- mattboot3[1:1000,3712:6240] #7yrs after
ap3.rowsums <- apply(ap3,1,sum) #This adds up a row
ap3.rowsums.quantiles <- quantile(ap3.rowsums,probs=c(0.025,0.5,0.975))
ap3.rowsums.quantiles$period <- c("7yrs after")

dp1 <- mattboot3[1:1000,3651:3711] #period around the CE
dp1.rowsums <- apply(dp1,1,sum) #This adds up a row
dp1.rowsums.quantiles <- quantile(dp1.rowsums,probs=c(0.025,0.5,0.975))
dp1.rowsums.quantiles$period <- c("Cardinal period")

NATSI3PNE <- bp1.rowsums.quantiles 
NATSI3PNE <- (rbind(NATSI3PNE,dp1.rowsums.quantiles))
NATSI3PNE <- (rbind(NATSI3PNE,ap3.rowsums.quantiles))
write.csv(NATSI3PNE,file=(paste("Z:/Yap Project folder/Completed/MentalED/eddc agecat",myagegrp,"totals.csv", sep="")), row.names=F)

