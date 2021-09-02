setwd("Z:/Yap Project folder/Completed/MentalED")
plot_period_agecat1 <- read.csv("eddc agecat1totals.csv")
plot_period_agecat2 <- read.csv("eddc agecat2totals.csv")
plot_period_agecat3 <- read.csv("eddc agecat3totals.csv")
plot_period_agecat4 <- read.csv("eddc agecat4totals.csv")
plot_period_agecat5 <- read.csv("eddc agecat5totals.csv")

plot_period_agecat1$agecat <- "<20"
plot_period_agecat2$agecat <- "20-29"
plot_period_agecat3$agecat <- "30-39"
plot_period_agecat4$agecat <- "40-49"
plot_period_agecat5$agecat <- "50+"


tmp_plot_ED_All <- as.data.frame(rbind(plot_period_agecat1,plot_period_agecat2,plot_period_agecat3,plot_period_agecat4,plot_period_agecat5))
table(tmp_plot_ED_All$period)
## Here we need to pick out the correct time frames ##
#tmp_plot_ED_All <- tmp_plot_ED_All[which(tmp_plot_ED_All$period %in% c("1-2yrs before","1-2yrs after")),]

tmp_plot_ED_All$periodN [which(tmp_plot_ED_All$period=="7yrs before")] <- 1
tmp_plot_ED_All$periodN [which(tmp_plot_ED_All$period=="Cardinal period")] <- 2
tmp_plot_ED_All$periodN [which(tmp_plot_ED_All$period=="7yrs after")] <- 3


tmp_plot_ED_All$periodN [which(tmp_plot_ED_All$period=="7yrs before")] <- 1
tmp_plot_ED_All$periodN [which(tmp_plot_ED_All$period=="Cardinal period")] <- 2
tmp_plot_ED_All$periodN [which(tmp_plot_ED_All$period=="7yrs after")] <- 3



agecat_order <- tmp_plot_ED_All $agecat
agecat_order [which(agecat_order=="<55")] <- 1
agecat_order [which(agecat_order=="55-64")] <- 2
agecat_order [which(agecat_order=="65-74")] <- 3
agecat_order [which(agecat_order=="75-84")] <- 4
agecat_order [which(agecat_order=="85+")] <- 5
agecat_order <- as.integer(agecat_order)


# plot it #
library(ggplot2)

pdf(file = "eddc AUC plots (7v7).pdf", width = 8, height = 8)

      dodge <- position_dodge(width=0.8) 
g0 <- ggplot(data=tmp_plot_ED_All,  
          aes(x= tmp_plot_ED_All$periodN,
             y= tmp_plot_ED_All$X50., 
             color=reorder(tmp_plot_ED_All$agecat, agecat_order))) +  
              ggtitle("ED F-codes by age") +  ylim(0, 8) + xlab("") +
               ylab("ED presentations during each period") +  theme_bw() + 
theme(plot.title = element_text(lineheight=2, size=20, face="bold")) +
               theme(axis.text.x=element_text(size=14,face="bold",angle=0,hjust=0.5)) +        
               theme(axis.text.y=element_text(size=14,face="bold")) +
               theme(axis.title.x=element_text(size=14,face="bold")) + 
               theme(axis.title.y=element_text(angle=90,size=14,face="bold")) + 
			   theme(axis.line = NULL) +
               #theme(axis.line = element_line(size = 1)) +
			   theme(panel.grid.major= element_line(colour="white")) +
			   theme(panel.grid.minor= element_line(colour="white")) +
			   #theme(panel.grid.major.x= element_line(size = 0)) +
			   #theme(panel.grid.minor.x= element_line(size = 0.5, color="snow4")) +
theme(legend.text=element_text(size=14)) + 
               theme(legend.position = c(0.5,0.8)) + theme(legend.title=element_blank())
g0 + geom_errorbar(data= tmp_plot_ED_All,
               aes(x= tmp_plot_ED_All$periodN, 
                  y= tmp_plot_ED_All$X50.,
                  color=reorder(tmp_plot_ED_All$agecat,agecat_order),
                  ymin= tmp_plot_ED_All$X2.5., 
                  ymax= tmp_plot_ED_All$X97.5.), position=dodge, size=0.5, width=0.4) + 
               geom_point(position=dodge) + scale_colour_grey(start = 0, end = .7) +
               scale_x_continuous(breaks=c(1,2,3), labels=c("7 yrs before","Cardinal period","7 yrs after"))+
			   #scale_y_continuous(breaks=c(0,5000,10000,15000,20000,25000,30000), labels=c("$0","$5k", "$10k", "$15k","$20k","$25k","$30k"))+
			   expand_limits(y=0)
dev.off()