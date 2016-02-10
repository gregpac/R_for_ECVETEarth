### to do :

### TIMEGRAPH : add annotations (Pid? place? nbr of impacted people?) for the specific values (bigger than frame, remarquable : to check with consortium)
### SUBSETTING : try to rewrite with [ function instead of subset()


### HOW-TO :
### instructions for a letter (e.g. ### A ###) are given for the lines between the letter and its closing tag (e.g. starts at ### A ### and ends at ### /A ###) 
### check the filename and its location at letter ### A ###
### to disable producing global graph, comment at ### B ###
### enter the partner number at letter  ### C ### 
### to disable saving new file for partner graph, comment at ### D ###
### to disable calling for partner timeline (different sheet than count analysis) : comment at ### E ###


rm(list = ls())
library(ggplot2)
library(gridExtra)
library(scales)
library(plyr)
source("/Users/gregoirepaccoud/R/PIRATE/dissanalysis_f.R")
source("/Users/gregoirepaccoud/R/PIRATE/disstimeline_f.R")


### A ###
setwd ('/Volumes/HDWL/01.PROJETS-SUBV/01.PIRATE/WP6/DISSANALYSIS')
dissdata<-read.csv(file="dissem_151119.csv", header=TRUE, stringsAsFactors=FALSE)
### /A ###

##global_nodouble and Targeted are intiated before calling for GLOBALCOUNT
##subset to eliminate rows with a defined value in column DOUBLE (suppress doubles from dataframe)
global_nodouble <- subset(dissdata, is.na(dissdata$DOUBLE))
##count overall public targeted 
Targeted<-sum(global_nodouble$IMPACTEDPEOPLE, na.rm=TRUE)

nodouble_g<- subset(global_nodouble, grepl("P*", PARTNER)) 
nodouble_g<- nodouble_g[complete.cases(nodouble_g[,8]),]

###p is defined to be a replacement box for (yet) unavailable content/plot
p = rectGrob()

### B ###
##Call for Global count and plot : if only partner plot is needed, comment in GLOBALCOUNT
source("/Users/gregoirepaccoud/R/PIRATE/dissanalysis-GLOBALCOUNT-150611.R")
### /B ###
 
#extract all possible values for IMPACT and LEVEL
impact_val<-unique(global_nodouble$IMPACT)
target_val<-unique(global_nodouble$LEVEL) 

### C ###
# define partner number ("PXX")
partner_number<-"P1"
### /C ###

partner<-paste("^",partner_number, "$", sep="")
nodouble<- global_nodouble[grep(partner, global_nodouble$PARTNER),]
#subset(global_nodouble, grepl(partner, PARTNER)) 
nodouble<- nodouble[complete.cases(nodouble[,8]),]


## extracting data for plot title 
#count impacted people for the partner selected
Targeted_p<-sum(nodouble$IMPACTEDPEOPLE, na.rm=TRUE)

# convert ENDDATE to date format
nodouble$ENDDATE<-as.Date(nodouble$ENDDATE,)

#delete NA for dates and extracting start and end dates
nodouble_date <- subset(nodouble, !is.na(nodouble$ENDDATE))
start_date<-min( nodouble_date[ , "ENDDATE" ] )
end_date<-max( nodouble_date[ , "ENDDATE" ] )

##titre to print partner number and period with bigger typo
#subs with partner and total consortium impacted people 
maintitle<-paste(partner_number, "from", start_date, "to", end_date)
subtitle<-paste(Targeted_p, "impacted people - Total for Pirate :", Targeted)
mainT<-textGrob(maintitle, gp=gpar(fontsize=24, fontface="bold"), vjust=0.8)
subT<-textGrob(subtitle, gp=gpar(fontsize=16, fontface="italic"), vjust=0)

## create dataframes with null values for impactedpeople and 0 for the rest, 
##except target_val and impact_val 
##combine these with nodouble 
## this is an attempt (successful until now) to keep all impact and target entries and have the same dimensions in all plots
## which is a condition to compare global and partner impacted people
a<-length(impact_val)
b<-length(names(nodouble))
c<-length(target_val)
impact_df<-data.frame(matrix(0, nrow = a, ncol = b))
names(impact_df)<-names(nodouble)
impact_df$IMPACT<-impact_val

target_df<-data.frame(matrix(0, nrow = c, ncol = b))
names(target_df)<-names(nodouble)
target_df$LEVEL<-target_val

nodouble_i<-rbind(impact_df,nodouble)
nodouble_t<-rbind(target_df,nodouble)  


##apply same principle for timeline series (target only) and set ENDDATE to dateformat and start date of project-1 to avoid double counts and wide bars
start_date_l<-rep("2012-10-30", times=c)
target_df$ENDDATE<-start_date_l
target_df$ENDDATE<-as.Date(target_df$ENDDATE)
nodouble_date<-rbind(target_df, nodouble_date)


## get subsets and add for each level the overall total people impacted 
IMP<-media_count(nodouble_i)
TARG<-public_count(nodouble_t)
total_impact<-IMP_g[,2]
total_target<-TARG_g[,2]
##need to have same order for both data.frame before combining
IMP_p<-IMP[order(factor(IMP$impact,levels=IMP_g$impact)),]
TARG_p<-TARG[order(factor(TARG$target,levels=TARG_g$target)),]
##combine 
TARG<-cbind(TARG_p,total_target)
IMP<-cbind(IMP_p,total_impact)
##reorder factors to keep order in plot
TARG$target<-factor(TARG$target, levels=TARG$target[order(TARG$total_target)])
IMP$impact<-factor(IMP$impact, levels=IMP$impact[order(IMP$total_impact)])


##call plot function- see in dissanalysis_f.R for plot aesthetics
partner_graph<-summaryplot_f(TARG, IMP, TARG_p, IMP_p, Targeted,,mainT, subT)


## save in svg file with A4 size (ready for paper print)
now<-format(Sys.time(), "%Y%m%d")
filename<-paste(partner_number, "-summary-",now,".png", sep="")
### D ###
##comment/uncomment to output file
ggsave(filename, plot=partner_graph, width=20, height=28, units="cm")
### /D ###


## Call partner timeline 
### E ###
## comment/uncomment to output file. call timeplot_p to plot graph for control
source("/Users/gregoirepaccoud/R/PIRATE/disstimeline_f.R")
partner_timeline(partner_number, nodouble_date, nodouble_g_date)
### /E ###

## Call bubbles
#### F ###
## comment/uncomment to output file. call BUBBLESplot to plot graph for control
source("/Users/gregoirepaccoud/R/PIRATE/dissbubbles_f.R")
### /E ###
