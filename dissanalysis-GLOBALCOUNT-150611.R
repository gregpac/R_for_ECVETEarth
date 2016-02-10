### This is the file for getting overall (global) dissemination analyses data and plot.
### result stored in variable : summaryplot_g 

##Uncomment until ### /A ### and check path in ### A ### if using the file as standalone for viz of global data

rm(list = ls())
library(ggplot2)
library(gridExtra)
library(scales)
source("/Users/gregoirepaccoud/R/PIRATE/dissanalysis_f.R")


### A ### 
setwd ('/Volumes/HDWL/01.PROJETS-SUBV/01.PIRATE/WP6/DISSANALYSIS')
dissdata<-read.csv(file="dissem_151119.csv", header=TRUE, stringsAsFactors=FALSE)

##global_nodouble and Targeted are intiated before calling for GLOBALCOUNT
##subset to eliminate rows with a defined value in column DOUBLE (suppress doubles from dataframe)
global_nodouble <- subset(dissdata, is.na(dissdata$DOUBLE))
##count overall public targeted 
Targeted<-sum(global_nodouble$IMPACTEDPEOPLE, na.rm=TRUE)

nodouble_g<- subset(global_nodouble, grepl("P*", PARTNER)) 
nodouble_g<- nodouble_g[complete.cases(nodouble_g[,8]),]


###call Bubbleplot file
#source("/Users/gregoirepaccoud/R/PIRATE/dissbubbles_f.R")

###p is defined to be a replacement box for (yet) unavailable content/plot
p = rectGrob()
### /A ###


# convert ENDDATE to date format
nodouble_g$ENDDATE<-as.Date(nodouble_g$ENDDATE,)

#delete NA for dates and extracting start and end dates
nodouble_g_date <- subset(nodouble_g, !is.na(nodouble_g$ENDDATE))
start_date_g<-min( nodouble_g_date[ , "ENDDATE" ] )
end_date_g<-max( nodouble_g_date[ , "ENDDATE" ] )

##maintitle and sub title for global plot, same format as partner plots 
maintitle<-paste(" Pirate consortium dissemination -", start_date_g, "to", end_date_g)
subtitle<-paste(Targeted, "impacted people")
mainT<-textGrob(maintitle, gp=gpar(fontsize=20, fontface="bold"), vjust=0.8)
subT<-textGrob(subtitle, gp=gpar(fontsize=16, fontface="italic"), vjust=0)

#get global counts for impact and target 
media_count<-function (nodouble){
  IMPACTS <- rbind(by(nodouble[,8],nodouble[,2],sum))
  IMPACTS<-t(IMPACTS)
  IMPACTS <- as.data.frame(IMPACTS)
  names(IMPACTS)<-"people"
  impact <-rownames(IMPACTS)
  IMPACTS <- cbind(impact,IMPACTS)
  IMPACTS <- IMPACTS[order(IMPACTS$people),]
  IMPACTS$impact<-factor(IMPACTS$impact, levels=IMPACTS$impact[order(IMPACTS$people)])
  return(IMPACTS)
}

##count publics (dissemination levels)
public_count<-function (nodouble){
  TARGET<-rbind(by(nodouble[,8], nodouble[,7], sum))
  TARGET<-t(TARGET)
  TARGET<-as.data.frame(TARGET)
  names(TARGET)<-"people"
  target<-rownames(TARGET)
  TARGET<-cbind(target,TARGET)
  TARGET<-TARGET[order(TARGET$people),]
  TARGET$target<-factor(TARGET$target, levels=TARGET$target[order(TARGET$people)])
  return(TARGET)
}


IMP_g<-media_count(nodouble_g)
TARG_g<-public_count(nodouble_g)

##create additionnal column for Y value for bubble plot
y<-1
TARG_g_w<-TARG_g
TARG_g_w$y<-y


##plot global graphs 
table_limit<-Targeted-10000
table_zero<-(Targeted/2)+10000
zoom<-3000
TARGETplot_overall <- ggplot(TARG_g, aes(x=target, y=people))+
  geom_bar(stat="identity") +
  labs(x="",y="", title="People/dissemination levels")+
  scale_y_continuous(limits=c(0, Targeted), breaks=seq(0,Targeted,250000), labels=comma) +
  theme_bw()+
  annotation_custom(tableGrob(TARG_g[rev(rownames(TARG_g)),], gpar.coretext = gpar(fontsize = 11), show.rownames=FALSE), xmin=1.5, xmax=4, ymin=table_zero, ymax=table_limit)+
  coord_flip()  

IMPACTSplot_overall<-ggplot(IMP_g, aes(x=impact , y=people))+
  geom_bar(stat="identity") +
  labs(x="",y="", title="People/impact levels") +
  scale_y_continuous(limits=c(0, Targeted), breaks=seq(0,Targeted,250000), labels=comma) +
  theme_bw()+
  annotation_custom(tableGrob(IMP_g[rev(rownames(IMP_g)),], gpar.coretext = gpar(fontsize = 11), show.rownames=FALSE), xmin=2, xmax=5, ymin=table_zero, ymax=table_limit)+
  coord_flip() 

TARGETplot_zoom <- ggplot(TARG_g, aes(x=target, y=people))+
  geom_bar(stat="identity", fill="grey") +
  labs(x="",y="")+
  theme_bw()+
  coord_flip(ylim=c(0, zoom))  

IMPACTSplot_zoom <-ggplot(IMP_g, aes(x=impact , y=people))+
  geom_bar(stat="identity", fill="grey") +
  labs(x="",y="") +
  theme_bw()+
  coord_flip(ylim=c(0, zoom)) 

summaryplot_g<-arrangeGrob(arrangeGrob(TARGETplot_overall, IMPACTSplot_overall, TARGETplot_zoom, IMPACTSplot_zoom, widths=c(1/2, 1/2), ncol=2), BUBBLESplot_g, heights=c(2/3,1/3), nrow=2, main=mainT, sub=subT)                   
summaryplot_g_paper<-arrangeGrob(TARGETplot_overall, IMPACTSplot_overall, TARGETplot_zoom, IMPACTSplot_zoom, nrow=2, main=mainT, sub=subT)

# save in svg file with A4 size (ready for paper print)
now<-format(Sys.time(), "%Y%m%d")
filename<-paste("global-summary-",now,".png")
### comment/uncomment to output file
ggsave(filename, plot=summaryplot_g_paper, width=20, height=28, units="cm")
###

###Call timeline for global
#source("/Users/gregoirepaccoud/R/PIRATE/disstimeline_f.R")
#global_timeline(nodouble_g_date)



