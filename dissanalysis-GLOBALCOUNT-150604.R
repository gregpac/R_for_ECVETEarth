library(ggplot2)
library(gridExtra)
library(scales)

setwd ('/Volumes/HDWL/01.PROJETS EUROP/01.PIRATE/WP6/DISSANALYSIS')

dissdata<-read.csv(file="dissreports150604.csv", header=TRUE)

###SUMMARY AS BAR GRAPH
###To get summary for a specific partner, call IMP_TARG_BAR(partner="number of partner")
### to do :
### add as title the subset (global or partner) for which the bar is plotted
### update parters plot function with printed values (see ###GLOBAL ANALYSIS) 
### script the partner timegraphs (normal and zoom)
### add annotations (Pid? place? nbr of impacted people?) for the specific values (bigger than frame and remarquable)

##subset to eliminate rows with a defined value in column DOUBLE (suppress doubles from dataframe)
global_nodouble <- subset(dissdata, is.na(dissdata$DOUBLE))

##count overall public targeted 
Targeted<-sum(global_nodouble$IMPACTEDPEOPLE, na.rm=TRUE)

##count media (impact levels)
media_count<-function (ID_nodouble){
ID_1_<-subset(ID_nodouble, ID_nodouble$IMPACT=="1")
ID_2_<-subset(ID_nodouble, ID_nodouble$IMPACT=="2")
ID_3_<-subset(ID_nodouble, ID_nodouble$IMPACT=="3")
ID_4_<-subset(ID_nodouble, ID_nodouble$IMPACT=="4")
ID_5_<-subset(ID_nodouble, ID_nodouble$IMPACT=="5")
ID_6_<-subset(ID_nodouble, ID_nodouble$IMPACT=="6")
ID_7_<-subset(ID_nodouble, ID_nodouble$IMPACT=="7")
ID_8_<-subset(ID_nodouble, ID_nodouble$IMPACT=="8")
ID_1<-sum(ID_1_$IMPACTEDPEOPLE, na.rm=TRUE)
ID_2<-sum(ID_2_$IMPACTEDPEOPLE, na.rm=TRUE)
ID_3<-sum(ID_3_$IMPACTEDPEOPLE, na.rm=TRUE)
ID_4<-sum(ID_4_$IMPACTEDPEOPLE, na.rm=TRUE)
ID_5<-sum(ID_5_$IMPACTEDPEOPLE, na.rm=TRUE)
ID_6<-sum(ID_6_$IMPACTEDPEOPLE, na.rm=TRUE)
ID_7<-sum(ID_7_$IMPACTEDPEOPLE, na.rm=TRUE)
ID_8<-sum(ID_8_$IMPACTEDPEOPLE, na.rm=TRUE)
impact_level<-c("1","2", "3", "4", "5", "6", "7", "8")
impact_data<-c(ID_1, ID_2, ID_3, ID_4, ID_5, ID_6, ID_7, ID_8)
IMPACTS<-data.frame(impact=impact_level, data=impact_data)
IMPACTS<-IMPACTS[order(IMPACTS$data), ]
IMPACTS$impact<-factor(IMPACTS$impact, levels=IMPACTS$impact[order(IMPACTS$data)])
return(IMPACTS)
}

##count publics (dissemination levels)
public_count<-function (ID_nodouble){
ID_O_ <- subset (ID_nodouble, ID_nodouble$LEVEL=="O")
ID_C_ <- subset (ID_nodouble, ID_nodouble$LEVEL=="C")
ID_T_ <- subset (ID_nodouble, ID_nodouble$LEVEL=="T")
ID_S_ <- subset (ID_nodouble, ID_nodouble$LEVEL=="S")
ID_E_ <- subset (ID_nodouble, ID_nodouble$LEVEL=="E")
ID_P_ <- subset (ID_nodouble, ID_nodouble$LEVEL=="P")
ID_G_ <- subset (ID_nodouble, ID_nodouble$LEVEL=="G")
ID_O<-sum(ID_O_$IMPACTEDPEOPLE, na.rm=TRUE)
ID_C<-sum(ID_C_$IMPACTEDPEOPLE, na.rm=TRUE)
ID_T<-sum(ID_T_$IMPACTEDPEOPLE, na.rm=TRUE)
ID_S<-sum(ID_S_$IMPACTEDPEOPLE, na.rm=TRUE)
ID_E<-sum(ID_E_$IMPACTEDPEOPLE, na.rm=TRUE)
ID_P<-sum(ID_P_$IMPACTEDPEOPLE, na.rm=TRUE)
ID_G<-sum(ID_G_$IMPACTEDPEOPLE, na.rm=TRUE)
target_level<-c("O", "C","T", "S", "E", "P","G")
target_data<-c(ID_O,ID_C, ID_T, ID_S, ID_E, ID_P, ID_G)
TARGET<-data.frame(target=target_level, data=target_data)
TARGET<-TARGET[order(TARGET$data),]
TARGET$target<-factor(TARGET$target, levels=TARGET$target[order(TARGET$data)])
return(TARGET)
}

## BARPLOT function to plot side by side bar charts with x axis starting from the middle and table of numeric values. 
## All charts have the same scale to allow easy comparison. 
## use 200000 for Targeted for interim diss. optimum view 
BARPLOT<- function (TARG,IMP, Targeted)
{
  TARGETplot <- ggplot(TARG, aes(x=target, y=data))+
    geom_bar(stat="identity") +
    labs(x="",y="", title="Dissemination Levels")+
    scale_y_continuous(limits=c(0, Targeted))+
    theme_bw()+
    annotation_custom(tableGrob(TARG[rev(rownames(TARG)),], gpar.coretext = gpar(fontsize = 11), show.rownames=FALSE), xmin=1, xmax=4, ymin=-90000, ymax=-190000)+
    coord_flip() + 
    scale_y_reverse()
  
  IMPACTSplot<-ggplot(IMP, aes(x=impact , y=data))+
    geom_bar(stat="identity") +
    labs(x="",y="", title="Impact Levels") +
    scale_y_continuous(limits=c(0, Targeted))+
    theme_bw()+
    annotation_custom(tableGrob(IMP[rev(rownames(IMP)),], gpar.coretext = gpar(fontsize = 11), show.rownames=FALSE), xmin=2, xmax=4, ymin=90000, ymax=190000)+
    coord_flip() 
  
  summaryplot<-arrangeGrob(TARGETplot, IMPACTSplot, ncol=2)
  return(summaryplot)
}

partner_bar<- function(partner)
{
  nodouble<- subset(global_nodouble, PARTNER==partner)
  IMP<-media_count(nodouble)
  TARG<-public_count(nodouble)
  summaryplot<-BARPLOT(TARG, IMP, Targeted)
  
  ##first plot code, giving errors
#  TARGETplot <- ggplot(TARG, aes(x=target, y=data)) +
#    geom_bar(stat="identity") +
#    labs(x="",y="", title="Dissemination Levels") +
#    scale_y_continuous(trans="reverse", expand=c(0,0), limits=c(0, 200000)) +
#    theme_bw() +
#    annotation_custom(tableGrob(TARG[rev(rownames(TARG)),], gpar.coretext = gpar(fontsize = 11), show.rownames=FALSE), xmin=1, xmax=4, ymin=-90000, ymax=-190000) +
#    coord_flip() 
  
#  IMPACTSplot<-ggplot(IMP, aes(x=impact , y=data)) +
#    geom_bar(stat="identity") +
#    labs(x="",y="", title="Impact Levels") +
#    scale_y_continuous(expand=c(0,0), limits=c(0, 200000)) +
#    theme_bw() +
#    annotation_custom(tableGrob(IMP[rev(rownames(IMP)),], gpar.coretext = gpar(fontsize = 11), show.rownames=FALSE), xmin=3, xmax=5, ymin=90000, ymax=190000) +
#    coord_flip()  
  
#  summaryplot<-arrangeGrob(TARGETplot, IMPACTSplot, ncol=2)
  
  ## create svg file for plot 
  now<-format(Sys.time(), "%Y%m%d")
  filename<-paste(partner, "-summary-",now,".png", sep="")
  ggsave(filename, plot=summaryplot)
  
}

###GLOBAL ANALYSIS

IMP_global<-media_count(global_nodouble)
TARG_global<-public_count(global_nodouble)

ggsave("global_summary_150607.png", plot=summaryplot)

###END BAR GRAPH SUMMARY###

###TIMELINE ANALYSIS###
## convert to date and sort starting from earlier entry

global_nodouble$ENDDATE<-as.Date(global_nodouble$ENDDATE,)
date <- subset(global_nodouble, !is.na(global_nodouble$ENDDATE))
#date<-date[order(date$ENDDATE),]

##plot timegraph
cbPalette <- c("#56B4E9", "#009E73",  "#F0E442", "#0072B2", "#E69F00", "#D55E00", "#CC79A7")

timegraph<-ggplot(date, aes(ENDDATE, IMPACTEDPEOPLE)) + geom_bar(aes(fill=LEVEL), stat='identity') +
  scale_x_date(breaks = date_breaks("2 month"),
               labels = date_format("%b %y")) +
  scale_fill_manual(values=cbPalette) +
  theme_bw()+
  coord_cartesian(ylim = c(0, 510)) +
  labs(x="", y="Impacted People")

timegraph_zoom<-ggplot(date, aes(ENDDATE, IMPACTEDPEOPLE)) + geom_bar(aes(fill=LEVEL), stat='identity') +
  scale_x_date(breaks = date_breaks("2 month"),
               labels = date_format("%b %y")) +
  scale_fill_manual(values=cbPalette) +
  theme_bw()+
  coord_cartesian(ylim = c(0, 80)) +
  labs(x="", y="Impacted People")

ggsave("global_timegraph_150608.png", plot = timegraph, width = 40, height = 10, units = "cm", dpi = 300)
ggsave("global_timegraph_zoom_150608.png", plot = timegraph_zoom, width = 40, height = 10, units = "cm", dpi = 300)


###END TIMELINE ANALYSIS###


