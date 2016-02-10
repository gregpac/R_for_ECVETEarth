### to do :
### SUMMARYSHEET : add as title the subset (global or partner) for which the bar is plotted
### TIMEGRAPH : script the partner timegraphs (normal and zoom) and integrate in summarysheet
### TIMEGRAPH : add annotations (Pid? place? nbr of impacted people?) for the specific values (bigger than frame and remarquable)
### SUMMARYSHEEET : gather title + bargraph + timegraph in 1 A4 svg document
### BUBBLES : make a nice viz that shows the levels (colours, same as timegraph) + impacts (2 impact levels with alpha) as disks of area=impacted people
### SUBSETTING : try to rewrite with [ function instead of subset()


### HOW-TO :
### instructions for a letter (e.g. ### A ###) are given for the lines between the letter and its closing tag (e.g. starts at ### A ### and ends at ### /A ###) 
### check the filename and its location at letter ### A ###
### enter the partner number at letter  ### B ### 

library(ggplot2)
library(gridExtra)
library(scales)

### A ###
setwd ('/Volumes/HDWL/01.PROJETS EUROP/01.PIRATE/WP6/DISSANALYSIS')
dissdata<-read.csv(file="dissreports150604.csv", header=TRUE, stringsAsFactors=FALSE)
### /A ###

##subset to eliminate rows with a defined value in column DOUBLE (suppress doubles from dataframe)
global_nodouble <- subset(dissdata, is.na(dissdata$DOUBLE))
##count overall public targeted 
Targeted<-sum(global_nodouble$IMPACTEDPEOPLE, na.rm=TRUE)


### B ###
# define partner number ("PXX") or overall ("Global")
partner_number<-"Global"
### /B ###

if (partner_number=='Global'){
  partner<-"P*"
} else {partner<-partner_number}

nodouble<- subset(global_nodouble, grepl(partner, PARTNER)) 
nodouble<- nodouble[complete.cases(nodouble[,8]),]
#previously used for subsetting, changed because not allowing regex : 
#global_nodouble[global_nodouble$PARTNER %in% partner,]
targeted_p<-sum(nodouble$IMPACTEDPEOPLE, na.rm=TRUE)
  
# convert ENDDATE to date format
nodouble$ENDDATE<-as.Date(nodouble$ENDDATE,)

#delete NA for dates and extracting start and end dates
nodouble_date <- subset(nodouble, !is.na(nodouble$ENDDATE))
start_date<-min( nodouble_date[ , "ENDDATE" ] )
end_date<-max( nodouble_date[ , "ENDDATE" ] )


##setup title: function to create title with subs 
titleGrob <- function(x=c("First line", "second line"), size=10, ...){
  n <- length(x)
  size <- rep(size, length.out=n)
  one_label <- function(x, size, ...)
    textGrob(x, gp=gpar(fontsize=size), ...)
  lg <- mapply(one_label, x=x, size=size, ..., SIMPLIFY=FALSE)
  
  wg <- lapply(lg, grobWidth) 
  hg <- lapply(lg, grobHeight) 
  
  widths <- do.call(unit.c, wg)
  heights <- do.call(unit.c, hg) 
  
  maxwidth <- max(widths)
  g <- frameGrob(layout = grid.layout(n, 1, width=maxwidth, height=heights) )
  for(ii in seq_along(lg))
    g <- placeGrob(g, lg[[ii]], row=ii)
  
  g
}


##SUMMARY AS BAR GRAPH
#to get summary for a specific partner, call IMP_TARG_BAR(partner="number of partner")

#count media (impact levels)
media_count<-function (nodouble){
  impactVar<- split (nodouble, nodouble$IMPACT)
  
  df_<-rbind(by(df[,3],df[,2],sum) )
  nodouble<-
  IMPACTS <- rbind(by(nodouble[,8],nodouble[,2],sum))
  
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
IMPACTS<-data.frame(impact=impact_level, people=impact_data)
IMPACTS<-IMPACTS[order(IMPACTS$people), ]
IMPACTS$impact<-factor(IMPACTS$impact, levels=IMPACTS$impact[order(IMPACTS$people)])
return(IMPACTS)
}

#count publics (dissemination levels)
public_count<-function (nodouble){
ID_O_ <- subset (nodouble, nodouble$LEVEL=="O")
ID_C_ <- subset (nodouble, nodouble$LEVEL=="C")
ID_T_ <- subset (nodouble, nodouble$LEVEL=="T")
ID_S_ <- subset (nodouble, nodouble$LEVEL=="S")
ID_E_ <- subset (nodouble, nodouble$LEVEL=="E")
ID_P_ <- subset (nodouble, nodouble$LEVEL=="P")
ID_G_ <- subset (nodouble, nodouble$LEVEL=="G")
ID_O<-sum(ID_O_$IMPACTEDPEOPLE, na.rm=TRUE)
ID_C<-sum(ID_C_$IMPACTEDPEOPLE, na.rm=TRUE)
ID_T<-sum(ID_T_$IMPACTEDPEOPLE, na.rm=TRUE)
ID_S<-sum(ID_S_$IMPACTEDPEOPLE, na.rm=TRUE)
ID_E<-sum(ID_E_$IMPACTEDPEOPLE, na.rm=TRUE)
ID_P<-sum(ID_P_$IMPACTEDPEOPLE, na.rm=TRUE)
ID_G<-sum(ID_G_$IMPACTEDPEOPLE, na.rm=TRUE)
target_level<-c("O", "C","T", "S", "E", "P","G")
target_data<-c(ID_O,ID_C, ID_T, ID_S, ID_E, ID_P, ID_G)
TARGET<-data.frame(target=target_level, people=target_data)
TARGET<-TARGET[order(TARGET$people),]
TARGET$target<-factor(TARGET$target, levels=TARGET$target[order(TARGET$people)])
return(TARGET)
}

## BARPLOT function to plot side by side bar charts and table of numeric values. 
# All charts have the same scale to allow easy comparison. 
# zoom can be changed to adapt for each graph, Targeted remains global to allow comparison with the total of the consortium
# use 3000 for zoom for optimal view with interim diss. 
BARPLOT<- function(TARG,IMP,Targeted, zoom=3000)
{
  table_limit<-Targeted-10000
  table_zero<-(Targeted/2)+10000
  TARGETplot_overall <- ggplot(TARG, aes(x=target, y=people))+
    geom_bar(stat="identity") +
    labs(x="",y="", title="People/dissemination levels")+
    scale_y_continuous(limits=c(0, Targeted), breaks=seq(0,Targeted,100000), labels=comma) +
    theme_bw()+
    annotation_custom(tableGrob(TARG[rev(rownames(TARG)),], gpar.coretext = gpar(fontsize = 11), show.rownames=FALSE), xmin=1.5, xmax=4, ymin=table_zero, ymax=table_limit)+
    coord_flip()  
  
  IMPACTSplot_overall<-ggplot(IMP, aes(x=impact , y=people))+
    geom_bar(stat="identity") +
    labs(x="",y="", title="People/impact levels") +
    scale_y_continuous(limits=c(0, Targeted), breaks=seq(0,Targeted,100000), labels=comma) +
    theme_bw()+
    annotation_custom(tableGrob(IMP[rev(rownames(IMP)),], gpar.coretext = gpar(fontsize = 11), show.rownames=FALSE), xmin=2, xmax=5, ymin=table_zero, ymax=table_limit)+
    coord_flip() 
  
  TARGETplot_zoom <- ggplot(TARG, aes(x=target, y=people))+
    geom_bar(stat="identity", fill="grey") +
    labs(x="",y="")+
   # scale_y_continuous(breaks=seq(0,zoom,zoom/5), labels=comma) +
    theme_bw()+
    coord_flip(ylim=c(0, zoom))  
  
  IMPACTSplot_zoom <-ggplot(IMP, aes(x=impact , y=people))+
    geom_bar(stat="identity", fill="grey") +
    labs(x="",y="") +
   # scale_y_continuous(breaks=seq(0,zoom,zoom/5), labels=comma) +
    theme_bw()+
    coord_flip(ylim=c(0, zoom)) 
 
  
  #title with partner number and immpacted people count
  #subs with 1st and last entries' dates, and total consortium impacted people  
  titre<-titleGrob(x=c("Titre test", "subs test"), size=c(18,12))
   # proposed not ok : constante de type chaîne de caractères inattendu(e)
  # partner"-count",start_date" to "end_date" / total impacted people="Targeted
  summaryplot<-arrangeGrob(TARGETplot_overall, IMPACTSplot_overall, TARGETplot_zoom, IMPACTSplot_zoom, ncol=2, nrow=2, main=titre)                   
  return(summaryplot)
}

## GLOBAL ANALYSIS (to be removed if automated orks fine)
##IMP_global<-media_count(global_nodouble)
##TARG_global<-public_count(global_nodouble)
##summaryplot<-BARPLOT(TARG_global, IMP_global,Targeted,)
##summaryplot
## STOP HERE FOR TEST WITH GLOBAL VALUES
##ggsave("global_summary_150610.png", plot=summaryplot)
## /GLOBAL ANALYSIS

## PARTNER ANALYSIS -comment and uncomment to switch from plot to saving file
bargraph<- function(nodouble)
{
  TARG<-public_count(nodouble)
  IMP<-media_count(nodouble)
  partner_graph<-BARPLOT(TARG, IMP, Targeted,)
  
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
  
  
  # create svg file for plot : activate ggsave when ready...
  now<-format(Sys.time(), "%Y%m%d")
  filename<-paste(partner_number, "-summary-",now,".svg", sep="")
  #ggsave(filename, plot=partner_graph)
  return(partner_graph)
}

bargraph(partner_number)

## END BAR GRAPH SUMMARY ##



## TIMELINE ANALYSIS ##

#sort dates starting from earlier entry !CHECK IF REQUIRED, DELETE IF NOT!
date <- subset(global_nodouble, !is.na(global_nodouble$ENDDATE))
#date<-date[order(date$ENDDATE),]

#plot timegraph
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


## END TIMELINE ANALYSIS ##


