library(ggplot2)
library(gridExtra)

setwd ('/Volumes/HDWL/01.PROJETS EUROP/01.PIRATE/WP6/DISSANALYSIS')

dissdata<-read.csv(file="dissreports150604.csv", header=TRUE)

###GLOBAL COUNT (to set as function>how?)
##subset to eliminate rows with a defined value in column DOUBLE (suppress doubles from dataframe)
ID_nodouble <- subset(dissdata, is.na(dissdata$DOUBLE))
##count overall public targeted 
Targeted<-sum(ID_nodouble$IMPACTEDPEOPLE, na.rm=TRUE)

##count media (impact)
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
IMPACTS$impact

##count publics (levels)
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
TARGET$target

##plot
TARGETplot <- ggplot(TARGET, aes(x=target, y=data))+
  geom_bar(stat="identity") +
  labs(x="",y="", title="Dissemination Levels")+
  coord_flip() + 
  scale_y_reverse()

IMPACTSplot<-ggplot(IMPACTS, aes(x=impact , y=data))+
  geom_bar(stat="identity") +
  labs(x="",y="", title="Impact Levels") +
  coord_flip() 



summaryplot<-arrangeGrob(TARGETplot, IMPACTSplot, ncol=2)


## create svg file for plot
ggsave("testplot2_summary_150606.png", plot=summaryplot)

###END GLOBAL COUNT

