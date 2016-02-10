### create bubble chart plot with pirate dissemination data

# BUBBLES : Aim= show the activity, = the number of activities for each level (ordered as for impacted people)
# make a nice viz that shows the levels (colours, same as timegraph) + impacts (2 impact levels with alpha)

#from nodouble count the number of activities for each target
#distinguish the activities in wide impact and in specific. 

### uncomment if using the GLOBALCOUNT file for running this file
### comment if using the file PARTNERCOUNT file for running this file
nodouble<-nodouble_g
partner_number<-"Global"

#1. count total, write partner name in title
target_count<-as.data.frame(table(nodouble$LEVEL))
bubbles_titre<-paste(partner_number,"- Pirate dissemination activities per level")
#2. count the activities for specific only
nodouble_s<-nodouble[nodouble$IMPACT %in% c(4,6,7,8),]
target_s_count<-as.data.frame(table(nodouble_s$LEVEL))
# give names to columns for easy binding
colnames(target_s_count)<-c("target","Freq.s")
colnames(target_count)<-c("target","Freq.g")
#3. merge total and specific
targets<-merge(target_count, target_s_count, by="target", all=TRUE)

#4. create data frame for x y values (for plot)
T_levels<-unique(nodouble$LEVEL)
###use y0 for all y values to zero ()
##(or change zero to other constant value)
xy0<-rep(0,length(T_levels)) 
###or define manually y for each target level [[[NOT FUNCTIONAL YET]]]: 
xyC<-c("C", 2, 0) 
xyE<-c("E", 2, 0)
xyG<-c("G", 5, 0)
xyO<-c("O", 2, 0)
xyP<-c("P", 5, 0)
xyS<-c("S", 1, 0)
xyT<-c("T", 1, 0)
###create data frame from these, give names for easy merging
xyLevels<- as.data.frame(rbind(xyC,xyE,xyG,xyO,xyP,xyS,xyT))
colnames(xyLevels)<-c("target","y", "x")
###here choose y0 for same y value or yLevels for manually defined y (and x?)
xy<-data.frame(xy=xy0, target=T_levels)

#5. merge activity count with coordinates
targets_xy<-merge(targets,xy, by="target", all=TRUE)

#6. plot bullseye with total first, alpha = 0.5, and specific fill over total (same center)
BUBBLESplot<-ggplot(targets_xy, aes(x=target, y=xy, size=Freq.g))+
  geom_point(alpha=0.5)+
  geom_point(data=targets_xy, aes(x=target,y=xy, size=Freq.s), alpha=0.8)+
  scale_size_identity(guide="none", breaks=c(10,20,40))+
  guides(size = guide_legend(override.aes = list(shape = 1)))+
  ggtitle(bubbles_titre)+
  theme(axis.title=element_blank(), 
      axis.text.y=element_blank(), 
      axis.ticks=element_blank(), 
      axis.line=element_blank(), 
      legend.key=element_rect(fill="white"),
      legend.title=element_blank(), 
      legend.position="bottom",
      panel.background=element_rect(fill = "white"))


#7. save in svg/png file 
now<-format(Sys.time(), "%Y%m%d")
filename<-paste(partner_number,"-Diss activities-",now,".png")
### comment/uncomment to output file
ggsave(filename, plot=BUBBLESplot, width=28, height=20, units="cm")
###
