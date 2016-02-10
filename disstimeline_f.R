
### TIMELINE PLOTS ###
##call global_timeline(nodouble_g_date) for overall pirate timeline
##call partner_timeline(nodouble_date, nodouble_g_date) for pirate partner timeline 

##color palette to vizualize the diss activities. To be adapted to the levels' order. 
cbPalette <- c("#56B4E9", "#009E73",  "#F0E442", "#0072B2", "#E69F00", "#D55E00", "#CC79A7")
time_zoom<- 51
themecol<-"white"

global_timeline<-function(nodouble_g_date) {
##define Title for global timeline plot
mainTT_g<-textGrob("Pirate dissemination timeline", gp=gpar(fontsize=24, fontface="bold"), vjust=0.8)

##GLOBAL PLOT
TIMEGRAPH_global<-ggplot(nodouble_g_date, aes(ENDDATE, IMPACTEDPEOPLE)) + geom_bar(aes(fill=LEVEL), stat='identity') +
  scale_x_date(breaks = date_breaks("2 month"),
               labels = date_format("%b %y")) +
  scale_fill_manual(values=cbPalette) +
  theme(axis.title.x=element_blank(),    
        legend.position="none",
        axis.title.x=element_blank(),
        panel.grid.major=element_line(colour="darkgrey", linetype="dotted"),
        panel.grid.minor=element_line(colour=themecol),
        panel.background=element_rect(fill =themecol))+
  coord_cartesian(ylim = c(0, 510)) +
  ylab("Impacted People")
  #guides(fill=FALSE)

TIMEGRAPH_global_zoom<-ggplot(nodouble_g_date, aes(ENDDATE, IMPACTEDPEOPLE)) + geom_bar(aes(fill=LEVEL), stat='identity') +
  scale_x_date(breaks = date_breaks("2 month"),
               labels = date_format("%b %y")) +
  scale_fill_manual(values=cbPalette, guide = guide_legend(title=NULL, direction = "horizontal", label.position="bottom", label.hjust = 0.5, label.vjust = 0.5, keywidth = 1, keyheight = 1)) +
  theme(axis.title.x=element_blank(),    
        legend.position="bottom",
        axis.title.x=element_blank(),
        axis.title.y=element_text(colour="grey"),
        panel.grid.major=element_line(colour="darkgrey", linetype="dotted"),
        panel.grid.minor=element_line(colour=themecol),
        panel.background=element_rect(fill = themecol))+
  coord_cartesian(ylim = c(0, time_zoom)) +
  ylab("Impacted People")

timeplot_g<-arrangeGrob(TIMEGRAPH_global, TIMEGRAPH_global_zoom, nrow=2, main=mainTT_g)                   

# save in svg file with A4 size (ready for paper print)
now<-format(Sys.time(), "%Y%m%d")
filename<-paste("global-timeline-",now,".png")
ggsave(filename, plot=timeplot_g, width=28, height=20, units="cm")
return(timeplot_g)
}

#global_timeline(nodouble_g_date)

##PARTNER PLOT

partner_timeline<-function(partner_number, nodouble_date, nodouble_g_date) {
  ##define Title for partner timeline plot
  partner_tt<-paste(partner_number, "dissemination timeline")
  mainTT<-textGrob(partner_tt, gp=gpar(fontsize=24, fontface="bold"), vjust=0.8)
  
  ##PARTNER PLOT
  TIMEGRAPH_partner<-ggplot(nodouble_date, aes(ENDDATE, IMPACTEDPEOPLE, width=1)) + 
    geom_bar(aes(fill=LEVEL), stat='identity') +
    geom_bar(data=nodouble_g_date, aes(ENDDATE, IMPACTEDPEOPLE), stat='identity', alpha=0.2)+
    scale_x_date(breaks = date_breaks("2 month"),
                 labels = date_format("%b %y")) +
    scale_fill_manual(values=cbPalette) +
    theme(axis.title.x=element_blank(),    
          legend.position="none",
          axis.title.x=element_blank(),
          panel.grid.major=element_line(colour="darkgrey", linetype="dotted"),
          panel.grid.minor=element_line(colour=themecol),
          panel.background=element_rect(fill = themecol))+
    coord_cartesian(ylim = c(0, 510)) +
    labs(x="", y="Impacted People")+
    guides(fill=FALSE)
  
  TIMEGRAPH_partner_zoom<-ggplot(nodouble_date, aes(ENDDATE, IMPACTEDPEOPLE, width=1)) + 
    geom_bar(aes(fill=LEVEL), stat='identity') +
    geom_bar(data=nodouble_g_date, aes(ENDDATE, IMPACTEDPEOPLE), stat='identity', alpha=0.2)+
    scale_x_date(breaks = date_breaks("2 month"),
                 labels = date_format("%b %y")) +
    scale_fill_manual(values=cbPalette,
                      guide = guide_legend(title=NULL, direction = "horizontal", label.position="bottom", label.hjust = 0.5, label.vjust = 0.5, keywidth = 1, keyheight = 1)) +
    theme(axis.title.x=element_blank(),    
          legend.position="bottom",
          axis.title.x=element_blank(),
          axis.title.y=element_text(colour="grey"),
          panel.grid.major=element_line(colour="darkgrey", linetype="dotted"),
          panel.grid.minor=element_line(colour=themecol),
          panel.background=element_rect(fill = themecol))+
    coord_cartesian(ylim = c(0, time_zoom)) +
    labs(x="", y="Impacted People")+
    theme(legend.position="bottom")
  
  timeplot_p<-arrangeGrob(TIMEGRAPH_partner, TIMEGRAPH_partner_zoom, nrow=2, main=mainTT)                   

# save in svg file with A4 size (ready for paper print)
now<-format(Sys.time(), "%Y%m%d")
filename<-paste(partner_number,"-timeline-",now,".png")
ggsave(filename, plot=timeplot_p, width=28, height=20, units="cm")
return(timeplot_p)
}




