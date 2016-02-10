
## TIMELINE ANALYSIS ##

#sort dates starting from earlier entry !CHECK IF REQUIRED, DELETE IF NOT!
date <- subset(global_nodouble, !is.na(global_nodouble$ENDDATE))
#date<-date[order(date$ENDDATE),]

##color palette to vizualize the diss activities. To be adapted to the levels' order. 
cbPalette <- c("#56B4E9", "#009E73",  "#F0E442", "#0072B2", "#E69F00", "#D55E00", "#CC79A7")


#function to plot global timegraph 
timegraph_global<-function (nodouble_date_g){
timegraph_g<-ggplot(nodouble_date_g, aes(ENDDATE, IMPACTEDPEOPLE)) + geom_bar(aes(fill=LEVEL), stat='identity') +
  scale_x_date(breaks = date_breaks("2 month"),
               labels = date_format("%b %y")) +
  scale_fill_manual(values=cbPalette) +
  theme_bw()+
  coord_cartesian(ylim = c(0, 510)) +
  labs(x="", y="Impacted People")
return(timegraph_g)
}

timegraph_zoom<-ggplot(date, aes(ENDDATE, IMPACTEDPEOPLE)) + geom_bar(aes(fill=LEVEL), stat='identity') +
  scale_x_date(breaks = date_breaks("2 month"),
               labels = date_format("%b %y")) +
  scale_fill_manual(values=cbPalette) +
  theme_bw()+
  coord_cartesian(ylim = c(0, 80)) +
  labs(x="", y="Impacted People")

#ggsave("global_timegraph_150608.png", plot = timegraph, width = 40, height = 10, units = "cm", dpi = 300)
#ggsave("global_timegraph_zoom_150608.png", plot = timegraph_zoom, width = 40, height = 10, units = "cm", dpi = 300)


## END TIMELINE ANALYSIS ##



