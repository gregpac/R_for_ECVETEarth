### TARGET/IMPACT PLOT ###
##call global_timeline(nodouble_g_date) for overall pirate timeline
##call partner_timeline(nodouble_date, nodouble_g_date) for pirate partner timeline 

##color palette to vizualize the diss activities. To be adapted to the levels' order. 
cbPalette <- c("#56B4E9", "#009E73",  "#F0E442", "#0072B2", "#E69F00", "#D55E00", "#CC79A7")
themecol<-"white"

#get to x = partners ; y = impact level ; dot colour (alpha 0.3) = target
#use geom_jitter


target_impacts<-ggplot(nodouble_g, aes(x=0, y=IMPACT, col=LEVEL))+
geom_jitter(position = position_jitter(height = .4),aes(size=2), alpha=0.8)+
scale_colour_manual(values=cbPalette)+
scale_y_continuous(breaks=seq(0, 8, 1))+
guides(colour = guide_legend(override.aes = list(alpha = 1, size=5)))+
theme(legend.key=element_blank(),
      axis.title.x=element_blank(),
      axis.ticks.x=element_blank(),
      axis.text.x=element_blank(),
      axis.text.y=element_text(face="bold"),
      panel.background=element_rect("black"),
      panel.grid.major=element_line("black"), 
      panel.grid.minor=element_line("black"))+
coord_cartesian(xlim = c(-2, 2))
  
target_impacts

# save in svg file with A4 size (ready for paper print)
now<-format(Sys.time(), "%Y%m%d")
filename<-paste("global-pointsTARG_IMP-",now,".png")
ggsave(filename, plot=target_impacts, width=20, height=28, units="cm")