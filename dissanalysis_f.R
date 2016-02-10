### FUNCTIONS FILE FOR PIRATE DISSEMINATION ANALYSIS
### list of functions and last update : 
### - media_count (11/06/2015) : sum of impacted people for each impact level
### - public_count (11/06/2015) : sum of impacted people for each dissemination level
### - summaryplot_f (10/06/2015)[INCOMPLETE] : plot all graph for selected partner as one A4 with title


##count media (impact levels)
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


## summaryplot function to plot side by side bar charts and table of numeric values. 
# All charts have the same scale to allow easy comparison. 
# zoom can be changed to adapt for each graph, Targeted remains global to allow comparison with the total of the consortium
# use 3000 for zoom for optimal view with interim diss.
summaryplot_f<- function(TARG,IMP,TARG_p, IMP_p, Targeted, zoom=3000, mainT="PIRATE Dissemination", subT="")
{
  table_limit<-Targeted-100000
  table_zero<-(Targeted/2)+10000
  alpha_g<-0.2
  
  TARGETplot_overall <- ggplot(data=TARG, aes(x=target, y=people))+
    geom_bar(stat="identity", fill="black")+
    geom_bar(data=TARG, aes(x=target, y=total_target),stat="identity", alpha=alpha_g) +
    labs(x="",y="", title="People/dissemination levels")+
    scale_y_continuous(limits=c(0, table_limit), breaks=seq(0,Targeted,250000), labels=comma) +
    theme_bw()+
    annotation_custom(tableGrob(TARG_p[rev(rownames(TARG_p)),], gpar.coretext = gpar(fontsize = 11), show.rownames=FALSE), xmin=1.5, xmax=4, ymin=table_zero, ymax=table_limit-50000)+
    coord_flip()  
  
  IMPACTSplot_overall<-ggplot(data=IMP, aes(x=impact , y=people))+
    geom_bar(stat="identity", fill="black") +
    geom_bar(data=IMP, aes(x=impact, y=total_impact), stat="identity", alpha=alpha_g) +
    labs(x="",y="", title="People/impact levels") +
    scale_y_continuous(limits=c(0, table_limit), breaks=seq(0,Targeted,100000), labels=comma) +
    theme_bw()+
    annotation_custom(tableGrob(IMP_p[rev(rownames(IMP_p)),], gpar.coretext = gpar(fontsize = 11), show.rownames=FALSE), xmin=2, xmax=5, ymin=table_zero, ymax=table_limit-50000)+
    coord_flip() 
  
  TARGETplot_zoom <- ggplot(TARG, aes(x=target, y=people))+
    geom_bar(stat="identity", fill="grey") +
    geom_bar(data=TARG, aes(x=target, y=total_target),stat="identity", alpha=alpha_g) +
    labs(x="",y="")+
    theme_bw()+
    coord_flip(ylim=c(0, zoom))  
  
  IMPACTSplot_zoom <-ggplot(IMP, aes(x=impact , y=people))+
    geom_bar(stat="identity", fill="grey") +
    geom_bar(data=IMP, aes(x=impact, y=total_impact), stat="identity", alpha=alpha_g) +
    labs(x="",y="") +
    theme_bw()+
    coord_flip(ylim=c(0, zoom)) 
  
  summaryplot<-arrangeGrob(TARGETplot_overall, IMPACTSplot_overall, TARGETplot_zoom, IMPACTSplot_zoom, ncol=2, nrow=2, main=mainT, sub=subT)
  
  return(summaryplot)
}
