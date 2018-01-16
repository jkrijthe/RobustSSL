library(RSSL)
library(reshape)
library(ggplot2)
require(gridExtra)

load("Results/Learningcurves3sets.RData")

# Newer version:
# class(cvb[[1]])<-"ErrorCurve"
# class(cvb[[2]])<-"ErrorCurve"
# class(cvb[[3]])<-"ErrorCurve"
errorcurves[[1]]$results<-(errorcurves[[1]]$results)[,1:10,,]
errorcurves[[2]]$results<-(errorcurves[[2]]$results)[,1:10,,]
errorcurves[[3]]$results<-(errorcurves[[3]]$results)[,1:10,,]
# 
# plot.ErrorCurve2(cvb,classifier_names=c("LDA","LDAoracle","MCLDA","SLLDA","ICLDA"),dataset_names=names(datasets))

plots<-list()
for (n in 1:length(errorcurves)) { 
  thesecurves<-errorcurves[[n]]
  # dimnames(thesecurves$results)[[3]]<-c("SL","LS","LSoracle","UCLS","ICLS")
  if (1) { thesecurves$results<-thesecurves$results[,,,] }
  dimnames(thesecurves$results)[[3]]<-c("LDA","LDAoracle","MCLDA","SLLDA","ICLDA")
  class(thesecurves)<-"ErrorCurve"
  plots<-c(plots,list(plot.ErrorCurve(thesecurves,measurement=1,dataset_names=names(datasets)[[n]],legendsetting="none")))
  # plots<-c(plots,list(plot.ErrorCurve(errorcurves[[n]],measurement=2,datasetname=names(datasets)[[n]],legendsetting="none")))
}
plots1<-plots

# for (k in 1:ceiling(length(plots)/5)) {
#   pl<-createlegend.ErrorCurve(thesecurves,measurement=1,datasetname=names(datasets)[[n]],legendsetting="right")
#   pdf(paste("~/Dropbox/Results/LearningCurves2-",n_labeled,"-",repeats,"repeats-",k,"-",description,".pdf",sep=""),paper="a4")
#   gridplot<-do.call(grid.arrange, c(plots[(1+5*(k-1)):min(5*k,length(plots))],list(pl),ncol=2))
#   dev.off()
# }

plots<-list()
for (n in 1:length(errorcurves)) { 
  thesecurves<-errorcurves[[n]]
  # dimnames(thesecurves$results)[[3]]<-c("SL","LS","LSoracle","UCLS","ICLS")
  if (1) { thesecurves$results<-thesecurves$results[,,,] }
  dimnames(thesecurves$results)[[3]]<-c("LDA","LDAoracle","MCLDA","SLLDA","ICLDA")
  class(thesecurves)<-"ErrorCurve"
  plots<-c(plots,list(plot.ErrorCurve(thesecurves,measurement=2,dataset_names=names(datasets)[[n]],legendsetting="none")))
  # plots<-c(plots,list(plot.ErrorCurve(errorcurves[[n]],measurement=2,datasetname=names(datasets)[[n]],legendsetting="none")))
}
plots2<-plots

# for (k in 1:ceiling(length(plots)/5)) {
#   pl<-createlegend.ErrorCurve(errorcurves[[1]],measurement=2,datasetname=names(datasets)[[n]],legendsetting="right")
#   pdf(paste("~/Dropbox/Results/LearningCurves-Loss-",n_labeled,"-",repeats,"repeats-",k,"-",description,".pdf",sep=""),paper="a4")
#   gridplot<-do.call(grid.arrange, c(plots[(1+5*(k-1)):min(5*k,length(plots))],list(pl),ncol=2))
#   dev.off()
# }


pdf(paste("Figures/errorcurves.pdf",sep=""),width=14,height=7)

# Add legend to last figure
tc<-errorcurves[[3]]
class(tc)<-"ErrorCurve"
dimnames(tc$results)[[3]]<-c("LDA","LDAoracle","MCLDA","SLLDA","ICLDA")
plots2[3]<-list(plot.ErrorCurve(tc,measurement=2,dataset_names=names(datasets)[[3]],legendsetting="right")+theme(legend.text = element_text(size = 8),legend.justification = c(1, 0), legend.position = c(1.0, 0.2)))
gridplot<-do.call(grid.arrange, c(plots1,plots2,ncol=3))
dev.off()
