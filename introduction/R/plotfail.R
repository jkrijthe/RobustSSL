library(RSSL)
library(dplyr,warn.conflicts = FALSE)
library(ggplot2)
library(ggthemes)
library(magrittr)
library(knitr)
library(scales)
library(extrafont,quietly = TRUE)
opts_chunk$set(echo=FALSE, warning=FALSE, dev="pdf")

library(ggthemes)
library(scales)
theme_classy <-  function(base_size=12) { 
  theme_foundation(base_size=base_size) +
    theme(rect=element_rect(fill=NA,colour=NA),
          legend.position="bottom",
          text=element_text(family="Palatino",size = base_size,colour="black"),
          line=element_line(size=0,colour="black"),
          axis.title.y=element_text(angle=0),
          axis.line=element_line(size=0.5,colour = "black",linetype=1),
          axis.ticks=element_line(size=0.5,color="black"),
          panel.grid=element_line(size=0.1,colour="grey",linetype = 2),
          panel.grid.minor=element_line(size=0.0,colour=NA,linetype = 2),
          strip.text=element_text(size=base_size*1.5),
          legend.text=element_text(size=base_size))
}

datasets <- list("2 Gaussian Expected"=generate2ClassGaussian(n=2000,d=2,expected=TRUE),
                 "2 Gaussian Non-Expected"=generate2ClassGaussian(n=2000,d=2,expected=FALSE))
formulae <- list("2 Gaussian Expected"=formula(Class~.),
                 "2 Gaussian Non-Expected"=formula(Class~.))

classifiers <- list("Supervised" = 
                      function(X,y,X_u,y_u) { LeastSquaresClassifier(X,y)},
                    "Self-learning" = 
                      function(X,y,X_u,y_u) { SelfLearning(X,y,X_u,method = LeastSquaresClassifier)})

measures <- list("Error" =  measure_error,
                 "Loss" = measure_losstest,
                 "Loss labeled" = measure_losslab,
                 "Loss Lab+Unlab" = measure_losstrain
)



res_lc <- LearningCurveSSL(formulae,datasets,classifiers=classifiers,measures=measures,verbose=TRUE,repeats=100,n_l=10,sizes = 2^(1:10))

save(res_lc,file="introduction/R/plotfail.RData")
