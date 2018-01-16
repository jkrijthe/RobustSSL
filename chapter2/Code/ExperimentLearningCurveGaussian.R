## Experiment Semi-Supervised versions of Linear Discriminant Classifiers
## Jesse Krijthe 2013

# Load Libraries
library(RSSL)

## Load data

datasets<-list("2DGaussian"=Generate2ClassGaussian(1000,2,0.8))
modelforms<-list("2DGaussian"=formula(y~.))
repeats<-1000
n_labeled<-"enough2"
description<-"LDA-nopca-3setsB"

## Settings
verbose <- TRUE
classifiers<-c(function(X,y,X_u,y_u) {LinearDiscriminantClassifier(X,y,scale=FALSE) },
               function(X,y,X_u,y_u) {LinearDiscriminantClassifier(rbind(X,X_u),c(y,y_u),scale=FALSE) },
               function(X,y,X_u,y_u) {MCLinearDiscriminantClassifier(X,y,X_u,scale=FALSE) },
               function(X,y,X_u,y_u) {EMLinearDiscriminantClassifier(X,y,X_u,scale=FALSE) },
               function(X,y,X_u,y_u) {SelfLearning(X,y,X_u,method=LinearDiscriminantClassifier,scale=FALSE) },
               function(X,y,X_u,y_u) {ICLinearDiscriminantClassifier(X,y,X_u,scale=FALSE,factr=1e11) }
)

## Calculate Learning Curves
errorcurves<-lapply(names(datasets),function(dname){
  cat(dname,"\n");
  classname<-all.vars(modelforms[[dname]])[1]
  current_data<-SSLDataFrameToMatrices(modelforms[[dname]],datasets[[dname]],intercept=FALSE)
  if (n_labeled=="enough") { n_l<-max(ncol(current_data$X)+10,20) }
  if (n_labeled=="enough2") { n_l<-max(ncol(current_data$X)*2,10) }
  else {n_l<-n_labeled}
  if (n_l<ncol(current_data$X)) {
    pca<-prcomp(current_data$X)
    current_data$X<-(predict(pca, current_data$X))[,1:(n_l-1)]
  }
  ErrorCurveSSL(current_data$X,current_data$y,classifiers,n_l=n_l,s=2^(0:10),repeats=repeats,verbose=verbose, with_replacement=FALSE,n_min=2)})

## Save Results
save.image("Results/LearningCurveGaussian.RData")
