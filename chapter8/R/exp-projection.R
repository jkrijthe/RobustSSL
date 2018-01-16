library(RSSL)
library(methods)
library(createdatasets)
library(randomForest)
library(MASS)
library(magrittr)
library(dplyr)
library(ggplot2)
library(gridExtra)
source("datasets.R")

classifiers<-list(
  "Supervised"=function(X,y,X_u,y_u) {LeastSquaresClassifier(X,y,intercept=TRUE,x_center=TRUE,scale=FALSE) },
  "USM"=function(X,y,X_u,y_u) {USMLeastSquaresClassifier(X,y,X_u,intercept=FALSE,x_center=TRUE,y_scale=TRUE) }, 
  #"USMSupCenter"=function(X,y,X_u,y_u) {USMLeastSquaresClassifier(X,y,X_u,intercept=FALSE,x_center=TRUE,y_scale=TRUE,use_Xu_for_scaling=FALSE) }, 
  #"SupervisedNoCenter"=function(X,y,X_u,y_u) {LeastSquaresClassifier(X,y,intercept=TRUE,x_center=FALSE,scale=FALSE) },
  #"ProjectionNoCenter"=function(X,y,X_u,y_u) {ICLeastSquaresClassifier(X,y,X_u,intercept=TRUE,x_center=FALSE,y_scale=FALSE,projection = "semisupervised") }, 
  "Projection"=function(X,y,X_u,y_u) {ICLeastSquaresClassifier(X,y,X_u,intercept=TRUE,x_center=TRUE,y_scale=FALSE,projection = "semisupervised",use_Xu_for_scaling=FALSE) }, "ICLS"=function(X,y,X_u,y_u) {ICLeastSquaresClassifier(X,y,X_u,intercept=TRUE,x_center=TRUE,y_scale=FALSE,projection = "supervised",use_Xu_for_scaling=FALSE) },
  #"Projection"=function(X,y,X_u,y_u) {ICLeastSquaresClassifier(X,y,X_u,intercept=TRUE,x_center=TRUE,y_scale=FALSE,projection = "semisupervised") }, 
  #"Projection_yscale"=function(X,y,X_u,y_u) {ICLeastSquaresClassifier(X,y,X_u,intercept=TRUE,x_center=TRUE,y_scale=TRUE,projection = "semisupervised") }, 
  "Oracle"=function(X,y,X_u,y_u) {LeastSquaresClassifier(rbind(X,X_u),unlist(list(y,y_u)),intercept=TRUE,x_center=TRUE,scale=FALSE)}
)

datasets<-list("p=5,delta=4.65"=generateRaudysGaussian(500,50,0.2)
)


# measure_vector <- function (trained_classifier, X_l = NULL, y_l = NULL, X_u = NULL, 
#           y_u = NULL, X_test = NULL, y_test = NULL) 
# {
#   trained_classifier@theta
# }

models <- rep(list(formula(Class~.)),length(datasets))
names(models) <- names(datasets)

measures <- list("Error"=measure_error,
                 "Average Loss Test"=measure_losstest,
                 "Average Loss Train"=measure_losstrain)

lc_trans <- LearningCurveSSL(models,datasets,
                             classifiers=classifiers,
                             measures=measures,
                             n_l=4,repeats=30,verbose=TRUE,
                             with_replacement=TRUE,
                             pre_scale = FALSE, pre_pca = TRUE, mc.cores=1, low_level_cores=1,sizes = c(1,seq(5,100,by=5)))

save(lc_trans,file="exp-projections.RData")

load("exp-projections.RData")

p1 <- lc_trans$results %>% 
  dplyr::filter(Measure=="Average Loss Test") %>% 
  group_by(Dataset, Measure, `Number of unlabeled objects`,Classifier) %>% 
  summarize(Mean_log=mean(log(value))) %>% 
  ungroup %>% 
  dplyr::filter(Classifier %in% c("Supervised","USM","Oracle","Projection","ICLS")) %>% 
  ggplot(aes(x=`Number of unlabeled objects`,y=Mean_log, color=Classifier)) +
  geom_line() +
  geom_point() +
  ggtitle("On Test Set")

p2 <- lc_trans$results %>% 
  dplyr::filter(Measure=="Average Loss Train") %>% 
  group_by(Dataset, Measure, `Number of unlabeled objects`,Classifier) %>% 
  summarize(Mean=mean((value))) %>% 
  ungroup %>% 
  dplyr::filter(Classifier %in% c("Supervised","USM","Oracle","Projection","ICLS")) %>%  
  ggplot(aes(x=`Number of unlabeled objects`,y=Mean, color=Classifier)) +
  geom_line() +
  geom_point() +
  ggtitle("On Train Set")

p3 <- lc_trans$results %>% 
  dplyr::filter(Measure=="Error") %>% 
  group_by(Dataset, Measure, `Number of unlabeled objects`,Classifier) %>% 
  summarize(Error=mean((value))) %>% 
  ungroup %>% 
  dplyr::filter(Classifier %in% c("Supervised","USM","Oracle","Projection","ICLS")) %>%  
  ggplot(aes(x=`Number of unlabeled objects`,y=Error, color=Classifier)) +
  geom_line() +
  geom_point() +
  ggtitle("On test set")


pdf("exp-projection.pdf",width = 14,height=7)
grid.arrange(p1,p2,p3,ncol=3)
dev.off()