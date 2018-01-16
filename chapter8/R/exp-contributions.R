# Generate curve that indicates contribution of extra object in increasing error due to covariance and mean estimation separately

#' library(MASS, warn.conflicts = FALSE)
#' library(dplyr, warn.conflicts = FALSE)
#' library(purrr, warn.conflicts = FALSE)
#' library(tidyr, warn.conflicts = FALSE)
#' 
#' source("classifiers.R")
#' 
#' #' @param N number of objects per class
#' #' @param d number of dimensions
#' #' @delta delta Mahanalobis distance between classes
#' generateRaudysGaussian <- function (N, p, delta) 
#' {
#'   X <- rbind(mvrnorm(N, rep(-0.5*delta/sqrt(p), p), diag(p)), 
#'              mvrnorm(N, rep(0.5*delta/sqrt(p), p), diag(p)))
#'   y <- rbind(matrix(-1, N, 1), matrix(1, N, 1))
#'   
#'   return(data.frame(X, Class = factor(y)))
#' }
#' 
#' generate_lcset <- function(label_set_sizes,delta=4.65) {
#'   data_frame(
#'     train = label_set_sizes %>% 
#'       map(generateRaudysGaussian,p=50,delta=delta),
#'     test = replicate(length(label_set_sizes),generateRaudysGaussian(500,p=50,delta=delta),FALSE),
#'     N=label_set_sizes
#'   )
#' }
#' 
#' generate_all <- function(repeats,n_samples,label_set_sizes) {
#'   replicate(repeats,generate_lcset(label_set_sizes),FALSE) %>%
#'     bind_rows(.id="Repeat")
#' }
#' 
#' all_class <- function(train,test) {
#'   truth <- test$Class
#'   out <- data_frame(name=c("Covariance","Means","Nothing","Both"), 
#'                     models = list(
#'                       train %>% SemiFisherLDAClassifier(Class~., .,n_labeled_per_class=(nrow(train)-2)/2),
#'                       train %>% SemiFisherLDAClassifier(Class~., .,n_labeled_per_class=(nrow(train)-2)/2,update="means"),
#'                       train %>% SemiFisherLDAClassifier(Class~., .,n_labeled_per_class=(nrow(train)-2)/2,update="nothing"),
#'                       train %>% FisherLDAClassifier(Class~., .,type = "total")
#'                     )) %>% mutate(
#'                       preds = map(models, predict,test),
#'                       error = map_dbl(preds, function(x){ 1-mean(x==truth)})
#'                     )
#'   out
#' }
#' 
#' 
#' 
#' out <- generate_all(repeats=100,n_samples=100,label_set_sizes=seq(2,60,1)) %>% 
#'   mutate(classifiers = map2(train, test, all_class)) %>% 
#'   select(-train,-test) %>% 
#'   unnest() %>% 
#'   select(-preds,-models)
#' 
#' out2 <- out %>% filter(name!="QR")  %>% group_by(N,name) %>% summarize(error=mean(error)) %>% ungroup
#' 
#' out2 %>% 
#'   ggplot(aes(x=N,y=error, color=name)) +
#'   geom_line() +
#'   ggtitle("2 Gaussian, delta=4.65, p=50") +
#'   xlab("Number of objects per class") +
#'   ylab("Error")
#' 
#' out2 %>% 
#'   group_by(N) %>% 
#'   mutate(error=error-error[name=="Nothing"]) %>% 
#'   ungroup %>% 
#'   ggplot(aes(x=N,y=error, color=name)) +
#'   geom_line() +
#'   ggtitle("2 Gaussian, delta=4.65, p=50") +
#'   xlab("Number of objects per class") +
#'   ylab("Error")
#' 
#' save(out,out2,file="contributions.RData")


# classifiers <- list("Covariance"=,
#                     "Means"=,
#                     "Nothing"=,
#                     "Both"=)
#   
# ))
# 
# datasets <- generate_datasets()
library(RSSL)
library(methods)
library(createdatasets)
library(randomForest)
library(MASS)
library(magrittr)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(Matrix)

source("datasets.R")
source("classifiers.R")
# 
# classifiers<-list(
#   "Supervised" = function(X,y,X_u,y_u) { LeastSquaresClassifier(X,y,intercept=TRUE,x_center=TRUE,scale=FALSE) },
#   "USM" = function(X,y,X_u,y_u) { 
#     X_e <- rbind(X,X_u[-c(nrow(X_u)-1,nrow(X_u)),,drop=FALSE]); 
#     y_e <- unlist(list(y,y_u[-c(length(y_u)-1,length(y_u))]))
#     X_u <- X_u[nrow(X_u)-1,,drop=FALSE];
#     y_u <- y_u[length(y_u)-1];
#     USMLeastSquaresClassifier(X_e,y_e,X_u,intercept=FALSE,x_center=TRUE,y_scale=TRUE,use_Xu_for_scaling = FALSE) 
#   }, 
#   "USM_semiscaling" = function(X,y,X_u,y_u) { 
#     X_e <- rbind(X,X_u[-c(nrow(X_u)-1,nrow(X_u)),,drop=FALSE]); 
#     y_e <- unlist(list(y,y_u[-c(length(y_u)-1,length(y_u))]))
#     X_u <- X_u[nrow(X_u)-1,,drop=FALSE];
#     y_u <- y_u[length(y_u)-1];
#     USMLeastSquaresClassifier(X_e,y_e,X_u,intercept=FALSE,x_center=TRUE,y_scale=TRUE,use_Xu_for_scaling = TRUE) 
#   }, 
#   "UpdatedMean_sup" = function(X,y,X_u,y_u) { 
#     X_e <- rbind(X,X_u[-c(nrow(X_u)-1,nrow(X_u)),,drop=FALSE]); 
#     y_e <- unlist(list(y,y_u[-c(length(y_u)-1,length(y_u))]))
#     X_u <- X_u[nrow(X_u)-1,,drop=FALSE];
#     y_u <- y_u[length(y_u)-1];
#     UpdateMeanLeastSquaresClassifier(X_e,y_e,X_u,y_u,intercept=FALSE,x_center=TRUE,y_scale=TRUE, use_Xu_for_scaling = FALSE) 
#     
#   },
#   "UpdatedMean_semi" = function(X,y,X_u,y_u) { 
#     X_e <- rbind(X,X_u[-c(nrow(X_u)-1,nrow(X_u)),,drop=FALSE]); 
#     y_e <- unlist(list(y,y_u[-c(length(y_u)-1,length(y_u))]))
#     X_u <- X_u[nrow(X_u)-1,,drop=FALSE];
#     y_u <- y_u[length(y_u)-1];
#     UpdateMeanLeastSquaresClassifier(X_e,y_e,X_u,y_u,intercept=FALSE,x_center=TRUE,y_scale=TRUE, use_Xu_for_scaling = TRUE) 
#     
#   },
#   "UpdatedOnlyMean" = function(X,y,X_u,y_u) { 
#     
#     UpdateMeanLeastSquaresClassifier(X,y,X_u,y_u,intercept=FALSE,x_center=TRUE,y_scale=TRUE,use_Xu_for_scaling = TRUE) 
#     
#   },
#   "UpdatedOnlyMeanSupCenter" = function(X,y,X_u,y_u) { 
#     
#     UpdateMeanLeastSquaresClassifier(X,y,X_u,y_u,intercept=FALSE,x_center=TRUE,y_scale=TRUE,use_Xu_for_scaling = FALSE) 
#     
#   },
#   "Both" = function(X,y,X_u,y_u) { 
#     
#     Xe <- rbind(X,X_u[-c(nrow(X_u)-1,nrow(X_u)),,drop=FALSE]); 
#     ye <- unlist(list(y,y_u[-c(nrow(X_u)-1,nrow(X_u))]));
#     
#     
#     Xu <- X_u[nrow(X_u)-1,,drop=FALSE];
#     Xm <- X_u[nrow(X_u),,drop=FALSE];
#     ym <- y_u[length(y_u)];
#     
#     UpdateBothLeastSquaresClassifier(Xe,ye,Xu,Xm,ym,intercept=FALSE,x_center=TRUE,y_scale=TRUE, use_Xu_for_scaling = TRUE) 
#     
#   },
#   "Default" = function(X,y,X_u,y_u) { 
#     X_e <- rbind(X,X_u[-c(nrow(X_u)-1,nrow(X_u)),,drop=FALSE]); 
#     y_e <- unlist(list(y,y_u[-c(length(y_u)-1,length(y_u))]))
#     LeastSquaresClassifier(X_e, y_e, intercept=TRUE, x_center=TRUE, scale=FALSE)
#   },
#   "Oracle" = function(X,y,X_u,y_u) { LeastSquaresClassifier(rbind(X,X_u),unlist(list(y,y_u)),intercept=TRUE,x_center=TRUE,scale=FALSE)}
# )
# 
datasets<-list("p=5,delta=4.65"=generateRaudysGaussian(500,5,4.65),
               "p=10,delta=4.65"=generateRaudysGaussian(500,10,4.65),
               "p=50,delta=4.65"=generateRaudysGaussian(500,50,4.65),
               "p=5,delta=2"=generateRaudysGaussian(500,5,2),
               "p=10,delta=2"=generateRaudysGaussian(500,10,2),
               "p=50,delta=2"=generateRaudysGaussian(500,50,2)
)

models <- rep(list(formula(Class~.)),length(datasets))
names(models) <- names(datasets)
# 
# lc3 <- LearningCurveSSL(models[3],datasets[3],
#                         classifiers=classifiers,
#                         measures=measures,
#                         n_l=10,repeats=10,verbose=TRUE,
#                         with_replacement=TRUE,
#                         pre_scale = FALSE, pre_pca = FALSE, 
#                         mc.cores=1, low_level_cores=1,
#                         sizes = c(seq(10,100,by=5)))
# 

# pdf("contributions.pdf",height = 10)
# grid.arrange(p1,p2,nrow=2,ncol=1)
# dev.off()

stepsize <- 1

classifiers<-list(
  "Supervised" = function(X,y,X_u,y_u) { LeastSquaresClassifier(X,y,intercept=TRUE,x_center=TRUE,scale=FALSE) },
  "UpdatedCovariance" = function(X,y,X_u,y_u) { 
    X_e <- rbind(X,X_u[-c((nrow(X_u)-(stepsize-1)):nrow(X_u)),,drop=FALSE]); 
    y_e <- unlist(list(y,y_u[-c((length(y_u)-(stepsize-1)):length(y_u))]))
    X_u <- X_u[(nrow(X_u)-(stepsize-1)):nrow(X_u),,drop=FALSE];
    y_u <- y_u[(length(y_u)-(stepsize-1)):length(y_u)];
    USMLeastSquaresClassifier(X_e,y_e,X_u,intercept=TRUE,x_center=TRUE,y_scale=TRUE,use_Xu_for_scaling = FALSE) 
  }, 
  "FixedRank" = function(X,y,X_u,y_u) { 
    X_e <- rbind(X,X_u[-c((nrow(X_u)-(stepsize-1)):nrow(X_u)),,drop=FALSE]); 
    y_e <- unlist(list(y,y_u[-c((length(y_u)-(stepsize-1)):length(y_u))]))
    X_u <- X_u[(nrow(X_u)-(stepsize-1)):nrow(X_u),,drop=FALSE];
    y_u <- y_u[(length(y_u)-(stepsize-1)):length(y_u)];
    UpdateRankLeastSquaresClassifier(X_e,y_e,X_u,y_u,intercept=TRUE,x_center=TRUE,y_scale=TRUE,use_Xu_for_scaling = TRUE) 
  }, 
  "UpdateRank" = function(X,y,X_u,y_u) { 
    X_e <- rbind(X,X_u[-c((nrow(X_u)-(2*stepsize-1)):(nrow(X_u))),,drop=FALSE]); 
    y_e <- unlist(list(y,y_u[-c((length(y_u)-(2*stepsize-1)):(length(y_u)))]))
    X_u <- X_u[(nrow(X_u)-(2*stepsize-1)):(nrow(X_u)-stepsize),,drop=FALSE];
    y_u <- y_u[(length(y_u)-(2*stepsize-1)):(length(y_u)-stepsize)];
    
    UpdateRankLeastSquaresClassifier(X_e,y_e,X_u,y_u,intercept=TRUE,x_center=TRUE,y_scale=TRUE,use_Xu_for_scaling = TRUE) 
  }, 
  "FixedRankCovariance" = function(X,y,X_u,y_u) { 
    X_e <- rbind(X,X_u[-c((nrow(X_u)-(stepsize-1)):nrow(X_u)),,drop=FALSE]); 
    y_e <- unlist(list(y,y_u[-c((length(y_u)-(stepsize-1)):length(y_u))]))
    X_u <- X_u[(nrow(X_u)-(stepsize-1)):nrow(X_u),,drop=FALSE];
    y_u <- y_u[(length(y_u)-(stepsize-1)):length(y_u)];
    UpdateRankLeastSquaresClassifier(X_e,y_e,X_u,y_u,intercept=TRUE,x_center=TRUE,y_scale=TRUE,use_Xu_for_scaling = FALSE,type="covariance") 
  }, 
  "UpdateRankCovariance" = function(X,y,X_u,y_u) { 
    X_e <- rbind(X,X_u[-c((nrow(X_u)-(2*stepsize-1)):(nrow(X_u))),,drop=FALSE]); 
    y_e <- unlist(list(y,y_u[-c((length(y_u)-(2*stepsize-1)):(length(y_u)))]))
    X_u <- X_u[(nrow(X_u)-(2*stepsize-1)):(nrow(X_u)-stepsize),,drop=FALSE];
    y_u <- y_u[(length(y_u)-(2*stepsize-1)):(length(y_u)-stepsize)];
    
    UpdateRankLeastSquaresClassifier(X_e,y_e,X_u,y_u,intercept=TRUE,x_center=TRUE,y_scale=TRUE,use_Xu_for_scaling = FALSE,type="covariance") 
  }, 
  # "UpdatedMean" = function(X,y,X_u,y_u) { 
  #   X_e <- rbind(X,X_u[-c((nrow(X_u)-(stepsize-1)):nrow(X_u)),,drop=FALSE]); 
  #   y_e <- unlist(list(y,y_u[-c((length(y_u)-(stepsize-1)):length(y_u))]))
  #   X_u <- X_u[(nrow(X_u)-(stepsize-1)):nrow(X_u),,drop=FALSE];
  #   y_u <- y_u[(length(y_u)-(stepsize-1)):length(y_u)];
  #   UpdateMeanLeastSquaresClassifier(X_e,y_e,X_u,y_u,intercept=FALSE,x_center=TRUE,y_scale=TRUE, use_Xu_for_scaling = FALSE) 
  #   
  # },
  "UpdatedMean" = function(X,y,X_u,y_u) { 
    X_e <- rbind(X,X_u[-c((nrow(X_u)-(2*stepsize-1)):(nrow(X_u))),,drop=FALSE]); 
    y_e <- unlist(list(y,y_u[-c((length(y_u)-(2*stepsize-1)):(length(y_u)))]))
    X_u <- X_u[(nrow(X_u)-(2*stepsize-1)):(nrow(X_u)-stepsize),,drop=FALSE];

    y_u <- y_u[(length(y_u)-(2*stepsize-1)):(length(y_u)-stepsize)];
    USMLeastSquaresClassifier(X_e,y_e,X_u,intercept=TRUE,x_center=TRUE,y_scale=TRUE,use_Xu_for_scaling = TRUE) 
  },
  # "UpdatedOnlyMean" = function(X,y,X_u,y_u) { 
  #   
  #   UpdateMeanLeastSquaresClassifier(X,y,X_u,y_u,intercept=TRUE,x_center=TRUE,y_scale=TRUE,use_Xu_for_scaling = TRUE) 
  #   
  # },
  "Default" = function(X,y,X_u,y_u) { 
    X_e <- rbind(X,X_u[-c((nrow(X_u)-(stepsize-1)):nrow(X_u)),,drop=FALSE]); 
    y_e <- unlist(list(y,y_u[-c((length(y_u)-(stepsize-1)):length(y_u))]))
    LeastSquaresClassifier(X_e, y_e, intercept=TRUE, x_center=TRUE, scale=FALSE,y_scale=TRUE)
  },
  "Oracle" = function(X,y,X_u,y_u) { LeastSquaresClassifier(rbind(X,X_u),unlist(list(y,y_u)),intercept=TRUE,x_center=TRUE,scale=FALSE,y_scale=TRUE)}
)

measures <- list("Error"=measure_error,
                 "Average Loss Test"=measure_losstest,
                 "Average Loss Train"=measure_losstrain)

lc3 <- LearningCurveSSL(models[3],datasets[3],
                        classifiers=classifiers,
                        measures=measures,
                        n_l=5,repeats=6,verbose=TRUE,
                        with_replacement=TRUE,
                        pre_scale = FALSE, pre_pca = FALSE,
                        mc.cores=1, low_level_cores=1,
                        sizes = c(seq(stepsize*3,100,by=stepsize)))


save(lc3,stepsize,file="exp-contributions2.RData")


load("exp-contributions2.RData")

p_error <- lc3$results %>%
  group_by(Dataset, Measure, `Number of unlabeled objects`,Classifier) %>%
  summarize(Mean=mean(value)) %>%
  ungroup %>%
  mutate(`Number of unlabeled objects`=`Number of unlabeled objects`) %>% 
  filter(Measure=="Error", Classifier%in% c("Oracle","Supervised","UpdatedOnlyMean")) %>%
  #filter(Measure=="Error", Classifier!="UpdatedMean_semi") %>%
  ggplot(aes(x=`Number of unlabeled objects`,y=Mean, color=Classifier)) +
  geom_line() +
  geom_point() +
  ylab("Error")

df_plot <- lc3$results %>%
  group_by(repeats, Dataset, Measure, `Number of unlabeled objects`) %>%
  mutate(value=value-value[Classifier=="Default"]) %>%
  ungroup %>%
  mutate(value=ifelse(Classifier %in% c("UpdatedMean","UpdateRank","UpdateRankCovariance"),-value,value))
  
df_sum <- df_plot %>% 
  group_by(repeats, Dataset, Measure, `Number of unlabeled objects`) %>%
  mutate(value=value[Classifier=="UpdatedCovariance"]+value[Classifier=="UpdatedMean"]) %>%
  ungroup %>%
  filter(Classifier=="UpdatedCovariance") %>% 
  mutate(Classifier="Effect Cov+Mean")

df_sum2 <- df_plot %>% 
  group_by(repeats, Dataset, Measure, `Number of unlabeled objects`) %>%
  mutate(value=value[Classifier=="UpdateRank"]+value[Classifier=="FixedRank"]) %>%
  ungroup %>%
  filter(Classifier=="UpdateRank") %>% 
  mutate(Classifier="Effect Rank+Data")

df_sum3 <- df_plot %>% 
  group_by(repeats, Dataset, Measure, `Number of unlabeled objects`) %>%
  mutate(value=value[Classifier=="UpdateRankCovariance"]+value[Classifier=="FixedRankCovariance"]) %>%
  ungroup %>%
  filter(Classifier=="UpdateRank") %>% 
  mutate(Classifier="Effect Rank+Data Covariance")

rbind(df_plot,df_sum,df_sum2,df_sum3) %>% 
  group_by(Dataset, Measure, `Number of unlabeled objects`,Classifier) %>%
  summarize(Mean=mean(value)) %>%
  ungroup %>%
  mutate(`Number of unlabeled objects`=`Number of unlabeled objects`) %>% 
  filter(Measure=="Error") %>%
  filter((Classifier %in% c("Oracle","UpdatedCovariance","UpdatedMean","Effect Cov+Mean"))) %>% 
  ggplot(aes(x=`Number of unlabeled objects`,y=Mean, color=Classifier)) +
  geom_line() +
  geom_point() +
  ylab("Difference") +
  geom_hline(yintercept=0) +
  xlab("Number of labeled objects") +
  ggtitle(paste("Increase in terms of error rate with ", stepsize, "additional objects are added")) +
  scale_x_continuous(breaks=seq(0,100,by=10), minor_breaks = seq(0,100,by=5))

rbind(df_plot,df_sum,df_sum2,df_sum3) %>% 
  group_by(Dataset, Measure, `Number of unlabeled objects`,Classifier) %>%
  summarize(Mean=mean(value)) %>%
  ungroup %>%
  mutate(`Number of unlabeled objects`=`Number of unlabeled objects`) %>% 
  filter(Measure=="Error") %>%
  filter((Classifier %in% c("UpdateRankCovariance","FixedRankCovariance","Effect Rank+Data Covariance","UpdatedCovariance"))) %>% 
  ggplot(aes(x=`Number of unlabeled objects`,y=Mean, color=Classifier)) +
  geom_line() +
  geom_point() +
  ylab("Difference") +
  geom_hline(yintercept=0) +
  xlab("Number of labeled objects") +
  ggtitle(paste("Increase in terms of error rate when ", stepsize, "objects are added")) +
  scale_x_continuous(breaks=seq(0,100,by=10), minor_breaks = seq(0,100,by=5))

rbind(df_plot,df_sum,df_sum2,df_sum3) %>% 
  group_by(Dataset, Measure, `Number of unlabeled objects`,Classifier) %>%
  summarize(Mean=mean(value)) %>%
  ungroup %>%
  mutate(`Number of unlabeled objects`=`Number of unlabeled objects`) %>% 
  filter(Measure=="Error") %>%
  filter((Classifier %in% c("UpdateRank","FixedRank","Effect Rank+Data","Oracle"))) %>% 
  ggplot(aes(x=`Number of unlabeled objects`,y=Mean, color=Classifier)) +
  geom_line() +
  geom_point() +
  ylab("Difference") +
  geom_hline(yintercept=0) +
  xlab("Number of labeled objects") +
  ggtitle(paste("Increase in terms of error rate when ", stepsize, "objects are added")) +
  scale_x_continuous(breaks=seq(0,100,by=10), minor_breaks = seq(0,100,by=5))


p_loss <- lc3$results %>%
  group_by(repeats, Dataset, Measure, `Number of unlabeled objects`) %>%
  mutate(value=value-value[Classifier=="Default"]) %>%
  ungroup %>%
  group_by(Dataset, Measure, `Number of unlabeled objects`,Classifier) %>%
  #summarize(Mean=(sign(mean(value)))*sqrt(abs(mean(value)))) %>%
  summarize(Mean=mean(value)) %>%
  ungroup %>%
  mutate(`Number of unlabeled objects`=`Number of unlabeled objects`) %>% 
  filter(Measure=="Average Loss Test") %>%
  filter((Classifier %in% c("Oracle","USM_semiscaling","UpdatedCovariance","UpdatedRank","UpdatedRankCovariance","UpdatedMean"))) %>%
  filter(`Number of unlabeled objects`>60) %>% 
  ggplot(aes(x=`Number of unlabeled objects`,y=Mean, color=Classifier)) +
  geom_line() +
  geom_point() +
  ylab("Difference") +
  geom_hline(yintercept=0) +
  xlab("Number of labeled objects") +
  ggtitle("Improvement in terms of loss on test when 5 objects are added") +
  scale_x_continuous(breaks=seq(0,100,by=10), minor_breaks = seq(0,100,by=5))

pdf("exp-contributions.pdf",height=11)
grid.arrange(p_error,p_diff,p_loss,ncol=1)
dev.off()