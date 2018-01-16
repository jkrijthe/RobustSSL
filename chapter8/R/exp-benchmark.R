library(RSSL)
library(methods)
library(createdatasets)
library(randomForest)
library(MASS)
library(magrittr)
library(dplyr)
library(ggplot2)

setdatadir("~/Data")
datasets<-list("Haberman"=createHaberman(),
               "Ionosphere"=createIonosphere(),
               "Parkinsons"=createParkinsons(),
               "Diabetes"=na.roughfix(createDiabetes()),
               "Sonar"=createSonar(),
               "SPECT"=createSPECT(),
               "SPECTF"=createSPECTF(),
               "Transfusion"=createTransfusion(),
               "WDBC"=createWDBC(),
               "Mammography"=na.roughfix(createMammographicMass()),
               "Digit1"=createDigit1(),
               "USPS"=createUSPS(),
               "COIL2"=createCOIL2(),
               "BCI"=createBCI(),
               "g241c"=createG241C(),
               "g241d"=createG241N())

models <- list("Haberman"=formula(Survival~.),
               "Ionosphere"=formula(Return~.),
               "Parkinsons"=formula(status~ . -subject -recording),
               "Diabetes"=formula(Diabetes~.),
               "Sonar"=formula(Label ~ .),
               "SPECT"=formula(Diagnosis ~ .),
               "SPECTF"=formula(Diagnosis ~ .),
               "Transfusion"=formula(Donated ~ .),
               "WDBC"=formula(Diagnosis ~ . -ID),
               "Mammography"=formula(Severity ~ . -BIRADS),
               "Digit1"=formula(Class ~ .),
               "USPS"=formula(Class ~ .),
               "COIL2"=formula(Class ~ .),
               "BCI"=formula(Class ~ .),
               "g241c"=formula(Class ~ .),
               "g241d"=formula(Class ~ .))

## Settings
classifiers<-list(
  "Base"=function(X,y,X_u,y_u) {LeastSquaresClassifier(X,y,intercept=TRUE,x_center=TRUE,scale=FALSE,y_scale=TRUE) },
  "Semi-supervised"=function(X,y,X_u,y_u) {USMLeastSquaresClassifier(X,y,X_u,intercept=FALSE,x_center=TRUE,y_scale=TRUE) }, 
  "Supervised"=function(X,y,X_u,y_u) {LeastSquaresClassifier(rbind(X,X_u),unlist(list(y,y_u)),intercept=TRUE,x_center=TRUE,scale=FALSE,y_scale=TRUE)}
)

set.seed(5618)
modelforms <- models[c("Ionosphere","SPECT","Sonar","SPECTF")]
datasets2 <- datasets[c("Ionosphere","SPECT","Sonar","SPECTF")]

measures <- list("Error"=measure_error,
                 "Average Loss Test"=measure_losstest)

lc <- LearningCurveSSL(modelforms,datasets2,
                       classifiers=classifiers,
                       measures=measures,
                       n_l="half",repeats=100,verbose=TRUE,
                       with_replacement=TRUE,
                       pre_scale = FALSE, pre_pca = TRUE, mc.cores=1, low_level_cores=3,sizes = c(1,seq(5,100,by=5)))

plot(lc)$data %>% 
  dplyr::filter(Measure=="Error") %>% 
  ggplot(aes(x=`Number of unlabeled objects`,y=`Mean`,color=Classifier)) +
  geom_line() +
  geom_ribbon(aes(ymin=Mean-SE,ymax=Mean+SE,fill=Classifier),alpha=0.3,color=NA) +
  facet_wrap(~Dataset,ncol=3,scales = "free") +
  ggtitle("10 labeled examples") +
  ggsave("benchmark.pdf")

set.seed(5618)
modelforms <- models[c("USPS","Digit1","COIL2","g241c","g241d")]
datasets2 <- datasets[c("USPS","Digit1","COIL2","g241c","g241d")]

measures <- list("Error"=measure_error,
                 "Average Loss Test"=measure_losstest)

save(lc,file="exp-benchmark.RData")

lc_big <- LearningCurveSSL(modelforms[-4],datasets2[-4],
                       classifiers=classifiers,
                       measures=measures,
                       n_l="half",repeats=100,verbose=TRUE,
                       with_replacement=TRUE,
                       pre_scale = FALSE, pre_pca = TRUE, mc.cores=3, low_level_cores=1,sizes = c(seq(10,250,by=10),500,1000))

save(lc,lc_big,file="exp-benchmark.RData")

plot(lc_big)$data %>% 
  dplyr::filter(Measure=="Error") %>% 
  ggplot(aes(x=`Number of unlabeled objects`,y=`Mean`,color=Classifier)) +
  geom_line() +
  geom_ribbon(aes(ymin=Mean-SE,ymax=Mean+SE,fill=Classifier),alpha=0.3,color=NA) +
  facet_wrap(~Dataset,ncol=3,scales = "free") +
  ggtitle("10 labeled examples") +
  ggsave("benchmark-big.pdf")
# 
# datasets<-list("p=5,delta=4.65"=generateRaudysGaussian(500,5,4.65),
#                "p=10,delta=4.65"=generateRaudysGaussian(500,10,4.65),
#                "p=50,delta=4.65"=generateRaudysGaussian(500,50,4.65),
#                "p=5,delta=2"=generateRaudysGaussian(500,5,2),
#                "p=10,delta=2"=generateRaudysGaussian(500,10,2),
#                "p=50,delta=2"=generateRaudysGaussian(500,50,2)
# )
# 
# models <- rep(list(formula(Class~.)),length(datasets))
# names(models) <- names(datasets)
# 
# lc2 <- LearningCurveSSL(models,datasets,
#                        classifiers=classifiers,
#                        measures=measures,
#                        n_l=4,repeats=30,verbose=TRUE,
#                        with_replacement=TRUE,
#                        pre_scale = FALSE, pre_pca = TRUE, mc.cores=1, low_level_cores=1,sizes = c(1,seq(5,100,by=5),500,1000))
# 
# library(magrittr)
# library(ggplot2)
# lc2$results$Dataset <- factor(lc2$results$Dataset,levels=names(datasets),labels=names(datasets))
# 
# plot(lc2)$data %>% 
#   dplyr::filter(Measure=="Error") %>% 
#   ggplot(aes(x=`Number of unlabeled objects`,y=`Mean`,color=Classifier)) +
#   geom_line() +
#   geom_ribbon(aes(ymin=Mean-SE,ymax=Mean+SE,fill=Classifier),alpha=0.3,color=NA) +
#   facet_wrap(~Dataset,ncol=3,scales = "free") +
#   ggtitle("4 labeled examples") +
#   ggsave("dimensionalities.pdf")
# 
# 
# 
# 
# 
# lc4 <- LearningCurveSSL(models[3],datasets[3],
#                         classifiers=classifiers,
#                         measures=measures,
#                         n_l=80,repeats=10,verbose=TRUE,
#                         with_replacement=TRUE,
#                         pre_scale = FALSE, pre_pca = TRUE, mc.cores=1, low_level_cores=1,sizes = c(seq(2,20,by=1)))
# 
# lc5 <- LearningCurveSSL(models[3],datasets[3],
#                         classifiers=classifiers,
#                         measures=measures,
#                         n_l=30,repeats=10,verbose=TRUE,
#                         with_replacement=TRUE,
#                         pre_scale = FALSE, pre_pca = FALSE, mc.cores=1, low_level_cores=1,sizes = c(seq(5,100,by=5)))
# pdf("exp-halfway.pdf",width=11)
# plot(lc5)
# dev.off()
