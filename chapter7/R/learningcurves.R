library(methods)
library(RSSL)
library(createdatasets)

setdatadir("~/Data")

measures <- list("Error"=measure_error,
                 "Average Loss Test"=measure_losstest)

library(createdatasets)
library(randomForest)

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

classifiers <- list(
  "Supervised" = function(X,y,X_u,y_u) {
    LeastSquaresClassifier(X,y)
  },
  "EM" = function(X,y,X_u,y_u) {
    EMLeastSquaresClassifier(X,y,X_u, eps = 1e-5)
  },
  "SL" = function(X,y,X_u,y_u) {
    SelfLearning(X,y,X_u,method=LeastSquaresClassifier)
  },
  "Oracle"=function(X,y,X_u,y_u) {LeastSquaresClassifier(rbind(X,X_u),unlist(list(y,y_u)),intercept=TRUE,x_center=TRUE,scale=FALSE) }
)

set.seed(42)
lc <- LearningCurveSSL(models,datasets,
                       classifiers=classifiers,
                       measures=measures,
                       n_l="enough",repeats=100,verbose=TRUE,
                       pre_scale = TRUE, pre_pca = TRUE,
                       low_level_cores = 4)

save(lc,file="learningcurves-enough.RData")