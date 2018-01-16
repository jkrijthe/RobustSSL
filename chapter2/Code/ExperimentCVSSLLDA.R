# ExperimentBenchmarkLeastSquaresSSL
library(RSSL)

## Load data
load("Data/Datasets.RData")
datasets<-datasets[c(1:9,14)]
repeats<-20
n_labeled<-"enough2"

## Settings
verbose <- TRUE
classifiers<-c(
  function(X,y,X_u,y_u) {LinearDiscriminantClassifier(X,y,scale=FALSE) },
  function(X,y,X_u,y_u) {LinearDiscriminantClassifier(rbind(X,X_u),c(y,y_u),scale=FALSE) },
  function(X,y,X_u,y_u) {MCLinearDiscriminantClassifier(X,y,X_u,scale=FALSE) },
  function(X,y,X_u,y_u) {EMLinearDiscriminantClassifier(X,y,X_u,scale=FALSE) },
  function(X,y,X_u,y_u) {SelfLearning(X,y,X_u,method=LinearDiscriminantClassifier,scale=FALSE) },
  function(X,y,X_u,y_u) {ICLinearDiscriminantClassifier(X,y,X_u,scale=FALSE,factr=1e11) }

)



## SSL Type 2
cat("SSL Type 2\n")
cvresults<-lapply(names(datasets),function(dname){
							classname<-all.vars(modelforms[[dname]])[1]
							current_data<-SSLDataFrameToMatrices(modelforms[[dname]],datasets[[dname]],intercept=FALSE)
              
							if (n_labeled=="enough") { n_l<-max(ncol(current_data$X)+10,20) }
							if (n_labeled=="enough2") { n_l<-max(ncol(current_data$X)*2,10) }
							else {n_l<-n_labeled}
              print(mean(bootstrap::crossval(current_data$X,current_data$y,LeastSquaresClassifier,predict,ngroup=10)$cv.fit==current_data$y))
							CrossValidationSSL2(current_data$X,current_data$y,classifiers,n_labeled=n_l,k=10,repeats=repeats,dataset_name=dname,verbose=T,n_min=2)}
                                   )



## Transductive 
# cvresults<-lapply(names(datasets),function(dname){
#                                    cat(dname,"\n");
#                                    classname<-all.vars(modelforms[[dname]])[1]
#                                    current_data<-SSLDataFrameToMatrices(modelforms[[dname]],datasets[[dname]],intercept=FALSE)
#                                    n_pos<-max(ncol(current_data$X)+5,20)
#                                    CrossValidationTransductive(current_data$X,current_data$y,classifiers,n_labeled=n_pos,k=min(floor(nrow(current_data$X)/n_pos),10),repeats=10,verbose=T)})

## Save results
save.image(paste("BenchmarkLDA-",repeats,"rep-justenoughplus5or20.RData",sep=""))

