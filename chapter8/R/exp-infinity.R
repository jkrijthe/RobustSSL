library(RSSL)
library(reshape2)
library(purrr)
library(tidyr)
library(MASS)
library(dplyr)
library(ggplot2)
source("datasets.R")
source("classifiers.R")

generate_curves <- function(sizes = seq(1,100,by=2),repeats=10,df_generator=function(n){generateRaudysGaussian(n,50,4)}) {

  results <- array(NA,c(length(sizes),repeats,2,2),
                   dimnames=list("Size"=sizes,
                                "Repeat"=1:repeats,
                                "Measure"=c("Error","Loss"),
                                "Classifier"=c("Semi-supervised","Supervised")
                                ))
  
  for(i in seq_along(sizes)) {
    df_test <- df_generator(1000)
    for (j in 1:repeats) {
      df_1000 <- df_generator(1000)
      df_1000$Class[-c(1:sizes[i],(nrow(df_1000)/2+1):(nrow(df_1000)/2+sizes[i]))] <- NA
      g_1 <- USMLeastSquaresClassifier(Class~.,df_1000,x_center=TRUE,y_scale=TRUE)
      g_2 <- LeastSquaresClassifier(Class~.,df_1000,x_center=TRUE,y_scale=TRUE)
      results[i,j,1,1] <- 1-mean(predict(g_1,df_test)==df_test$Class)
      results[i,j,1,2] <- 1-mean(predict(g_2,df_test)==df_test$Class)
      results[i,j,2,1] <- mean(loss(g_1,df_test))
      results[i,j,2,2] <- mean(loss(g_2,df_test))
    }
  }
  melt(results)
}

generate_curves_asymptotic <- function(sizes = seq(1,100,by=2),repeats=10,p,delta) {
  
  df_generator=function(n){generateRaudysGaussian(n,p,delta)}
  
  results <- array(NA,c(length(sizes),repeats,2,2),
                   dimnames=list("Size"=sizes,
                                 "Repeat"=1:repeats,
                                 "Measure"=c("Error","Loss"),
                                 "Classifier"=c("Semi-supervised","Supervised")
                   ))
  
  Tinv <- solve(diag(p) + matrix(0.25*delta^2/p,p,p))
  
  for (j in 1:repeats) {
    df_test <- df_generator(1000)
    X_test <- as.matrix(df_test[,1:p])
    y_test <- df_test$Class
    for(i in seq_along(sizes)) {
      df_train <- df_generator(max(sizes[i]))
    
      df_1000 <- df_train[c(1:sizes[i],(nrow(df_train)/2+1):(nrow(df_train)/2+sizes[i])), ] 
      X_train <- as.matrix(df_train[,1:p])
      y_train <- df_train$Class
      
      g_1 <- GivenCovLeastSquaresClassifier(X_train,y_train,Tinv,intercept=FALSE,x_center=TRUE,y_scale=TRUE)
      g_2 <- LeastSquaresClassifier(X_train,y_train,x_center=TRUE,y_scale=TRUE)
      results[i,j,1,1] <- 1-mean(predict(g_1,X_test)==y_test)
      results[i,j,1,2] <- 1-mean(predict(g_2,X_test)==y_test)
      results[i,j,2,1] <- mean(loss(g_1,X_test,y_test))
      results[i,j,2,2] <- mean(loss(g_2,X_test,y_test))
    }
  }
  melt(results)
}

res_asymptotic <- generate_curves_asymptotic(sizes = seq(1,100,by=2),repeats=100,p=50,delta=4)
res <- generate_curves(sizes = seq(1,100,by=2),repeats=2,df_generator=function(n){generateRaudysGaussian(n,50,4)})
res_asymptotic %>% 
  group_by(Classifier,Measure,Size) %>% 
  summarize(Mean=mean(value)) %>% 
  ungroup %>% 
  ggplot(aes(x=Size,y=Mean,color=Classifier)) +
  geom_line() +
  facet_wrap(~Measure,scales = "free")



res <- expand.grid("p"=c(10,20,50),"delta"=c(0.5,1,2,4,8)) %>% 
  mutate(out=map2(p,delta,function(p,delta){
    generate_curves(sizes = seq(1,100,by=2),repeats=100,df_generator=function(n){generateRaudysGaussian(n,p,delta)})
  }))

res_asymptotic_grid <- expand.grid("p"=c(10,20,50,100),"delta"=c(0.5,1,2,4,8)) %>% 
  mutate(out=map2(p,delta,function(p,delta){
    generate_curves_asymptotic(sizes = seq(1,100,by=2),repeats=100,p=p,delta=delta)
  }))

#save(res,res_asymptotic_grid,file="exp-infinity.RData")

load("exp-infinity.RData")

res %>% 
  mutate(p=factor(p),delta=factor(delta)) %>% 
  unnest() %>% 
  filter(Measure=="Loss") %>% 
  group_by(Classifier,Measure,Size,p,delta) %>% 
  summarize(Mean=mean(value)) %>% 
  ungroup %>% 
  ggplot(aes(x=Size,y=Mean,color=Classifier)) +
  geom_line() +
  facet_grid(delta~p,scales = "free") +
  coord_cartesian(ylim = c(0,4))

res %>% 
  mutate(p=factor(p),delta=factor(delta)) %>% 
  unnest() %>% 
  filter(Measure=="Error") %>% 
  group_by(Classifier,Measure,Size,p,delta) %>% 
  summarize(Mean=mean(value)) %>% 
  ungroup %>% 
  ggplot(aes(x=Size,y=Mean,color=Classifier)) +
  geom_line() +
  facet_grid(delta~p,scales = "free") 

res_asymptotic_grid %>% 
  mutate(p=factor(p),delta=factor(delta)) %>% 
  unnest() %>% 
  filter(Measure=="Loss") %>% 
  group_by(Classifier,Measure,Size,p,delta) %>% 
  summarize(Mean=mean(value)) %>% 
  ungroup %>% 
  ggplot(aes(x=Size,y=Mean,color=Classifier)) +
  geom_line() +
  facet_grid(delta~p,scales = "free") +
  coord_cartesian(ylim = c(0,2))

res_asymptotic_grid %>% 
  mutate(p=factor(p),delta=factor(delta)) %>% 
  unnest() %>% 
  filter(Measure=="Error") %>% 
  group_by(Classifier,Measure,Size,p,delta) %>% 
  summarize(Mean=mean(value)) %>% 
  ungroup %>% 
  ggplot(aes(x=Size,y=Mean,color=Classifier)) +
  geom_line() +
  facet_grid(delta~p,scales = "free") 
