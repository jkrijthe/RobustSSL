library(RSSL)
library(reshape2)
library(purrr)
library(tidyr)
library(MASS)
library(dplyr)
library(ggplot2)
source("datasets.R")
source("classifiers.R")

generate_curves <- function(sizes = seq(1,40,by=1),repeats=10,df_generator=function(n){generateRaudysGaussian(n,50,4)}) {
  
  results <- array(NA,c(length(sizes),repeats,2,3),
                   dimnames=list("Size"=sizes,
                                 "Repeat"=1:repeats,
                                 "Measure"=c("Error","Loss"),
                                 "Classifier"=c("Semi-supervised","Base","Supervised")
                   ))
  for (j in 1:repeats) {
    print(j)
    df_test <- df_generator(1000)
      for(i in seq_along(sizes)) {
      df_1000 <- df_generator(sizes[i])
      
      g_3 <- LeastSquaresClassifier(Class~.,df_1000,x_center=TRUE,y_scale=TRUE)
      df_1000$Class[-c(1:min(sizes[i],10),(nrow(df_1000)/2+1):(nrow(df_1000)/2+min(sizes[i],10)))] <- NA
      g_1 <- USMLeastSquaresClassifier(Class~.,df_1000,x_center=TRUE,y_scale=TRUE)
      g_2 <- LeastSquaresClassifier(Class~.,df_1000,x_center=TRUE,y_scale=TRUE)
      
      results[i,j,1,1] <- 1-mean(predict(g_1,df_test)==df_test$Class)
      results[i,j,1,2] <- 1-mean(predict(g_2,df_test)==df_test$Class)
      results[i,j,1,3] <- 1-mean(predict(g_3,df_test)==df_test$Class)
      results[i,j,2,1] <- mean(loss(g_1,df_test))
      results[i,j,2,2] <- mean(loss(g_2,df_test))
      results[i,j,2,3] <- mean(loss(g_2,df_test))
    }
  }
  melt(results)
}

res <- generate_curves(repeats=500)

save(res,file="exp-illustration.RData")

load("exp-illustration.RData")
res %>% 
  group_by(Classifier,Measure,Size) %>% 
  summarize(Mean=mean(value)) %>% 
  ungroup %>% 
  ggplot(aes(x=Size,y=Mean,color=Classifier)) +
  geom_line() +
  facet_wrap(~Measure,scales = "free")

