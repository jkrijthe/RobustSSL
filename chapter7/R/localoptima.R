library(methods)
library(RSSL)
library(createdatasets)
library(magrittr)

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
set.seed(4)


perfs_hard <- list()
perfs_soft <- list()
resamp <- 10
reinit <- 50
for (d in names(datasets)) {
  df <- datasets[[d]]
  mf <- models[[d]]
  perf_soft <- matrix(NA,reinit+2,resamp)
  perf_hard <- matrix(NA,reinit+2,resamp)
for (j in 1:resamp) {
  
  idx_train <- sample(1:nrow(df),round(0.7*nrow(df)))
  df_train <- df[idx_train,,drop=FALSE]
  df_test <- df[-idx_train,,drop=FALSE]
  df_train <- df_train %>% add_missinglabels_mar(mf, 0.8)
  
  c_sup <- LeastSquaresClassifier(mf,df_train)
  
  c_init_hard <- EMLeastSquaresClassifier(mf,df_train,method="block",init="supervised",objective="hard")
  c_init_soft <- EMLeastSquaresClassifier(mf,df_train,method="block",init="supervised",objective="soft")
  
  perf_hard[1,j] <- mean(predict(c_sup,df_test)==df_test[[all.names(mf)[2]]])
  perf_hard[2,j] <- mean(predict(c_init_hard,df_test)==df_test[[all.names(mf)[2]]])
  
  perf_soft[1,j] <- mean(predict(c_sup,df_test)==df_test[[all.names(mf)[2]]])
  perf_soft[2,j] <- mean(predict(c_init_soft,df_test)==df_test[[all.names(mf)[2]]])
  
  for (i in 1:reinit) {
    c_rand_hard <- EMLeastSquaresClassifier(mf,df_train,method="block",init="random",objective="hard")
    c_rand_soft <- EMLeastSquaresClassifier(mf,df_train,method="block",init="random",objective="soft")
    
    perf_soft[i+2,j] <-  mean(predict(c_rand_soft,df_test)==df_test[[all.names(mf)[2]]])
    perf_hard[i+2,j] <-  mean(predict(c_rand_hard,df_test)==df_test[[all.names(mf)[2]]])
  }
  print(j)
}
perfs_hard[[d]] <- perf_hard
perfs_soft[[d]] <- perf_soft
}
#save(perfs_hard,perfs_soft,file="localoptima.RData")
library(dplyr)
library(tidyr)
library(ggplot2)
library(cowplot)



df_soft <- 
  lapply(names(perfs_soft),function(x){
    perfs_soft[[x]] %>% 
    as.data.frame %>% 
    mutate(.,Type=c("Supervised","Soft",rep("Random",nrow(.)-2))) %>% 
    mutate(Dataset=x) 
  }) %>% 
  bind_rows %>% 
  mutate(Experiment="Soft")

df_hard <- 
  lapply(names(perfs_hard),function(x){
    perfs_hard[[x]] %>% 
      as.data.frame %>% 
      mutate(.,Type=c("Supervised","Hard",rep("Random",nrow(.)-2))) %>% 
      mutate(Dataset=x) 
  }) %>% 
  bind_rows %>% 
  mutate(Experiment="Hard")

save(df_soft,df_hard,file="localoptima.RData")

df_soft %>% 
  rbind(df_hard) %>%
  gather(Repeat,Value,-Type,-Dataset,-Experiment) %>% 
  filter(Repeat=="V1") %>% 
  ggplot(aes(x=Dataset,y=Value,color=Type,size=Type)) +
  geom_point(alpha=0.7) +
  scale_color_manual(values=c(Supervised="blue",Hard="red",Soft="Green",Random="black")) +
  scale_size_manual(values=c(Supervised=3,Hard=3,Soft=3,Random=1)) +
  facet_grid(~Experiment)



perf %>% 
  as.data.frame %>% 
  mutate(.,Type=c("Supervised","Hard",rep("Random",nrow(.)-2))) %>% 
  gather(Repeat,Value,-Type) %>% 
  ggplot(aes(x=Repeat,y=Value,color=Type,size=Type)) +
  geom_point(alpha=0.7) +
  scale_color_manual(values=c(Supervised="blue",Hard="red",Random="black")) +
  scale_size_manual(values=c(Supervised=3,Hard=3,Random=1))

perf <- perf16
perfold <- perf 
