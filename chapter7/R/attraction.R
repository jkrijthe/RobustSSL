library(RSSL)
library(magrittr)
library(ggplot2)
library(dplyr,warn.conflicts = FALSE)
library(tidyr)

set.seed(4)
df_gauss <- generate2ClassGaussian(1000,d=2,var=0.2,expected = FALSE) %>%
  add_missinglabels_mar(Class~.,prob=0.995)
df_test <- generate2ClassGaussian(400,d=2,var=0.2,expected=FALSE)

c_sup <- LeastSquaresClassifier(Class~.,df_gauss)

c_soft <- EMLeastSquaresClassifier(Class~.,df_gauss,objective="label",method="block")
c_hard <- EMLeastSquaresClassifier(Class~.,df_gauss,objective="responsibility",method="block")
c_self <- SelfLearning(Class~.,df_gauss,method=LeastSquaresClassifier)

df_sup <- 
  data.frame(Soft=c_soft@opt_res$value,
             Hard=c_hard@opt_res$value,
             Init="Supervised")

df_sup_perf <- 
  data.frame(Soft=mean(predict(c_soft,df_test)==df_test$Class),
             Hard=mean(predict(c_hard,df_test)==df_test$Class),
             Init="Supervised")

df_sup_loss <- 
  data.frame(Soft=mean(loss(c_soft,df_test)),
             Hard=mean(loss(c_hard,df_test)),
             Init="Supervised")

results <- matrix(NA,100,2)
results_perf <- matrix(NA,100,2)
results_loss <- matrix(NA,100,2)
for (i in 1:100) {
  c_soft <- EMLeastSquaresClassifier(Class~.,df_gauss,objective="label",init="random", method = "simple")
  c_hard <- EMLeastSquaresClassifier(Class~.,df_gauss,objective="responsibility", init="random", method="simple")
  results[i,1] <- c_soft@opt_res$value
  results[i,2] <- c_hard@opt_res$value
  results_perf[i,1] <- mean(predict(c_soft,df_test)==df_test$Class)
  results_perf[i,2] <- mean(predict(c_hard,df_test)==df_test$Class)
  results_loss[i,1] <- mean(loss(c_soft,df_test))
  results_loss[i,2] <- mean(loss(c_hard,df_test))
}

df_gauss %>%
  as.tbl %>%
  ggplot() +
  geom_point(aes(x=X1,y=X2),color="black",size=1,data=df_gauss %>% filter(is.na(Class))) +
  geom_point(aes(x=X1,y=X2,color=Class,shape=Class),size=6,data=df_gauss %>% filter(!is.na(Class)),inherit.aes = FALSE) +
  coord_equal() +
  geom_classifier("Soft"=c_soft,"Hard"=c_hard)

results %>% 
  as.data.frame %>% 
  set_colnames(c("Soft","Hard")) %>% 
  mutate(Init="Random") %>% 
  rbind(df_sup) %>% 
  gather(Method,Value,-Init) %>% 
  group_by(Method) %>% 
  mutate(Value=Value/min(Value)) %>% 
  ungroup %>%
  rename(Initialization=Init) %>% 
  ggplot(aes(x=0,y=Value,color=Initialization)) +
  facet_wrap(~Method,scales = "free") +
  geom_point() +
  ylab("Objective/Best Objective Found")

results_perf %>% 
  as.data.frame %>% 
  set_colnames(c("Soft","Hard")) %>% 
  mutate(Init="Random") %>%
  rbind(df_sup_perf) %>%
  gather(Method,Value,-Init) %>% 
  rename(Initialization=Init) %>% 
  ggplot(aes(x=0,y=Value,color=Initialization)) +
  facet_wrap(~Method,scales = "free") +
  geom_point() +
  ylab("Accuracy on Test")

results_loss %>% 
  as.data.frame %>% 
  set_colnames(c("Soft","Hard")) %>% 
  mutate(Init="Random") %>% 
  rbind(df_sup_loss) %>% 
  gather(Method,Value,-Init) %>% 
  group_by(Method) %>% 
  #mutate(Value=Value/min(Value)) %>% 
  ungroup %>%
  rename(Initialization=Init) %>% 
  ggplot(aes(x=0,y=Value,color=Initialization)) +
  facet_wrap(~Method,scales = "free") +
  geom_point() +
  ylab("Loss on test")


# 1. For different datasets
# 2. Start with diffferent w's and see where we converge to.
set.seed(4)


df_gauss <- generate2ClassGaussian(1000,d=2,var=0.2,expected = FALSE) %>%
  add_missinglabels_mar(Class~.,prob=0.995)
df_test <- generate2ClassGaussian(400,d=2,var=0.2,expected=FALSE)

c_sup <- LeastSquaresClassifier(Class~.,df_gauss)

results <- matrix(NA,100,6)
results_str <- matrix(NA,100,3)
for (i in 1:100) {
  strt <- c_sup@theta+rnorm(3) 
  print(i)
  results_str[i,] <- strt
  c_soft <- EMLeastSquaresClassifier(Class~.,df_gauss,objective="label",init=strt, method = "block")
  c_hard <- EMLeastSquaresClassifier(Class~.,df_gauss,objective="responsibility",init=strt, method = "block")
  results[i,1:3] <- c_soft@theta
  results[i,4:6] <- c_hard@theta
}

res_sup_soft <- c(EMLeastSquaresClassifier(Class~.,df_gauss,objective="label",init=c_sup@theta, method = "block")@theta, c_sup@theta)

res_sup_hard <- c(EMLeastSquaresClassifier(Class~.,df_gauss,objective="responsibility",init=c_sup@theta, method = "block")@theta, c_sup@theta)

save(results,results_str,res_sup_soft,res_sup_hard,file="attraction.RData")

library(cowplot)
plot_soft <- cbind(results[,1:3],results_str) %>%
  rbind(res_sup_soft) %>% 
  as.data.frame %>% 
  set_colnames(c("w1","w2","w3","s1","s2","s3")) %>%
  mutate(Sol=c("Supervised",rep("Random",100))) %>% 
  ggplot() +
  geom_segment(aes(x=s2,xend=w2,y=s3,yend=w3,color=Sol),arrow = arrow(length = unit(0.00, "npc")),size=0.4,alpha=0.5) +
  scale_color_manual(values=c("black","red")) +
  ggtitle("Soft Labeling") +
  theme(legend.position="none")



plot_hard <- cbind(results[,4:6],results_str) %>% 
  as.data.frame %>%
  rbind(res_sup_hard) %>% 
  set_colnames(c("w1","w2","w3","s1","s2","s3")) %>% 
  mutate(Sol=c("Supervised",rep("Random",100))) %>%
  ggplot() +
  geom_segment(aes(x=s2,xend=w2,y=s3,yend=w3,color=Sol),size=0.4,alpha=0.5) +
  scale_color_manual(values=c("black","red")) +
  ggtitle("Hard Labeling") +
  theme(legend.position="none")

plot_grid(plot_soft, plot_hard, labels = c("A", "B"))

# Add supervised starting solution.