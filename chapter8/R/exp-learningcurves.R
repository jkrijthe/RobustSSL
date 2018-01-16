library(MASS, warn.conflicts = FALSE)
library(dplyr, warn.conflicts = FALSE)
library(purrr, warn.conflicts = FALSE)
library(tidyr, warn.conflicts = FALSE)

source("classifiers.R")


generate_lcset <- function(label_set_sizes,delta=4.65) {
  data_frame(
    train = label_set_sizes %>% 
      map(generateRaudysGaussian,p=50,delta=delta),
    test = replicate(length(label_set_sizes),generateRaudysGaussian(500,p=50,delta=delta),FALSE),
    N=label_set_sizes
  )
}

generate_all <- function(repeats,n_samples,label_set_sizes) {
  replicate(repeats,generate_lcset(label_set_sizes),FALSE) %>%
    bind_rows(.id="Repeat")
}

all_class <- function(train,test) {
  truth <- test$Class
  out <- data_frame(name=c("Within","Total","Semi-supervised","Update Means","Least Squares","Least Squares Centered","semi-ls"), 
                    models = list(
                      train %>% FisherLDAClassifier(Class~., .,type="between"),
                      train %>% FisherLDAClassifier(Class~., .,type="total"),
                      train %>% SemiFisherLDAClassifier(Class~., .,n_labeled_per_class=5),
                      train %>% SemiFisherLDAClassifier(Class~., .,n_labeled_per_class=5,update="means"),
                      train %>% LeastSquaresClassifier(Class~.,.),
                      train %>% LeastSquaresClassifier(Class~.,.,x_center = TRUE,scale = FALSE, y_scale = TRUE),
                      train %>% SemiLeastSquaresClassifier(Class~.,.,n_labeled_per_class=5)
                    )) %>% mutate(
                      preds = map(models, predict,test),
                      error = map_dbl(preds, function(x){ 1-mean(x==truth)})
                    )
  out
}

out <- generate_all(repeats=50,n_samples=100,label_set_sizes=seq(2,40,1)) %>% 
  mutate(classifiers = map2(train, test, all_class)) %>% 
  select(-train,-test) %>% 
  unnest() %>% 
  select(-preds,-models)

save(out,file="learningcurve.RData")
