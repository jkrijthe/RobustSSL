library(RSSL)

FisherLDAClassifier <- function(X,y,type="total") {
  ModelVariables<-RSSL:::PreProcessing(X,y,X_u=NULL,scale=FALSE,intercept=FALSE,x_center=FALSE)
  X<-ModelVariables$X
  y<-ModelVariables$y
  scaling<-ModelVariables$scaling
  classnames<-ModelVariables$classnames
  modelform<-ModelVariables$modelform
  
  if (length(unique(y))!=2) { stop("Not a two class problem")}
  Y <- model.matrix(~as.factor(y)-1)
  
  prior <- matrix(colMeans(Y),2,1)
  means <- t((t(X) %*% Y))/(colSums(Y))
  
  sigma.classes<-lapply(1:ncol(Y),function(c,X){cov(X[Y[,c]==1, ,drop=FALSE])},X)
  
  sigma<-sigma.classes[[1]]*prior[1]
  for (i in 2:length(sigma.classes)) {
    sigma<-sigma+sigma.classes[[i]]*prior[i]
  }
  
  if (type=="total") {
    S <- cov(X)
  } else if (type=="between") {
    S <- sigma
  }
  
  w <- ginv(S) %*% (means[1,]-means[2,])
  w0 <- -t((means[1,]+means[2,])/2) %*% w
  o <- list(w=c(w0,w),modelform=modelform,classnames=classnames)
  class(o) <- "FisherLDAClassifier"
  return(o)
} 

SemiFisherLDAClassifier <- function(X,y,n_labeled_per_class=5, update="covariance") {
  
  ModelVariables<-RSSL:::PreProcessing(X,y,X_u=NULL,scale=FALSE,intercept=FALSE,x_center=FALSE)
  X<-ModelVariables$X
  X_u<-ModelVariables$X_u
  y<-ModelVariables$y
  scaling<-ModelVariables$scaling
  classnames<-ModelVariables$classnames
  modelform<-ModelVariables$modelform
  
  if (length(unique(y))!=2) { stop("Not a two class problem")}
  if (length(y)!=2) {
  keep <- c(sample(which(y=="-1"),min(c(n_labeled_per_class,length(y)/2))),
            sample(as.numeric(which(y=="1")),min(c(n_labeled_per_class,length(y)/2))))
  } else {
    keep <- 1:2
  }
  X_l <- X[keep,,drop=FALSE]
  y_l <- y[keep]
  
  Y <- model.matrix(~as.factor(y)-1)
  Y_l <- model.matrix(~as.factor(y_l)-1)
  
  
  if (update=="covariance") {
    S <- cov(X)
    prior <- matrix(colMeans(Y_l),2,1)
    means <- t((t(X_l) %*% Y_l))/(colSums(Y_l))
  } else if (update=="means") {
    S <- cov(X_l)
    prior <- matrix(colMeans(Y),2,1)
    means <- t((t(X) %*% Y))/(colSums(Y))
  } else if (update=="nothing") {
    S <- cov(X_l)
    prior <- matrix(colMeans(Y_l),2,1)
    means <- t((t(X_l) %*% Y_l))/(colSums(Y_l))
  }
  
  
  w <- ginv(S) %*% (means[1,]-means[2,])
  w0 <- -t((means[1,]+means[2,])/2) %*% w
  o <- list(w=c(w0,w),modelform=modelform,classnames=classnames)
  class(o) <- "FisherLDAClassifier"
  return(o)
} 

SemiLeastSquaresClassifier <- function(X,y,n_labeled_per_class=5) {
  
  keep <- c(sample(which(y$Class=="-1"),min(c(n_labeled_per_class,nrow(y)/2))),
            sample(which(y$Class=="1"),min(c(n_labeled_per_class,nrow(y)/2))))
  y$Class[-keep] <- NA
  USMLeastSquaresClassifier(X,y,X_u=NULL,scale=TRUE,x_center=TRUE,y_scale=TRUE)
} 

predict.FisherLDAClassifier <- function(object,newdata,...) {
  ModelVariables <- RSSL:::PreProcessingPredict(object$modelform,newdata,scaling=NULL,intercept=TRUE,classnames=object$classnames)
  X <- ModelVariables$X
  factor(X %*% object$w > 0, levels=c(TRUE,FALSE), labels=object$classnames)
}

UpdateMeanLeastSquaresClassifier<-function(X, y, X_u, y_u, lambda=0, intercept=TRUE, x_center=FALSE, scale=FALSE, y_scale=FALSE, ...) {
  ## Preprocessing to correct datastructures and scaling  
  ModelVariables <- PreProcessing(X, unlist(list(y,y_u)), X_u=X_u, scale=scale, intercept=intercept, x_center=x_center,use_Xu_for_scaling=FALSE)
  X<-ModelVariables$X
  X_u<-ModelVariables$X_u
  #y<-ModelVariables$y
  scaling<-ModelVariables$scaling
  classnames<-ModelVariables$classnames
  modelform<-ModelVariables$modelform
  
  y <- ModelVariables$Y
  if (y_scale) {
    y_scale <- mean(y)
    y <- y-y_scale
  } else {
    y_scale <- 0
  }
  
  if (length(classnames)!=2) stop("Dataset does not contain 2 classes")
  
  ## Start Implementation
  n <- nrow(X)
  m <- ncol(X)
  k <- ncol(y)
  
  inv <- ginv
  
  Xe <- rbind(X,X_u)
  
  if (intercept) {
    theta <- inv(t(X) %*% X + n*lambda*diag(c(0,rep(1,(m-1))))) %*% ((n/nrow(Xe)) * t(Xe) %*% y)
  } else {
    theta <- inv(t(X) %*% X + n*lambda*diag(rep(1,m))) %*% ((n/nrow(Xe)) * t(Xe) %*% y)
  }
  
  ## Return correct object
  new("USMLeastSquaresClassifier",
      classnames=classnames,
      scaling=scaling,
      theta=theta,
      modelform=modelform,
      intercept=intercept,
      y_scale=y_scale
  )
}

UpdateBothLeastSquaresClassifier <- function(X, y, X_u, X_m, y_m, lambda=0, intercept=TRUE, x_center=FALSE, scale=FALSE, y_scale=FALSE, ...,use_Xu_for_scaling=TRUE) {
  ## Preprocessing to correct datastructures and scaling  
  ModelVariables <- PreProcessing(X, unlist(list(y,y_m)), X_u=X_u, scale=scale, intercept=intercept, x_center=x_center,use_Xu_for_scaling=TRUE)
  X<-ModelVariables$X
  X_u<-ModelVariables$X_u
  #y<-ModelVariables$y
  scaling<-ModelVariables$scaling
  classnames<-ModelVariables$classnames
  modelform<-ModelVariables$modelform
  
  y <- ModelVariables$Y
  if (y_scale) {
    y_scale <- mean(y)
    y <- y-y_scale
  } else {
    y_scale <- 0
  }
  
  if (length(classnames)!=2) stop("Dataset does not contain 2 classes")
  
  # There is a problem using ginv when using PCs as inputs: the problems seem to be rescaled such that the bias term is no longer correct
  
  ## Start Implementation
  n <- nrow(X)
  m <- ncol(X)
  k <- ncol(y)
  
  Xtrain<-NULL
  if (nrow(X)<ncol(X)) inv <- function(X) { ginv(X) }
  else inv <- function(X) { ginv(X) }
  
  if (intercept) X_u <- cbind(1,X_u)
  
  X_m <- predict(scaling,X_m)
  
  
  Xe <- rbind(X,X_u)
  Xm <- rbind(X,X_m)
  
  if (intercept) {
    theta <- inv(t(Xe) %*% Xe + n*lambda*diag(c(0,rep(1,(m-1))))) %*% (  t(Xm) %*% y)
  } else {
    theta <- inv(t(Xe) %*% Xe + n*lambda*diag(rep(1,m))) %*%  (t(Xm) %*% y)
  }
  
  ## Return correct object
  new("USMLeastSquaresClassifier",
      classnames=classnames,
      scaling=scaling,
      theta=theta,
      modelform=modelform,
      intercept=intercept,
      y_scale=y_scale
  )
}


UpdateRankLeastSquaresClassifier <- function(X, y, X_u, y_u, lambda=0, intercept=TRUE, x_center=FALSE, scale=FALSE, y_scale=FALSE, ...,use_Xu_for_scaling=TRUE,type="both") {
  ## Preprocessing to correct datastructures and scaling  
  ModelVariables <- PreProcessing(X, unlist(list(y,y_u)), X_u=X_u, scale=scale, intercept=intercept, x_center=x_center,use_Xu_for_scaling=TRUE)
  X<-ModelVariables$X
  X_u<-ModelVariables$X_u
  #y<-ModelVariables$y
  scaling<-ModelVariables$scaling
  classnames<-ModelVariables$classnames
  modelform<-ModelVariables$modelform
  
  y <- ModelVariables$Y
  if (y_scale) {
    y_scale <- mean(y)
    y <- y-y_scale
  } else {
    y_scale <- 0
  }
  
  if (length(classnames)!=2) stop("Dataset does not contain 2 classes")
  
  # There is a problem using ginv when using PCs as inputs: the problems seem to be rescaled such that the bias term is no longer correct
  
  ## Start Implementation
  n <- nrow(X)
  m <- ncol(X)
  k <- ncol(y)
  
  Xtrain<-NULL
  
  svd_inv <- function (E, keep_rank=NULL) {
    sv <- svd(E)
    
    if (is.null(keep_rank)) {
      rank <- sum(sv$d>10^-12)
    } else {
      
      rank <- min(c(keep_rank,nrow(E)))
      
    }
    invsv <- c(1/sv$d[1:rank],rep(0,nrow(E)-rank))
    
    Einv <- sv$u %*% diag(invsv) %*% t(sv$v)
  }
  
  Xe <- rbind(X,X_u)
  
  if (type=="both") {
    if (intercept) {
      theta <- svd_inv(t(Xe) %*% Xe + n*lambda*diag(c(0,rep(1,(m-1)))),keep_rank=rankMatrix(t(X)%*% X)) %*% (  t(Xe) %*% y)
    } else {
      theta <- svd_inv(t(Xe) %*% Xe + n*lambda*diag(rep(1,m)),keep_rank=rankMatrix(t(X)%*% X)) %*%  (t(Xe) %*% y)
    } 
  } else if (type=="covariance") {
    if (intercept) {
      theta <- svd_inv((n/nrow(Xe))* t(Xe) %*% Xe + n*lambda*diag(c(0,rep(1,(m-1)))),keep_rank=rankMatrix(t(X)%*% X)) %*% (  t(X) %*% y[1:nrow(X),])
    } else {
      theta <- svd_inv((n/nrow(Xe))* t(Xe) %*% Xe + n*lambda*diag(rep(1,m)),keep_rank=rankMatrix(t(X)%*% X)) %*%  (t(X) %*% y[1:nrow(X),])
    }
  } else if (type=="mean") {
    if (intercept) {
      theta <- svd_inv(t(X) %*% X + n*lambda*diag(c(0,rep(1,(m-1)))),keep_rank=rankMatrix(t(X)%*% X)) %*% (  (n/nrow(Xe))*  t(Xe) %*% y)
    } else {
      theta <- svd_inv(t(X) %*% X + n*lambda*diag(rep(1,m)),keep_rank=rankMatrix(t(X)%*% X)) %*%  (n/nrow(Xe))* (t(Xe) %*% y)
    }
  }
  
  ## Return correct object
  new("USMLeastSquaresClassifier",
      classnames=classnames,
      scaling=scaling,
      theta=theta,
      modelform=modelform,
      intercept=intercept,
      y_scale=y_scale
  )
}



GivenCovLeastSquaresClassifier<-function(X, y, invcov, lambda=0, intercept=TRUE, x_center=FALSE, scale=FALSE, y_scale=FALSE, ...) {
  ## Preprocessing to correct datastructures and scaling  
  ModelVariables <- PreProcessing(X, y, X_u=NULL, scale=scale, intercept=intercept, x_center=x_center,use_Xu_for_scaling=FALSE)
  X<-ModelVariables$X
  X_u<-ModelVariables$X_u
  #y<-ModelVariables$y
  scaling<-ModelVariables$scaling
  classnames<-ModelVariables$classnames
  modelform<-ModelVariables$modelform
  
  y <- ModelVariables$Y
  if (y_scale) {
    y_scale <- mean(y)
    y <- y-y_scale
  } else {
    y_scale <- 0
  }
  
  if (length(classnames)!=2) stop("Dataset does not contain 2 classes")
  
  ## Start Implementation
  n <- nrow(X)
  m <- ncol(X)
  k <- ncol(y)
  
  inv <- ginv
  
  
  if (intercept) {
    theta <- invcov %*% ((1/n) * t(X) %*% y)
  } else {
    theta <- invcov %*% ((1/n) * t(X) %*% y)
  }
  
  ## Return correct object
  new("USMLeastSquaresClassifier",
      classnames=classnames,
      scaling=scaling,
      theta=theta,
      modelform=modelform,
      intercept=intercept,
      y_scale=y_scale
  )
}