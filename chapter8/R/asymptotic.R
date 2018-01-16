gamma_function <- function(n,p,rep=10) {
  d <- matrix(NA,rep,1)
  for(i in 1:rep) {
    W <- mvrnorm(n,rep(0,p),diag(p))
    eig <- prcomp(W)$sdev^2
    d[i,1] <-1/(sample(eig[1:(n-1)],1))
  }
  
  sqrt(var(d))/mean(d)
}

gamma_function(30,50,100)

gamma_ext_function <- function(n,p,delta,rep=10) {
  d <- matrix(NA,rep,1)
  for(i in 1:rep) {
    X <- generateRaudysGaussian(n/2,p,delta)
    X$Class <- NULL
    
    eig <- prcomp(X)$sdev^2
    d[i,1] <-1/(sample(eig[1:(n-1)],1))
  }
  
  sqrt(var(d))/mean(d)
}

gamma_ext_function(30,50,1,100)

#' Raudys (fixed) formulation of the asymptotic learning curve
#' Slightly different from equation (10) which seems to be incorrect
#' @param delta Mahanalobis distance between classes
#' @param N the number of objects per class
#' @param p dimensionality of the input
raudys <- function(delta,N,p,type="within",rep=1000) {
  if (type=="total") {
    print("asd")
    gamma <- gamma_ext_function(2*N,p,delta,rep)
  } else {
    gamma <- gamma_function(2*N,p,rep)
  }
  #Old
  # T_term <- 1 + (1/N) + (4*p^2)/(N^2 * delta^2) + (2*p^2)/(delta^2 * N^3)
  # sqrt_term <- (1+gamma^2) * T_term + gamma^2 * 3 * N * (delta^2)/(8*p^2)
  # I <- -(delta/2)*sqrt(N/(2*p)) * 1/(sqrt(sqrt_term))
  
  # new
  T_term <- 1 + (1/N) + (p^2)/(N^2 * delta^2) + (p^2)/(2*delta^2 * N^3)
  sqrt_term <- (1+gamma^2) * T_term + gamma^2 * 3 * (delta^2)/(4*p)
  I <- -(delta/2)*sqrt(2*N/(p)) * 1/(sqrt(sqrt_term))
  pnorm(I)
}

raudys_fixed <- function(delta,N,p,type="within",rep=1000,N_l=5) {
  if (type=="total") {
    print("asd")
    gamma <- gamma_ext_function(2*N,p,delta,rep)
  } else {
    gamma <- gamma_function(2*N,p,rep)
  }
  #Old
  # T_term <- 1 + (1/N) + (4*p^2)/(N^2 * delta^2) + (2*p^2)/(delta^2 * N^3)
  # sqrt_term <- (1+gamma^2) * T_term + gamma^2 * 3 * N * (delta^2)/(8*p^2)
  # I <- -(delta/2)*sqrt(N/(2*p)) * 1/(sqrt(sqrt_term))
  
  # new
  T_term <- 1 + (1/N_l) + (p^2)/(N_l^2 * delta^2) + (p^2)/(2*delta^2 * N_l^3)
  sqrt_term <- (1+gamma^2) * T_term + gamma^2 * 3 * (delta^2)/(4*p)
  I <- -(delta/2)*sqrt(2*N/(p)) * 1/(sqrt(sqrt_term))
  pnorm(I)
}

raudys_big <- function(delta,N,p,N_l=NULL) {
  if (is.null(N_l)) {
    T_mu <- 1+2 * p / (delta^2 * N)
  } else {
    print("bla")
    T_mu <- 1+2 * p / (delta^2 * N_l)
  }
  T_sigma <- 1 + p/(2*N-p)
  I <- -(delta/2) * (1/sqrt(T_mu*T_sigma))
  pnorm(I)
}

raudys(4.65,10,50)
raudys(4.65,10,50,type="total")