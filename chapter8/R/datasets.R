#' @param N number of objects per class
#' @param d number of dimensions
#' @delta delta Mahanalobis distance between classes
generateRaudysGaussian <- function (N, p, delta) 
{
  X <- rbind(mvrnorm(N, rep(-0.5*delta/sqrt(p), p), diag(p)), 
             mvrnorm(N, rep(0.5*delta/sqrt(p), p), diag(p)))
  y <- rbind(matrix(-1, N, 1), matrix(1, N, 1))
  
  return(data.frame(X, Class = factor(y)))
}
