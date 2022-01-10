#' Permutation Test for Hotelling's T-squared Test Statistics
#'
#' Perform Permutation Test for Hotelling's T-squared Test Statistics
#' @param z A matrix of data set (each row is an observation)
#' @param sizes A 2-dimension vector of sample sizes
#' @param alpha Significance level
#' @param B The number of replicate in permutation test
#'
#' @return A decision on whether the null hypothesis is rejected
#' @export
#'
#' @examples
#' z<- matrix(rnorm(100),ncol=5)
#' HTT.perm(z,c(10,10),0.05,10)
HTT.perm<- function(z, sizes,alpha,B){
  T0<- HTT(z,1:nrow(z),sizes)
  Tperm<- rep(0,B)
  for (b in 1:B){
    permindex<- sample(1:nrow(z))
    Tperm[b]<- HTT(z,permindex,sizes)
  }
  P <- mean(c(T0, Tperm) >= as.numeric(T0))
  return(P<alpha)
}
