#' Permutation Test for Graph-based Test Statistics
#'
#' Perform Permutation Test for Graph-based Test Statistics
#' @param dst A distance matrix of data set
#' @param sizes A 2-dimension vector of sample sizes
#' @param Q The threshold in graph construction
#' @param alpha Significance level
#' @param B The number of replicate in permutation test
#'
#' @return A decision on whether the null hypothesis is rejected
#' @export
#'
#' @examples
#' z<- matrix(rnorm(100),ncol=5)
#' dst<- L.p.distance(z,2)
#' GST.perm(dst,c(10,10),2,0.05,10)
GST.perm<- function(dst, sizes,Q,alpha,B){
  T0<- GST(dst,1:nrow(dst),sizes,Q)
  Tperm<- rep(0,B)
  for (b in 1:B){
    permindex<- sample(1:nrow(dst))
    Tperm[b]<- GST(dst,permindex,sizes,Q)
  }
  P<-mean(c(T0,Tperm)>=T0)
  return(P<alpha)
}
