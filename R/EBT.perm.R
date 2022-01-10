#' Permutation Test for Energy-based Statistics
#'
#' Perform Permutation Test for Energy-based Test Statistics
#' @param dst A distance matrix of data set
#' @param sizes A 2-dimension vector of sample sizes
#' @param alpha Significance level
#' @param B The number of replicate in permutation test
#'
#' @return A decision on whether the null hypothesis is rejected
#' @export
#'
#' @examples
#' z<- matrix(rnorm(100),ncol=5)
#' dst<- L.p.distance(z,2)
#' EBT.perm(dst,c(10,10),0.05,10)
EBT.perm <- function(dst, sizes,alpha,B) {
  T0=EBT(dst,1:nrow(dst),sizes)
  TPerm=rep(0,B)
  for (b in 1:B) {
    permindex=sample(1:nrow(dst))
    TPerm[b]=EBT(dst,permindex,sizes)
  }
  P <- mean(c(T0, TPerm) >= T0)
  return(P<alpha)
}
