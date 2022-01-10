#' Permutation Test for Nearest Neighbor Statistics
#'
#' Perform Permutation Test for Nearest Neighbor Test Statistics
#'
#' @param z A matrix of data set (each row is an observation)
#' @param sizes A 2-dimension vector of sample sizes
#' @param R The number of neighborhood to be evaluated
#' @param alpha Significance level
#' @param B The number of replicate in permutation test
#'
#' @return A decision on whether the null hypothesis is rejected
#' @export
#'
#' @examples
#' z<- matrix(rnorm(100),ncol=5)
#' NNT(z,1:20,c(10,10),3)
#' NNT.perm(z,c(10,10),3,0.05,10)
NNT.perm <- function(z, sizes, R,alpha,B) {
  T0=NNT(z,1:nrow(z),sizes, R)
  TPerm=rep(0,B)
  for (b in 1:B) {
    permindex=sample(1:nrow(z))
    TPerm[b]=NNT(z,permindex,sizes, R)
  }
  P <- mean(c(T0, TPerm) >= T0)
  return(P<alpha)
}
