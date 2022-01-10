#' Nearest Neighbot Test
#'
#' Evaluate Nearest Neighbor Test Statistics
#'
#' @param z A matrix of data set (each row is an observation)
#' @param ix A permutation of row indices of z
#' @param sizes A 2-dimension vector of sample sizes
#' @param R The number of neighborhood to be evaluated
#'
#' @return A 1-dimension numeric value of test statistics
#' @export
#'
#' @examples
#' z<- matrix(rnorm(100),ncol=5)
#' NNT(z,1:20,c(10,10),3)
NNT <- function(z, ix, sizes, R) {
  n1 <- sizes[1]
  n2 <- sizes[2]
  n <- n1 + n2
  z <- z[ix, ]
  NN <- RANN::nn2(z, z, k=R+1)
  block1 <- NN$nn.idx[1:n1, -1]
  block2 <- NN$nn.idx[(n1+1):n, -1]
  i1 <- sum(block1 < n1 + .5)
  i2 <- sum(block2 > n1 + .5)
  return((i1 + i2) / (R * n))
}







