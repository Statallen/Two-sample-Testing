#' Energy-based Test
#'
#' Evaluate Energy-based Test Statistics
#' @param dst A distance matrix of data set
#' @param ix A permutation of row indices of dst
#' @param sizes A 2-dimension vector of sample sizes
#'
#' @return A 1-dimension numeric value of test statistics
#' @export
#'
#' @examples
#' z<- matrix(rnorm(100),ncol=5)
#' dst<- L.p.distance(z,2)
#' EBT(dst,1:20,c(10,10))
EBT <- function(dst, ix, sizes) {
  n1 <- sizes[1]
  n2 <- sizes[2]
  ii <- ix[1:n1]
  jj <- ix[(n1+1):(n1+n2)]
  w <- n1 * n2 / (n1 + n2)
  m11 <- sum(dst[ii, ii]) / (n1 * n1)
  m22 <- sum(dst[jj, jj]) / (n2 * n2)
  m12 <- sum(dst[ii, jj]) / (n1 * n2)
  e <- w * ((m12 + m12) - (m11 + m22))
  return (e)
}
