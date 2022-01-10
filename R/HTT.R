#' Hotelling's T-squared Test
#'
#' Evaluate Hotelling's T-squared Test Statistics
#' @param z A matrix of data set (each row is an observation)
#' @param ix A permutation of row indices of z
#' @param sizes A 2-dimension vector of sample sizes
#'
#' @return A 1-dimension numeric value of test statistics
#' @export
#'
#' @examples
#' z<- matrix(rnorm(100),ncol=5)
#' HTT(z,1:20,c(10,10))
HTT<- function(z, ix, sizes){
  n1 <- sizes[1]
  n2 <- sizes[2]
  z<- z[ix,,drop=F]
  x.bar<- matrix(colSums(z[1:n1,,drop=F])/n1,nrow = 1,ncol = dim(z)[2])
  y.bar<- matrix(colSums(z[(n1+1):(n1+n2),,drop=F])/n2,nrow = 1,ncol = dim(z)[2])
  x.bar.matrix<- matrix(as.numeric(x.bar),nrow = n1,ncol = dim(z)[2],byrow = T)
  y.bar.matrix<- matrix(as.numeric(y.bar),nrow = n2,ncol = dim(z)[2],byrow = T)
  diff.x<- z[1:n1,,drop=F]-x.bar.matrix
  diff.y<- z[(n1+1):(n1+n2),,drop=F]-y.bar.matrix
  sigma.x.hat<- (1/(n1-1))*t(diff.x)%*%diff.x
  sigma.y.hat<- (1/(n2-1))*t(diff.y)%*%diff.y
  sigma.hat<- (1/(n1+n2-2))*((n1-1)*sigma.x.hat+(n2-1)*sigma.y.hat)
  T.square<- ((n1*n2)/(n1+n2))*(x.bar-y.bar)%*%(solve(sigma.hat))%*%t(x.bar-y.bar)
  return(T.square)
}
