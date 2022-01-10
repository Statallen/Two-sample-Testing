#' Lp Distance
#'
#' Calculating the designated form of distance matrix
#' @param x A matrix of data set (each row is an observation)
#' @param p The specific form of distance
#'
#' @return A distance matrix of the chosen distance form
#' @export
#'
#' @examples
#' z<- matrix(rnorm(100),ncol=5)
#' L.p.distance(z,2)
L.p.distance<- function(x,p){
  x<- as.matrix(x)
  n<-dim(x)[1]
  results<- matrix(0,nrow = n,ncol = n)
  diag(results)<- 0
  for (i in 1:(n-1)){
    for (j in (i+1):n){
      results[i,j]<- (sum((abs(x[i,]-x[j,]))^p))^(1/p)
    }
  }
  results<- results+t(results)
  return(results)
}
