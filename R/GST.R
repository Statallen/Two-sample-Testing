#' Graph-based Test
#'
#' Evaluate Graph-based Test Statistics
#'
#' @param dst A distance matrix of data set
#' @param ix A permutation of row indices of dst
#' @param sizes A 2-dimension vector of sample sizes
#' @param Q The threshold in graph construction
#'
#' @return A 1-dimension numeric value of test statistics
#' @export
#'
#' @examples
#' z<- matrix(rnorm(100),ncol=5)
#' dst<- L.p.distance(z,2)
#' GST(dst,1:20,c(10,10),2)
GST<- function(dst, ix, sizes, Q){
  n1 <- sizes[1]
  n2 <- sizes[2]
  ii <- ix[1:n1]
  jj <- ix[(n1+1):(n1+n2)]
  n.edges.x<- sum(dst[ii,ii]<=Q)
  n.edges.y<- sum(dst[jj,jj]<=Q)
  n.edges.across<- sum(dst[ii,jj]<=Q)
  total.edges<- n.edges.across*2+n.edges.x+n.edges.y
  R<- (1/total.edges)*(n.edges.x+n.edges.y)
  return(R)
}
