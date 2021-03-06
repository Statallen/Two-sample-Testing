% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/GST.R
\name{GST}
\alias{GST}
\title{Graph-based Test}
\usage{
GST(dst, ix, sizes, Q)
}
\arguments{
\item{dst}{A distance matrix of data set}

\item{ix}{A permutation of row indices of dst}

\item{sizes}{A 2-dimension vector of sample sizes}

\item{Q}{The threshold in graph construction}
}
\value{
A 1-dimension numeric value of test statistics
}
\description{
Evaluate Graph-based Test Statistics
}
\examples{
z<- matrix(rnorm(100),ncol=5)
dst<- L.p.distance(z,2)
GST(dst,1:20,c(10,10),2)
}
