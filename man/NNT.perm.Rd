% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/NNT.perm.R
\name{NNT.perm}
\alias{NNT.perm}
\title{Permutation Test for Nearest Neighbor Statistics}
\usage{
NNT.perm(z, sizes, R, alpha, B)
}
\arguments{
\item{z}{A matrix of data set (each row is an observation)}

\item{sizes}{A 2-dimension vector of sample sizes}

\item{R}{The number of neighborhood to be evaluated}

\item{alpha}{Significance level}

\item{B}{The number of replicate in permutation test}
}
\value{
A decision on whether the null hypothesis is rejected
}
\description{
Perform Permutation Test for Nearest Neighbor Test Statistics
}
\examples{
z<- matrix(rnorm(100),ncol=5)
NNT(z,1:20,c(10,10),3)
NNT.perm(z,c(10,10),3,0.05,10)
}
