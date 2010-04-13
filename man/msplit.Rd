\name{msplit}
\alias{msplit}
\title{Split Vectors for Parallelization}
\description{
  Aids in splitting of a vector for parallelization, e.g. splitting up a group of SNPs into subgroups, so each one can analyzed in a parallel process. Other uses might be to choose splitting points for k-fold cross validation.
}
\usage{
msplit(vec, m)
}
\arguments{
  \item{vec}{A vector, e.g. c(1,2,3), 1:10.}
  \item{m}{Numer of splits.}
}
\details{
  Returns a list, with each member being a subgroup to be parallelized.
}
\seealso{
  \code{\link{parseCommandArgs}}, \code{\link{rbatch}}, \code{\link{mergeCsv}}
}
\examples{
  snps <- paste("snp", 1:98, sep="")
  print(snps)
  print(msplit(snps, 10)) ## Splits it into 10 groups
}
\references{
  Thomas J. Hoffmann (2011). Passing in Command Line Arguments and
  Parallel Cluster/Multicore Batching in R with batch. Journal of
  Statistical Software, Code Snippets, 39(1), 1-11. URL
  \url{http://www.jstatsoft.org/v39/c01/}.
}
\keyword{interface}
