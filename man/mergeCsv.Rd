\name{mergeCsv}
\alias{mergeCsv}
\title{Merge CSV Files}
\description{
  Merges multiple csv's in the current directory together, with the option of averaging over several. For instance, if you have broken a set of simulations each into five jobs, this will merge all groups of five simulations together.
}
\usage{
mergeCsv(every=1, outfile="allResults.csv", quote=FALSE)
}
\arguments{
  \item{every}{How many lines should be merged (averaged) over. This is especially useful when using the 'multiplier' open in the various batching routines (e.g. Rbatch, Rmosrun, etc.).}
  \item{outfile}{Name of the csv file to write the merged data to.}
  \item{quote}{Whether to quote each item.}
}
\seealso{
  \code{\link{parseCommandArgs}}, \code{\link{rbatch}}, \code{\link{msplit}}
}
\references{
  Thomas J. Hoffmann (2011). Passing in Command Line Arguments and
  Parallel Cluster/Multicore Batching in R with batch. Journal of
  Statistical Software, Code Snippets, 39(1), 1-11. URL
  \url{http://www.jstatsoft.org/v39/c01/}.
}
\keyword{interface}
