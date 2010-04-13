\name{mergeCsv}
\alias{mergeCsv}
\title{Merge CSV Files}
\description{
  Merges multiple csv's in the current directory together, with the option of averaging over several. For instance, if you have broken a set of simulations each into five jobs, this will merge all groups of five simulations together.
}
\usage{
mergeCsv(every=1, outfile="allResults.csv")
}
\arguments{
  \item{every}{How many lines should be merged (averaged) over. This is especially useful when using the 'multiplier' open in the various batching routines (e.g. Rbatch, Rmosrun, etc.).}
  \item{outfile}{Name of the csv file to write the merged data to.}
}
\seealso{
  \code{\link{parseCommandArgs}}, \code{\link{rbatch}}, \code{\link{msplit}}
}
\keyword{interface}
