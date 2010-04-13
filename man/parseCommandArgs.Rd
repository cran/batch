\name{parseCommandArgs}
\alias{parseCommandArgs}
\title{Parse Command-Line Arguments}
\description{
  \code{parseCommandArgs} allows for command line arguments to be passed into R. Arguments may be of the form of simple R objects. This makes running the same R code on multiple different options easy, and possible to run in parallel on a single machine or on a cluster.

  \code{parseCommandArgsDF} returns a dataframe with all of the values that were set when the code was executed.
}
\usage{
parseCommandArgs(evaluate=TRUE)
}
\arguments{
  \item{evaluate}{If TRUE, then the command-line arguments are assigned to the current namespace, over-riding any default values that may have already been set in software.}
}
\details{
Returns a list of the command-line arguments that were set.

See the example below for a good example of how to use this function, and how to run things in parallel with it.
}
\seealso{
  \code{\link{rbatch}}, \code{\link{mergeCsv}}, \code{\link{msplit}}
}
\examples{
\dontrun{
  ## mainSim.R
  ## Put the following code in the file 'mainSim.R'.
  ##
  ## Try this out by running:
  ##   R --vanilla < mainSim.R > mainSim.Rout1013
  ##   R --vanilla --args seed 1014 bbeta 0 < mainSim.R > mainSim.Rout1014
  ##   R --vanilla --args seed 1015 bbeta "c(10,20)" < mainSim.R > mainSim.Rout1015
  library(batch)

  ## Set values of some parameters
  seed <- 1013 ## default value
  bbeta <- 5   ## default value, note 'beta' is an R function, so we can't use that

  ## Overwrite the values of 'seed' and 'bbeta', e.g., if they have been
  ##  passed in from the command prompt.
  parseCommandArgs()

  ## Will disply the default values on the first run,
  ##  but bbeta=1014 and bbeta=0 on the second run.
  print(seed)
  print(bbeta)

  ## ... your simualtion code

  ## Write out your results to a csv file
  write.csv(data.frame(seed=seed, bbeta=paste(bbeta,collapse="~")),
    paste("res",seed,".csv",sep=""), row.names=FALSE)

  ## R.miSniam
}

\dontrun{
  ## run_mainSim_parallel.R
  ## Put the following code in 'run_mainSim_parallel.R'
  ##
  ## Selects a variety of parameter combinations to run
  ##    mainSim.R in parallel on a cluster.
  ##
  ## First see the commands that would be run (to make sure they are correct) with
  ##   R --vanilla --args RUN 0 < run_mainSim_parallel.R
  ##  Then run the commands with
  ##   R --vanilla < run_mainSim_parallel.R
  ##  or
  ##   R --vanilla --args RUN 1 < run_mainSim_parallel.R
  ##  These will all default to run locally.
  ## To run on a mosix cluster, run with
  ##   R --vanilla --args RUN 1 RBATCH mosix < run_mainSim_parallel.R
  ## And on a LSF cluster, run with
  ##   R --vanilla --args RUN 1 RBATCH lsf < run_mainSim_parallel.R
  library(batch)

  parseCommandArgs() ## for overwriting default values; here, 'run'

  ## Choose a high enough seed for later for pasting the results together
  ##  (1,...,9,10) sorts not the way you want, for example.
  seed <- 1000
  for(i in 1:10)
    seed <- rbatch("mainSim.R", seed=seed, bbeta=i)

  ## Only for local (but it does not hurt to run in other situations,
  ##  so suggested in all cases).
  ## This actually runs all the commands when run on the local system.
  rbatch.local.run()
  ## R.lellarap_miSniam_nur
}
\dontrun{
  ## paste_mainSim_results.R
  ## Put the following code in paste_mainSim_results.R (or just
  ##  type them in), and run
  ##   R --vanilla < paste_mainSim_results.R
  ##
  ## Pastes all of the csv files created in 'run_mainSim_parallel'
  ##  together.
  library(batch)
  mergeCsv()
}
}
\references{
  Thomas J. Hoffmann (2011). Passing in Command Line Arguments and
  Parallel Cluster/Multicore Batching in R with batch. Journal of
  Statistical Software, Code Snippets, 39(1), 1-11. URL
  \url{http://www.jstatsoft.org/v39/c01/}.
}
\keyword{interface}

