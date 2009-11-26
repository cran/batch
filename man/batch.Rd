\name{rbatch}
\alias{rbatch}
\alias{rbatch.default}
\alias{rbatch.local}
\alias{rbatch.local.run}
\alias{rbatch.lsf}
\alias{rbatch.mosix}
\title{R Batch Submission}
\description{
  Aids in the submission of multiple jobs to a cluster. Also can be used locally on a linux machine to utilize all cores (or processors), if the cluster is busy.
}
\usage{
rbatch(rfile, seed, ..., rbatch.control=rbatch.default())
rbatch.default()
rbatch.local(BATCH="ALLCORES", BATCHPOST="", QUOTE="",
             ARGQUOTE='"', RUN=1, MULTIPLIER=1)
rbatch.local.run()
rbatch.lsf(BATCH="bsub -q normal", BATCHPOST="", QUOTE='"',
           ARGQUOTE='""', RUN=1, MULTIPLIER=1)
rbatch.mosix(BATCH="nohup mosrun -e -b -q", BATCHPOST=" &", QUOTE="",
             ARGQUOTE='"', RUN=1, MULTIPLIER=1)
}
\arguments{
  \item{rfile}{Name of the R file that you wish to batch.}

  \item{seed}{What seed you wish to run. You will need to set this in your code, but it will be used to set the name of the output file.}
  \item{...}{Any other arguments you want to pass to your R file (to be parsed via parseCommandArgs() call, see that function for a detailed example.}
  \item{rbatch.control}{Object from rbatch.default(), rbatch.lsf() or rbatch.mosix(), controlling the cluster run. Suggested is to always use rbatch.default(), as is the default, and set the command line argument RBATCH to 'lsf' or 'mosix', or alter any of the parameters in the functions (e.g. BATCH, MULTIPLIER) through the command line arguments.}

  \item{BATCH}{Command string to use to batch the file on a cluster. The default is the normal queue for LSF, "bsub -q normal". Rmosrun sets this to "nohup mosrun -e -b -q" is used..}
  \item{BATCHPOST}{String that will be pasted to the end of the batch string (e.g. '&').}
  \item{QUOTE}{How to quote the command string. Default is for LSF.}
  \item{ARGQUOTE}{How to quote arguments when they are vectores, default is double quote to work with LSF, you might also find single quote to be useful.}
  \item{RUN}{Default is 0, in which case the commands will not be batched. Use this to first ensure you are really batching what you want to batch. Then set this to be 1.}
  \item{MULTIPLIER}{How many times to run the current set of arguments. For example, if you have a simulation that you want to run 1000 iterations on, you could set multiplier to 10, and run 100 iterations 10 times by setting multiplier to 10. Then you could use mergeCsv(10), e.g., if you wrote your results to a csv file as exemplified in the parseCommandArgs() routine. Automatically increments the seed.}
}
\details{
  Returns the next seed you can use (particularly useful when you set the multiplier argument).

  See the examples in \code{\link{parseCommandArgs}} for examples on how to run this.
}
\seealso{
  \code{\link{parseCommandArgs}}, \code{\link{mergeCsv}}, \code{\link{msplit}}
}
\keyword{interface}
