rbatch.default <- function(){
  res <- parseCommandArgs(FALSE) #DF()
  #print(res)
  if(is.null(res$RBATCH)){
    return(rbatch.local())
  }else{
    res$RBATCH <- tolower(res$RBATCH)
    if(res$RBATCH == "local")
      return(rbatch.local())
    if(res$RBATCH == "lsf")
      return(rbatch.lsf())
    else if(res$RBATCH == "mosix")
      return(rbatch.mosix())
  }
  stop(paste("rbatch.default error, you supplied argument RBATCH='", res$RBATCH, "', but it should be one of 'local', 'lsf', or 'mosix' (case insensitive).", sep=""))
}

rbatch.parseCommandArgs <- function(BATCH, BATCHPOST, QUOTE, ARGQUOTE, RUN, MULTIPLIER, LOCAL=0){
  RES <- parseCommandArgs(FALSE) #DF()
  #cat("rbatch.parseCommandArgs, RES=\n")
  #print(RES)
  #print(RES$RUN)

  if(!is.null(RES$BATCH))
    BATCH <- RES$BATCH
  if(!is.null(RES$BATCHPOST))
    BATCHPOST <- RES$BATCHPOST
  if(!is.null(RES$QUOTE))
    QUOTE <- RES$QUOTE
  if(!is.null(RES$ARGQUOTE))
    BATCHQUOTE <- RES$ARGQUOTE
  if(!is.null(RES$RUN)){
    #print("isn't null")
    #print(RES$RUN)
    RUN <- RES$RUN
    #print(RUN)
  }
  if(!is.null(RES$MULTIPLIER))
    MULTIPLIER <- RES$MULTIPLIER
  if(!is.null(RES$LOCAL))
    LOCAL <- RES$LOCAL

  #cat("rbatch.parseCommandArgs, RES$RUN=", RES$RUN, "\n")
  #cat("rbatch.parseCommandArgs, RUN=", RUN, "\n")

  return(c(BATCH=BATCH, BATCHPOST=BATCHPOST, QUOTE=QUOTE, ARGQUOTE=ARGQUOTE, RUN=RUN, MULTIPLIER=MULTIPLIER, LOCAL=LOCAL))
}

rbatch.local <- function(BATCH="ALLCORES", BATCHPOST="", QUOTE="", ARGQUOTE='"', RUN=1, MULTIPLIER=1){
  ncores <- 1
  if(is.character(BATCH)){
    if(BATCH == "ALLCORES")
      ncores <- multicore:::detectCores()
  }else
    ncores <- as.integer(BATCH)
  BATCH <- ""
  ## NOTE: IDEA: We could use the multicore package as a massive copout... hell yes! let's do it!!!
  return(rbatch.parseCommandArgs(BATCH=BATCH, BATCHPOST=BATCHPOST, QUOTE=QUOTE, ARGQUOTE=ARGQUOTE, RUN=RUN, MULTIPLIER=MULTIPLIER, LOCAL=ncores))
}

rbatch.lsf <- function(BATCH="bsub -q normal", BATCHPOST="", QUOTE='"', ARGQUOTE='""', RUN=1, MULTIPLIER=1){
  return(rbatch.parseCommandArgs(BATCH=BATCH, BATCHPOST=BATCHPOST, QUOTE=QUOTE, ARGQUOTE=ARGQUOTE, RUN=RUN, MULTIPLIER=MULTIPLIER))
}

rbatch.mosix <- function(BATCH="nohup mosrun -e -b -q", BATCHPOST=" &", QUOTE="", ARGQUOTE='"', RUN=1, MULTIPLIER=1){
  return(rbatch.parseCommandArgs(BATCH=BATCH, BATCHPOST=BATCHPOST, QUOTE=QUOTE, ARGQUOTE=ARGQUOTE, RUN=RUN, MULTIPLIER=MULTIPLIER))
}


####################################################################
# SET UP THE GLOBAL VARIABLES INTERFACE                            #
####################################################################
rbatch._env <- new.env();
rbatch._env.set <- function( x, value )
  assign( x, value, envir=rbatch._env );
rbatch._env.get <- function( x, mode="any" )
  get( x, envir=rbatch._env, mode=mode, inherits=FALSE );
rbatch._env.set("rbatch.local._queue", c())
rbatch._env.set("rbatch.local._numcores", 1)
## INTERNAL
rbatch.local.pushback <- function(cmdstr, cores){
  #print("rbatch.local")
  #print(cmdstr)
  rbatch._env.set("rbatch.local._queue", c(rbatch._env.get("rbatch.local._queue"), cmdstr))
  rbatch._env.set("rbatch.local._numcores", cores)
  return(invisible())
}

## EXPORTED, forks everything off!
rbatch.local.run <- function(){
  cmdstrs <- rbatch._env.get("rbatch.local._queue")

  ## NOW USE multicore, and go ahead and batch them all off!!!
  ncores <- rbatch._env.get("rbatch.local._numcores")
  cat("Local, ncores=", ncores, ".\n", sep="")

  #print(cmdstrs) ## Debug
  require(multicore)
    mclapply(cmdstrs, function(i){system(i); cat(i,"completed.\n");})
  #cat("RAN")

  return(invisible())
}

rbatch <- function(rfile, seed, ..., rbatch.control=rbatch.default()){
  call <- match.call(expand.dots=TRUE)
  #syscall <- sys.call(sys.parent())

  ##print( batch )
  #print(rbatch.control)
  #stop()

  ## turn an obj into an R expression
  ## currently, only works with vectors, etc.
  pasted <- function( obj, argquote ) {
    #print( eval(obj) )
    #obj <- eval(obj, parent.frame()) ## done externally now..

    if( length(obj) == 1 )
      return( as.character(obj) )

    ret <- paste( argquote, 'c(', sep="" )
    for( i in 1:length(obj) ) {
      ret <- paste(ret,obj[i],sep="")
      if( i != length(obj) )
        ret <- paste(ret,",",sep="")
    }
    ret <- paste(ret,')',argquote,sep="")
    return( ret )
  }

  ## first member of call is 'bsub'
  ## second member is 'rfile', with the name of the R file to run (but no need to use)
  ## 'seed' might as well be treated as any other argument, so there is really no need to mess with anything else here...

  for( m in 1:rbatch.control["MULTIPLIER"] ) {
    argstr <- ""
    for( i in 3:length(call) ) {
      callname <- names(call)[i]
      if( callname!="BATCH" && callname!="QUOTE" && callname!="ARGQUOTE" && callname!="RUN" && callname!="MULTIPLIER" && callname!="BATCHPOST" ){
        #wh <- which(callname == names(syscall))
        #if(length(wh) > 0){
        #  ## here's the funky addition
        #  argstr <- paste(argstr, names(call)[i], pasted(syscall[[wh]], argquote=rbatch.control["ARGQUOTE"]))
        #}else{
        #  argstr <- paste(argstr, names(call)[i], pasted(call[[i]], argquote=rbatch.control["ARGQUOTE"]))
        #}
        #print(eval(call[[i]], parent.frame()))
        evalCalli <- eval(call[[i]], parent.frame())
        if(!is.null(evalCalli))
          argstr <- paste(argstr, names(call)[i], pasted(evalCalli, argquote=rbatch.control["ARGQUOTE"]))
      }
    }
    ##cat( argstr, "\n" )

    cmdstr <- paste( rbatch.control["BATCH"], " ", rbatch.control["QUOTE"], "R --vanilla --args ", argstr, " < ", rfile, " > ", rfile, "out", seed, rbatch.control["QUOTE"], rbatch.control["BATCHPOST"], sep="" )
    cat( cmdstr, "\n" )

    #cat("LOCAL", rbatch.control["LOCAL"], "\n") ## DEBUG ONLY
    #cat("RUN", rbatch.control["RUN"], "\n")
    if(rbatch.control["RUN"] == 1){
      #print("inside the local...")
      if(rbatch.control["LOCAL"] == 0){
        system(cmdstr)
      }else{
        rbatch.local.pushback(cmdstr, rbatch.control["LOCAL"])
      }
    }

    seed <- seed + 1
    call[which(names(call) == "seed")] <- seed
  }

  return( seed )
}


## DEBUG
#source("parseCommandArgs.R")
#print(rbatch.default())

#rbatch("test.R", seed=10, test="hello", foo="bar")
#rbatch("test.R", seed=11, a="bye")
#rbatch.local.run()
