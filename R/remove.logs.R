#' Remove log files 
#'
#' Remove log files (compressed or as .log) to save space and make
#' simulation results more portable
#'
#' This function search for files ending with '.log' or '.gz' in the path provided
#' and remove them. Care should be taken if there are other files in the directory
#' that are not the target files.
#'
#' Note that it makes sense to execute this function in parallel only if a solid
#' state disk (SSD) is available. If \code{ncores} is left to its default
#' (\code{NULL}) and \code{parallel=TRUE} all available processors will be used.
#'
#' @inheritParams collate.census
#' @inheritParams compress.logs
#' @return a list where the first element is the list of log files found and the
#'   second is the gz files created
#' @importFrom R.utils gzip
#' @export
#' @examples
#' # Create a temp directory
#' tmp <- tempdir()
#' dir.create(file.path(tmp, "test"))
#' # Create a fake log file
#' cat(file=file.path(tmp, "test", "foo.log"), "Hello world!")
#' # Compress the log file
#' remove.logs(path.results=tmp, scenarios="test", delete.log=TRUE)
#' # clean up
#' unlink(tmp, recursive=TRUE)

remove.logs <- function(path.results=NULL, scenarios="all", delete.log=TRUE, 
                        delete.gz=TRUE, parallel=FALSE, ncores=NULL) {
  
  
  #--------------------------------------------------------------------------#
  
  txt <- "Please, select the 'Results' folder within the workspace"
  if(is.null(path.results)) path.results <- tk_choose.dir(caption=txt)
  if(length(scenarios) == 1) {
    if(scenarios == "all") 
      scenarios <- list.dirs(path=path.results, full.names=FALSE, recursive=FALSE)
  }
  lf <- list(logs=delete.log, gz=delete.gz)
  
  if(delete.log) {
  l.logs <- list.files(path=file.path(path.results, scenarios), pattern=".log$", 
                       full.names=TRUE, recursive=TRUE)
  lf[[1]] <- l.logs
  if(length(l.logs)==0) {
    warning("No log files found") 
  } else {
    file.remove(l.logs)
  }
  }
  
  if(delete.gz) {
    l.gz <- list.files(path=file.path(path.results, scenarios), pattern=".gz$", 
                       full.names=TRUE, recursive=TRUE)
    lf[[2]] <- l.gz
    if(length(l.gz)==0) {
      warning("No gz files found") 
    } else {
      file.remove(l.gz)
    }
  }
  
  # if(parallel) {
  #   message("Parallel execution...")
  #   if(is.null(ncores)) ncores <- parallel::detectCores() 
  #   if(length(l.logs) %% ncores != 0) 
  #     stop(paste0("The number of files detected (", length(l.logs), ") ",
  #                 "is not a multiple of the number of cores selected (", ncores, ")\n", 
  #                 "This will cause problem in the parallel execution\n",
  #                 "Please, adjust the number of cores or use parallel=FALSE"))
  #   cl <- parallel::makeCluster(ncores)
  #   on.exit(parallel::stopCluster(cl))
  #   parallel::clusterEvalQ(cl, library("R.utils"))
  #   parallel::clusterExport(cl, 
  #                           varlist=c("l.logs", "gz.names", "compression",
  #                                     "delete.log", "overwrite.gz"), 
  #                           envir=environment()) 
  #   t <-  system.time(
  #     parallel::clusterMap(cl, R.utils::gzip, l.logs,  destname=gz.names, 
  #                          MoreArgs=list(compression=compression,  remove=delete.log, 
  #                                        FUN=gzfile, ext="gz", overwrite=overwrite.gz))
  #   )
  #   
  # } else {
  #   t <- system.time(
  #     mapply(R.utils::gzip, l.logs,  destname=gz.names, 
  #            MoreArgs=list(compression=compression,  remove=delete.log, FUN=gzfile, 
  #                          ext="gz", overwrite=overwrite.gz))
  #   )
  # }
  # 
  # message("Done!")
  # message(paste("Elapsed time in secs", t[3]))
  return(lf)
}


