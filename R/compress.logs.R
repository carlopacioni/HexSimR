#' Compress log files and optionally remove them
#'
#' Compress log files and optionally remove them to save space and make
#' simulation results more portable
#'
#' From (very limited) testing, a compression of 1 gives substantial compression
#' (about 23\% of original size) and it is very fast. \code{compression=7} gives
#' almost the max compression (~18.5\% of the original size) taking ~ 4 times
#' longer than \code{compression=1}. Anything beyon 7 takes much longer but with
#' a gain of only a 0.1\% or less.
#'
#' Note that it makes sense to execute this function in parallel only if a solid
#' state disk (SSD) is available. If \code{ncores} is left to its default
#' (\code{NULL}) and \code{parallel=TRUE} all available processors will be used.
#'
#' @param delete.log whether the log file should be deleted
#' @param overwrite.gz if a .gz file with the same name is present, should it be
#'   overwritten?
#' @param compression compression rate, integer between 0 and 9. See
#'   \code{\link[base]{gzfile}}
#' @param parallel whether execute the function in parallel
#' @param ncores the number of cores if executed in parallel
#' @inheritParams collate.census
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
#' compress.logs(path.results=tmp, scenarios="test", delete.log=TRUE, overwrite.gz=TRUE)
#' # clean up
#' unlink(tmp, recursive=TRUE)

compress.logs <- function(path.results=NULL, scenarios="all", delete.log=TRUE, 
                          overwrite.gz=TRUE, compression=7, parallel=FALSE, ncores=NULL) {
  

  #--------------------------------------------------------------------------#

txt <- "Please, select the 'Results' folder within the workspace"
if(is.null(path.results)) path.results <- tk_choose.dir(caption=txt)
if(length(scenarios) == 1) {
  if(scenarios == "all") 
    scenarios <- list.dirs(path=path.results, full.names=FALSE, recursive=FALSE)
}

l.logs <- list.files(path=file.path(path.results, scenarios), pattern=".log$", 
                     full.names=TRUE, recursive=TRUE)
gz.names <- sub(pattern=".log$", replacement=".gz", x= l.logs) 

message("Starting compressing log files")

if(parallel) {
  message("Parallel execution...")
  if(is.null(ncores)) ncores <- parallel::detectCores() 
  if(length(l.logs) %% ncores != 0) 
    stop(paste0("The number of files detected (", length(l.logs), ") ",
               "is not a multiple of the number of cores selected (", ncores, ")\n", 
               "This will cause problem in the parallel execution\n",
               "Please, adjust the number of cores or use parallel=FALSE"))
  cl <- parallel::makeCluster(ncores)
  on.exit(parallel::stopCluster(cl))
  parallel::clusterEvalQ(cl, library("R.utils"))
  parallel::clusterExport(cl, 
                          varlist=c("l.logs", "gz.names", "compression",
                                    "delete.log", "overwrite.gz"), 
                          envir=environment()) 
  t <-  system.time(
    parallel::clusterMap(cl, R.utils::gzip, l.logs,  destname=gz.names, 
                         MoreArgs=list(compression=compression,  remove=delete.log, 
                                       FUN=gzfile, ext="gz", overwrite=overwrite.gz))
  )
  
} else {
  t <- system.time(
    mapply(R.utils::gzip, l.logs,  destname=gz.names, 
           MoreArgs=list(compression=compression,  remove=delete.log, FUN=gzfile, 
                         ext="gz", overwrite=overwrite.gz))
  )
}

message("Done!")
message(paste("Elapsed time in secs", t[3]))
return(list(logs=l.logs, gzs=gz.names))
}


#' Decompress files with ".gz" extension, append ".log" extension and optinally
#' remove the .gz files
#'
#' Decompress files with ".gz" extension, append ".log" extension and optinally
#' remove the .gz files, which may be necessay if reports need to be created
#'
#' Note that it makes sense to execute this function in parallel only if a solid
#' state disk (SSD) is available
#' 
#' @param delete.gz whether the .gz file should be deleted after decompression
#' @param overwrite.log if a .log file with the same name is present, should it be
#'   overwritten?
#' @inheritParams collate.census
#' @inheritParams compress.logs
#' @return a list where the first element is the list of gz files found and the
#'   second is the log files created

#' @importFrom R.utils gzip
#' @export

decompress.gz <- function(path.results=NULL, scenarios="all", delete.gz=TRUE, 
                          overwrite.log=TRUE, parallel=FALSE, ncores=NULL) {
  
  
  #--------------------------------------------------------------------------#
  
  txt <- "Please, select the 'Results' folder within the workspace"
  if(is.null(path.results)) path.results <- tk_choose.dir(caption=txt)
  if(length(scenarios) == 1) {
    if(scenarios == "all") 
      scenarios <- list.dirs(path=path.results, full.names=FALSE, recursive=FALSE)
  }
  
  l.gz <- list.files(path=file.path(path.results, scenarios), pattern=".gz$", 
                       full.names=TRUE, recursive=TRUE)
  log.names <- sub(pattern=".gz$", replacement=".log", x= l.gz) 
  
  message("Starting decompressing gz files...")
  if(parallel) {
    message("Parallel execution...")
    if(is.null(ncores)) ncores <- parallel::detectCores() 
    if(length(l.gz) %% ncores != 0) 
      stop(paste0("The number of files detected (", length(l.gz), ") ",
                  "is not a multiple of the number of cores selected (", ncores, ")\n", 
                  "This will cause problem in the parallel execution\n",
                  "Please, adjust the number of cores or use parallel=FALSE"))
    cl <- parallel::makeCluster(ncores)
    on.exit(parallel::stopCluster(cl))
    parallel::clusterEvalQ(cl, library("R.utils"))
    parallel::clusterExport(cl, 
                  varlist=c("l.gz", "log.names", "delete.gz", "overwrite.log"), 
                  envir=environment()) 
    t <-  system.time(
      parallel::clusterMap(cl, R.utils::gunzip, l.gz,  destname=log.names, 
                           MoreArgs=list(remove=delete.gz, FUN=gzfile, 
                                         ext="gz", overwrite=overwrite.log))
    )
    
  } else {
    t <- system.time(
      mapply(R.utils::gunzip, l.gz,  destname=log.names, 
             MoreArgs=list(remove=delete.gz, FUN=gzfile, 
                           ext="gz", overwrite=overwrite.log))
    )
  }
  
  message("Done!")
  message(paste("Elapsed time in secs", t[3]))
  return(list(logs=log.names, gzs=l.gz))
}

