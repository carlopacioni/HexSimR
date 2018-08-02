#' Compress log files and optinally remove them
#'
#' Compress log file and optinally remove them to save space and make
#' simulations results more portable
#'
#' From (very limited) testing, a compression of 1 gives substantial compression
#' (about 23% of original size) and it is very fast. \code{compression=7} gives
#' almost the max compression (~18.5% of the original size) taking ~ 4 times
#' longer than \code{compression=1}. Anything beyon 7 takes much longer but with
#' a gain of only a 0.1% or less.
#'
#' @param delete.log whether the log file should be deleted
#' @param overwrite.gz if a .gz file with the same name is present, should it be
#'   overwritten?
#' @param compression compression rate, integer between 0 and 9. See
#'   \code{\link[base]{gzfile}}
#' @inheritParams collate.census
#' @return a list where the first element is the list of log files found and the
#'   second is the gz files created
#' @importFrom R.utils gzip
#' @export

compress.logs <- function(path.results=NULL, scenarios="all", delete.log=TRUE, 
                          overwrite.gz=TRUE, compression=7) {
  

  #--------------------------------------------------------------------------#

txt <- "Please, select the 'Results' folder within the workspace"
if(is.null(path.results)) path.results <- tk_choose.dir(caption=txt)
suppressWarnings(if(scenarios == "all") 
  scenarios <- list.dirs(path=path.results, full.names=FALSE, recursive=FALSE))

l.logs <- list.files(path=file.path(path.results, scenarios), pattern=".log$", 
                     full.names=TRUE, recursive=TRUE)
gz.names <- sub(pattern=".log$", replacement=".gz", x= l.logs) 

message("Starting compressing log files")
t <- system.time(
  mapply(R.utils::gzip, l.logs,  destname=gz.names, 
       MoreArgs=list(compression=compression,  remove=delete.log, FUN=gzfile, 
                     ext="gz", overwrite=overwrite.gz))
)
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
#' @param delete.gz whether the .gz file should be deleted after decompression
#' @param overwrite.log if a .log file with the same name is present, should it be
#'   overwritten?
#' @inheritParams collate.census
#' @return a list where the first element is the list of gz files found and the
#'   second is the log files created
#' @importFrom R.utils gzip
#' @export

decompress.gz <- function(path.results=NULL, scenarios="all", delete.gz=TRUE, 
                          overwrite.log=TRUE) {
  
  
  #--------------------------------------------------------------------------#
  
  txt <- "Please, select the 'Results' folder within the workspace"
  if(is.null(path.results)) path.results <- tk_choose.dir(caption=txt)
  suppressWarnings(if(scenarios == "all") 
    scenarios <- list.dirs(path=path.results, full.names=FALSE, recursive=FALSE))
  
  l.gz <- list.files(path=file.path(path.results, scenarios), pattern=".gz$", 
                       full.names=TRUE, recursive=TRUE)
  log.names <- sub(pattern=".gz$", replacement=".log", x= l.gz) 
  
  message("Starting decompressing gz files")
  t <- system.time(
    mapply(R.utils::gunzip, l.gz,  destname=log.names, 
           MoreArgs=list(remove=delete.gz, FUN=gzfile, 
                         ext="gz", overwrite=overwrite.log))
  )
  message("Done!")
  message(paste("Elapsed time in secs", t[3]))
  return(list(logs=l.logs, gzs=gz.names))
}

