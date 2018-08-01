#' Compress log file and optinally remove them
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
#' @return print to screen log files' full names and respective .gz
#' @importFrom R.utils gzip
#' @export

compress.logs <- function(path.results=NULL, scenarios="all", delete.log=TRUE, 
                          overwrite.gz=TRUE, compression=7) {
  

  #--------------------------------------------------------------------------#

txt <- "Please, select the 'Results' folder within the workspace"
if(is.null(path.results)) path.results <- tk_choose.dir(caption = txt)
suppressWarnings(if(scenarios == "all") 
  scenarios <- list.dirs(path=path.results, full.names=FALSE, recursive=FALSE))

l.logs <- list.files(path = file.path(path.results, scenarios), pattern = ".log$", 
                     full.names = TRUE, recursive = TRUE)
gz.names <- sub(pattern = ".log$", replacement = ".gz", x =  l.logs) 

message("Starting compressing the log files")
t <- system.time(
  mapply(R.utils::gzip, l.logs,  destname=gz.names, 
       MoreArgs=list(compression=compression,  remove=delete.log, FUN=gzfile, 
                     ext="gz", overwrite=overwrite.gz))
)
message("Done!")
message(paste("Elapsed time", t[3]))
}

