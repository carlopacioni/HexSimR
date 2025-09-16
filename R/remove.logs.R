#' Remove log files 
#'
#' Remove log files (compressed or as .log) to save space and make
#' simulation results more portable
#'
#' This function search for files ending with '.log' or '.gz' in the path provided
#' and remove them. Care should be taken if there are other files in the directory
#' that are not the target files.
#'
#' @param delete.gz whether compressed files with extension .gz should be deleted
#' @inheritParams collate.census
#' @inheritParams compress.logs
#' @return a list where the first element is the list of log files and the
#'   second is the gz files found and deleted
#' @importFrom R.utils gzip
#' @export
#' @examples
#' # Create a temp directory
#' tmp <- tempdir()
#' dir.create(file.path(tmp, "test"))
#' # Create a fake log file
#' cat(file=file.path(tmp, "test", "foo.log"), "Hello world!")
#' # Remove the log file
#' remove.logs(path.results=tmp, scenarios="test", delete.log=TRUE)
#' # clean up
#' unlink(tmp, recursive=TRUE)

remove.logs <- function(path.results=NULL, scenarios="all", delete.log=TRUE, 
                        delete.gz=TRUE) {
  
  
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
  
  return(lf)
}


