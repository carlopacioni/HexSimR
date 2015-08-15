
#' Write a .bat file to generate combined log files in batch mode for all 
#'  selected scenarios
#' 
#' Write a .bat file to generate combined log files in batch mode for all 
#'   selected scenarios. When \code{scenarios="all"} all scenarios listed in the 
#'   'Results' folder within the workspace will be included in the batch file.
#'   
#' The file is saved in the path indicated by \code{dir.out}, typically where 
#'   HexSimCommandLine.exe is located. If NULL (default), 
#'   the working directory is used. This may be useful when the batch file is 
#'   being prepared on a different machine. Ultimately, the batch file needs to 
#'   be copied where the HexSimCommandLine.exe is located.
#'   
#' To run the batch file, open the command prompt, navigate where
#'   HexSimCommandLine.exe is located and type "comb_log_files.bat".
#'   
#' @param dir.out Path where to save the output. If NULL (default), the working 
#'   directory is used 
#' @inheritParams collate.census 
#' @return A .bat file named comb_log_files.bat
#' @export
w.combine.log.batch <- function(path.results=NULL, scenarios="all", dir.out=NULL) {
  #----------------------------------------------------------------------------#
  # Helper functions
  #----------------------------------------------------------------------------#
  
  com.scenario <- function(scen, path.results) {
    com <- paste("HexSimCommandLine.exe -combineLogFiles", 
                 paste0("\"", path.results, "/", scen, "\""))
    return(com)
  }
  
  #----------------------------------------------------------------------------#
  
  if(is.null(path.results)) path.results <- choose.dir(caption = txt)
  suppressWarnings(if(scenarios == "all") 
    scenarios <- list.dirs(path=path.results, full.names=FALSE, recursive=FALSE))
  if(is.null(dir.out)) dir.out <- getwd()
  l.coms <- lapply(scenarios, com.scenario, path.results)
   
  writeLines(unlist(l.coms), con=paste0(dir.out, "/", "comb_log_files.bat")) 
}