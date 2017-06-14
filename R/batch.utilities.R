
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
  txt <- "Please, select the 'Results' folder within the workspace"
  if(is.null(path.results)) path.results <- choose.dir(caption = txt)
  suppressWarnings(if(scenarios == "all") 
    scenarios <- list.dirs(path=path.results, full.names=FALSE, recursive=FALSE))
  if(is.null(dir.out)) dir.out <- getwd()
  l.coms <- lapply(scenarios, com.scenario, path.results)
   
  writeLines(unlist(l.coms), con=paste0(dir.out, "/", "comb_log_files.bat")) 
}

#' Write a .xml file to be passed to HexSim to queue 'movements' and/or 'ranges'
#'  reports
#' 
#' Generate a batch .xml file in the workspace directory that will instruct 
#'   OutputTransformer.exe to generate 'movements' and/or 'ranges' 
#'   report. 
#'   
#' It assumes that a combined log file have been generated from all replicates 
#'   for the given scenarios.
#'   
#' If \code{scenarios="all"}, then all the scenarios will be included.
#' 
#' @param ranges Whether a batch file to generate 'ranges' reports should be created
#' @param move Whether a batch file to generate 'movement' reports should be created
#' @inheritParams collate.census 
#' @return A .xml file named batchFile_Reports.xml
#' @export
report.batch <- function(path.results=NULL, scenarios="all", ranges=TRUE, 
                         move=TRUE) {
  #----------------------------------------------------------------------------#
  # Helper functions
  #----------------------------------------------------------------------------#
  
  report.block <- function(scenario, path.results, report, suf="-[all].log") {
    open.args <- "  <args>" 
    open.arg <- "    <arg>"
    close.arg <- "</arg>"
    close.args <- "  </args>"
    arg1 <- paste0("-", report)
    log.file <- paste0(scenario, suf)
    arg2 <- paste(path.results, scenario, log.file, sep="\\")
    block <- c(open.args, 
               paste0(open.arg, arg1, close.arg),
               paste0(open.arg, arg2, close.arg),
               close.args)
    return(block)
  }
  
  w.report.batch <- function(path.results, scenarios, report, suf="-[all].log") {
    fl <- "<?xml version=\"1.0\"?>"
    open.block <- "<OutputTransform>"
    close.block <- "</OutputTransform>"
    report.blocks <- lapply(scenarios, report.block, path.results, report, 
                            suf="-[all].log")
    wspace <- sub(pattern = "Results", replacement = "", path.results)
    writeLines(c(fl, open.block, unlist(report.blocks), close.block),
               con=paste0(wspace, paste0("batchFile_", report, "_Reports.xml"))) 
    
  }
  
  #----------------------------------------------------------------------------#
  txt <- "Please, select the 'Results' folder within the workspace"
  if(is.null(path.results)) path.results <- choose.dir(caption = txt)
  suppressWarnings(if(scenarios == "all") 
    scenarios <- list.dirs(path=path.results, full.names=FALSE, recursive=FALSE))
  
  if(ranges == TRUE) w.report.batch(path.results, scenarios, report="ranges", 
                                  suf="-[all].log")
  
  if(move == TRUE) w.report.batch(path.results, scenarios, report="movement", 
                                  suf="-[all].log")
}