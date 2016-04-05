#' Copy \code{HexSimR} results
#' 
#' This function is used to copy/back up  \code{HexSimR} results maintaining the
#'  same directory structure.
#'   
#' \code{copy.results} will duplicate all folders present within the 'Results' 
#'   folder when \code{scen.results=TRUE}. Note that only the first level of 
#'   results will be copied (i.e. no individual replicate results are copied). 
#'   All .xlsx files present in scenarios' folders will be copied (even 
#'   those with manually modified names) assuming that these are results to keep. 
#'   These include genetic mean distances. 
#' 
#' It assummes that file names of plots start with "plot" as for   \code{HexSimR} 
#'   default. If exists, the file "gen.plot.data.csv" (if the name was not 
#'   modified) will be copied as part of this option because considered part of 
#'   the plot files.
#'   
#' With \code{comp.census}, \code{comp.move}, and \code{comp.ranges} can be set 
#'   whether to copy results of statistical comparisons (output from \code{SSMD_census},
#'   \code{SSMD_move}, \code{SSMD_ranges}).
#'   
#' The SSMD file generated with \code{Pext} is copied automatically (if exists 
#'    and if the name was not modified), along with the other results from this
#'    function, when the option \code{Pext=TRUE} is selected.
#'   
#' When \code{comp.census=TRUE}, all files whose name starts with "SSMD_census"
#'    will be copied (note that if some file names has been modified and does  
#'    not start with "SSMD_census" anymore, these files will not be copied).
#'    
#' @param out The directory where to save the results
#' @param copy.invasion Whether to copy data saved from \code{invasion.front}
#' @param inv.name The name of file with results from   \code{invasion.front}
#' @param Pext Whether to save results from \code{Pext}
#' @param comp.census Whether to copy data saved from \code{SSMD_census}
#' @param comp.move Whether to copy data saved from \code{SSMD_move}
#' @param move.name The name of file with results from   \code{SSMD_move}
#' @param comp.ranges Whether to copy data saved from \code{SSMD_ranges}
#' @param ranges.name The name of file with results from   \code{SSMD_ranges}
#' @param plots Whether to copy plots
#' @param scen.results Whether to copy results saved in scenarios' folders
#' @inheritParams collate.census
#' @export

copy.results  <-  function(path.results, out=getwd(), 
                         copy.invasion=TRUE, inv.name="Invasion.front.xlsx",
                         Pext=TRUE,  
                         comp.census=TRUE,
                         comp.move=TRUE, move.name="SSMD_move.xlsx", 
                         comp.ranges=TRUE, ranges.name="SSMD_ranges.xlsx",
                         plots=TRUE, scen.results=TRUE, scenarios="all") {
  
  #----------------------------------------------------------------------------#
  # Helper functions
  #----------------------------------------------------------------------------#
  
  copy.folders <- function(root, foldername) {
    d <- paste(root, foldername, sep="/")
    dir.create(path = d)
  }
  
  copy.scen.summaries <- function(path.results, out, scenarios) {
    suppressWarnings(if(scenarios == "all") 
      scenarios <- list.dirs(path=path.results, full.names=FALSE, recursive=FALSE))
    lapply(scenarios, copy.res, p="*.xlsx$", path.results, out)
  }
  
  copy.res <- function(scenario, p, path.results, out) {
    file.list <- list.files(path = paste(path.results, scenario, sep="/"),
                          pattern = p,
                          full.names = FALSE)
    file.copy(from=paste(path.results, scenario, file.list, sep="/"), 
              to=paste(out, scenario, file.list, sep="/"))
    if(file.exists(paste(path.results, scenario, "summary_move.csv", sep="/"))) {
       copy.files(path.results, 
                  fname=paste(scenario, "summary_move.csv", sep="/"), root=out)
    }
    
    if(file.exists(paste(path.results, scenario, "descriptive_ranges.csv", sep="/"))) {
      copy.files(path.results, 
                 fname=paste(scenario, "descriptive_ranges.csv", sep="/"), root=out)
    }
  }
  
  copy.files <- function(fname, path.results, root=out) {
    file.copy(from=paste(path.results, fname, sep="/"), 
              to=paste(root, fname, sep="/"))
  }
  #----------------------------------------------------------------------------#
  if(scen.results == TRUE) {
    foldernames <- list.dirs(path.results, full.names=FALSE, recursive=FALSE)
    lapply(foldernames, copy.folders, root=out)
    copy.scen.summaries(path.results, out=out, scenarios)
  }
  
  if(comp.census == TRUE) {
    copy.res(scenario=NULL, p="^SSMD_census", path.results, out)
  }
  
  fnames <- c(if(copy.invasion == TRUE) inv.name, 
              if(comp.move == TRUE) move.name,
              if(comp.ranges == TRUE) ranges.name)
  
  if(!is.null(fnames)) {
    lapply(fnames, copy.files, path.results)
  }
  
  if(plots == TRUE) {
    copy.res(scenario=NULL, p="^plot", path.results, out)
    if(file.exists(paste(path.results, "gen.plot.data.csv", sep="/"))) {
      copy.files(path.results, fname="gen.plot.data.csv", root=out)
      }
  }
  if(Pext == TRUE) {
    lapply(c("^Cumulative_Pext", "^Pext_census"), copy.res, scenario=NULL, 
           path.results=path.results, out=out)
    ssmd_Pext <- list.files(path = path.results,
                            pattern = "^SSMD_Cumul_Pext",
                            full.names = FALSE)
    if(length(ssmd_Pext) > 0) {
      lapply(ssmd_Pext, copy.files, path.results, root=out)
    }
  }
}

