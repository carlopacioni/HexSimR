#' Copy \code{HexSimR} results
#' 
#' This function is used to copy/back up  \code{HexSimR} results maintaining the
#'  same directory structure.
#'   
#' \code{copy.results} will duplicate all folders present within the 'Results' folder.
#' 
#' It assummes that file names of plots start with "plot" as for   \code{HexSimR} 
#'   default. All .xlsx files present in scenarios' folders will be copied (even 
#'   those with manually modified names) assuming that these are results to keep. 
#'   
#' With \code{comp.census}, \code{comp.move}, and \code{comp.ranges} can be set 
#'   whether to copy results of statistical comparisons (output from \code{SSMD_census},
#'   \code{SSMD_move}, \code{SSMD_ranges}).
#'   
#' When \code{comp.census=TRUE}, all files whose name starts with "SSMD_census"
#'    will be copied (note that if some file names has been modified and does  
#'    not start with "SSMD_census" anymore, these files will not be copied).
#'    
#' @param out The directory where to save the results
#' @param copy.invasion Whether to copy data saved from \code{invasion.front}
#' @param inv.name The name of file with results from   \code{invasion.front}
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
    if(exists(paste(path.results, scenario, "summary_move.csv", sep="/"))) {
       copy.files(path.results, 
                  fname=paste(scenario, "summary_move.csv", sep="/"), root=out)
    }
    
    if(exists(paste(path.results, scenario, "descriptive_ranges.csv", sep="/"))) {
      copy.files(path.results, 
                 fname=paste(scenario, "descriptive_ranges.csv", sep="/"), root=out)
    }
  }
  
  copy.files <- function(path.results, fname, root=out) {
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
    lapply(fnames, copy.files, path.results, root=out)
  }
  
  if(plots == TRUE) copy.res(scenario=NULL, p="^plot", path.results, out)
}

