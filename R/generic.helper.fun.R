
#' Detect iteration folders
#' 
#' This function is used by a number of HexSimR functions to create a list of 
#'   path for where results for each iterations are saved.
#' @param dir.path See the path where the 'Results' folder is located 
#' @param scenario Scenario to be processed. This is normally passed with lapply()
#' @return A list where each element of the path to the replicates of a scenario
iter.folders <- function(dir.path, scenario) {
  folders <- list.dirs(path=paste(dir.path, scenario, sep="/"), 
                       recursive=FALSE)
  indices <- grep(pattern=paste0("^", scenario), x=basename(folders))
  scen.folders <- folders[indices]
  return(scen.folders)
}

#' Save SSMD results to xlsx file
#' 
#' Two tabs are saved for each scenarios: SSMD and p values.
#' @param i Numeric vector with seq 1 to number of scenarios
#' @param scenarios Character vector with names of scenarios
#' @param ssmds data.frame with SSMD values
#' @param pvalues data.frame with SSMD values
#' @param wb Workbook where to save the tabs
#' @import XLConnect
#' @export
ssmd2xlsx <- function(i, scenarios, ssmds, pvalues, wb) {
  suppressWarnings(if(nchar(scenarios) > 25) {
    scenarios <- substr(scenarios, nchar(scenarios) - 24, nchar(scenarios)) 
  })
  createSheet(wb, name=paste0("SSMD_", scenarios[i]))
  writeWorksheet(wb, ssmds[[i]], sheet=paste0("SSMD_", scenarios[i]))
  createSheet(wb, name=paste0("pval_", scenarios[i]))
  writeWorksheet(wb, pvalues[[i]], sheet=paste0("pval_", scenarios[i]))
  saveWorkbook(wb)}
