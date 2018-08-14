
#' Detect iteration folders
#' 
#' This function is used by a number of HexSimR functions to create a list of 
#'   path where results for each iteration are saved.
#' @param dir.path The path where the 'Results' folder is located 
#' @param scenario Scenario to be processed. This is normally passed with lapply()
#' @return A list where each element is the path to the replicates of a scenario
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
  if(nchar(scenarios[i]) > 25) {
    scenarios <- substr(scenarios, nchar(scenarios) - 24, nchar(scenarios)) 
  }
  createSheet(wb, name=paste0("SSMD_", scenarios[i]))
  writeWorksheet(wb, ssmds[[i]], sheet=paste0("SSMD_", scenarios[i]))
  createSheet(wb, name=paste0("pval_", scenarios[i]))
  writeWorksheet(wb, pvalues[[i]], sheet=paste0("pval_", scenarios[i]))
  saveWorkbook(wb)}

#' Calculate SSMDs given a df (or equivalent) of means and stds
#' 
#' Calculate SSMDs given a df (or equivalent) of means and stds.
#' 
#' Checks whether the two dfs have same column names and attempt to re-order the 
#'   columns if not.
#' 
#' @param mdata Data containing mean values
#' @param mbase Data containing mean values for base scenario
#' @param sddata Data containing standard deviation values
#' @param sdbase Data containing standard deviation values for base scenario
#' @param scenario The name of the scenario for which SSMDs are being calculated
#' @import data.table
#' @export
calc.ssmd <- function(mdata, mbase, sddata, sdbase, scenario) {
  mdata <- data.table(mdata) 
  mbase <- data.table(mbase)
  sddata <- data.table(sddata)
  sdbase <- data.table(sdbase)
  if(!identical(names(mdata), names(mbase))) {
    if(identical(sort(names(mdata)), sort(names(mbase)))) {
      message(paste("Column names in the census file from", scenario,
                    "do not match base scenario. Reordering the columns..."))
      setcolorder(mdata, names(mbase))
      setcolorder(sddata, names(sdbase))
      message("Done!")
      } else {
        stop(paste("Column names in the census file from", scenario,
                   "do not match base scenario and it is not possible to 
                   reorder them..."))
      }
    }
  r <- (mdata - mbase) / sqrt(sddata^2 + sdbase^2)
  return(r)
}

#' Calculate p values 
#' 
#' \code{pval} calculates one-tailed p values from a data.frame that contains 
#'   quantiles and it is used internally from SSMD functions.
#' @param x The data.frame of quantiles
pval <- function(x) pnorm(abs(as.matrix(x)), lower.tail=FALSE)

#' Read means values from output of collate.census
#' 
#' #' This function is internally used by other \code{HexSimR} functions
#' @param scenario A character vector with the name of the scenario  
#' @inheritParams collate.census
#' @inheritParams SSMD.census
#' @export
read.means <- function(scenario, path.results, ncensus) {
  mean_data <- readWorksheetFromFile(
    paste0(path.results, "/", scenario, "/", scenario, ".", 
           ncensus, ".", "all", ".", "xlsx"), 
    sheet="means")
  return(mean_data)
}

#' Read standard deviation values from output of collate.census
#' 
#' This function is internally used by other \code{HexSimR} functions
#' 
#' @param scenario A character vector with the name of the scenario  
#' @inheritParams collate.census
#' @inheritParams SSMD.census
#' @export
read.sds <- function(scenario, path.results, ncensus) {
  std_data <- readWorksheetFromFile(
    paste0(path.results, "/", scenario, "/", scenario, ".", 
           ncensus, ".", "all", ".", "xlsx"), 
    sheet="sd")
  return(std_data)
}