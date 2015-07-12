#' Combine together HexSim census output across iterations.
#'
#' Combine together HexSim census output across iterations. If the scenarios have 
#' multiple census events, all census files are processed (separately). Mean values 
#' are reported in the final file which is located in Results/scenario_name
#' folder. 
#'
#' If there are several scenarios and the argument scenarios="all" (default), all
#' scenarios are processed, otherwise it is possible to select a subset of scenarios
#' using a character vector, e.g. scenarios=c("scen1", "scen2").
#'
#' If \code{path.results=NULL} an interactive dialog box is used to select the 
#'   path where the results are located
#'
#' Note, when there is a large number of files, this function may be memory hungry 
#'
#' @param path.results The path where the 'Results' folder is located
#' @param scenarios A character vector with scenarios to be processed or "all"
#' @return A list with three elements: the combined data, the mean and standard
#'   deviation 
#' @import data.table
#' @import XLConnect
#' @export

collate.census <- function(path.results=NULL, scenarios="all") {
  
  #----------------------------------------------------------------------------#
  # Helper functions
  #----------------------------------------------------------------------------#
  
  iter.folders <- function(dir.path, scenario) {
    folders <- list.dirs(path=paste(dir.path, scenario, sep="/"), 
                         recursive=FALSE)
    indices <- grep(pattern=paste0("^", scenario), x=basename(folders))
    scen.folders <- folders[indices]
    return(scen.folders)
  }
  
  byiter <- function(iter, l.iter.folders, census.list, census, nscen) {
    f <- paste(l.iter.folders[[nscen]][iter], census.list[census], sep="/")
    # census.data <- fread(f) # This currently doesn't work because 
    # there is a comma at the end of header row 
    census.data <- fread(f, skip=1)
    h <- c("Run", "Time.Step", "Population.Size", "Group.Members",
           "Floaters",   "Lambda" )
    ncols <- dim(census.data)[2]
    traits <- paste0("TraitInd", 0:(ncols - 7))
    setnames(census.data, c(h, traits))
    return(census.data)
  }
  
  # Return a data.table with all iterations for one census type and one scenario
  bycensus <- function(census, iters, l.iter.folders, file.list, nscen) {
    # a list with data from one census type and one scenario for each iterations
    l.census.data <- lapply(iters, byiter, census=census, census.list=file.list, 
                            l.iter.folders=l.iter.folders, nscen)
    census.data.comb <- rbindlist(l.census.data)
    return(census.data.comb)
  }
  
  
  # Return a list with one scenario with each census type for element
  byscen <- function (nscen, scenarios, l.iter.folders) {
    file.list <- list.files(l.iter.folders[[nscen]][1], 
                            pattern=paste0(scenarios[[nscen]], "\\.", "[0-9]+", 
                                           "\\.", "csv$"))
    iters <- seq_along(l.iter.folders[[nscen]])
    ncensus <- seq_along(file.list)
    # A list with one scenario with each census type for element
    scen.i <- lapply(ncensus, bycensus, iters, l.iter.folders, file.list, nscen)
    return(scen.i)
  }
  
  mean.iter <- function(census) {
    each.census <- census[, lapply(.SD, mean), by="Time.Step" ]
    return(each.census)
  }
  
  census.mean <- function(scen) {
    census.means <- lapply(scen, mean.iter)
    return(census.means)
  }
  
  sd.iter <- function(census) {
    each.census <- census[, lapply(.SD, sd), by="Time.Step" ]
    return(each.census)
  }
  
  census.sd <- function(scen) {
    census.sds <- lapply(scen, sd.iter)
    return(census.sds)
  }
  
  save.xlsx <- function(census, dir.path, nscen, scen.means, scen.sds, scenarios) {
    wb <- loadWorkbook(paste(dir.path, scenarios[[nscen]], 
                             paste0(scenarios[[nscen]], ".", census - 1, ".", "all", 
                                    ".", "xlsx"), sep="/"), create=TRUE)
    createSheet(wb, name="means")
    writeWorksheet(wb, scen.means[[nscen]][[census]], sheet="means")
    createSheet(wb, name="sd")
    writeWorksheet(wb, scen.sds[[nscen]][[census]], sheet="sd")
    saveWorkbook(wb)}
  
  save2disk <- function(nscen, dir.path, scen.means, scen.sds, scenarios) {
    ncensus <- seq_along(scen.means[[nscen]])
    lapply(ncensus, save.xlsx, dir.path, nscen, scen.means, scen.sds, scenarios)
  }
  
  #--------------------------------------------------------------------------#
  txt <- "Please, select the 'Results' folder within the workspace"
  if(is.null(path.results)) path.results <- choose.dir(caption = txt)
  if(scenarios == "all") 
    scenarios <- list.dirs(path=path.results, full.names=FALSE, recursive=FALSE)
  
  l.iter.folders <- lapply(scenarios, iter.folders, dir.path=path.results)
  nscens <- seq_along(scenarios)
  # A list of lists, each being a scenario. Each scenario has census types for 
  # elements
  data.comb <- lapply(nscens, byscen, scenarios=scenarios, 
                      l.iter.folders=l.iter.folders)
  
  scen.means <- lapply(data.comb, census.mean)
  scen.sds <- lapply(data.comb, census.sd)
  
  lapply(nscens, save2disk, dir.path=path.results, scen.means, scen.sds, scenarios)
  
  return(list(data=data.comb, means=scen.means, sds=scen.sds))
}

#' Compare census values against a baseline scenario.
#' 
#' \code{SSMD.census} carrries out pairwise compasisons of the census values 
#'   against a baseline scenario using Strictly Standardised Mean Difference 
#'   (SSMD, Zhang 2007).  
#'   
#' It takes as data input the output from \code{collate.census} (it reads data
#'   directly from xls files). 
#'   
#' @param path.results The path where the results are located
#' @param scenarios A character vector with scenarios to be processed or "all"
#' @param base A character vector with the name of the scenario to be used as 
#'   term of comparison
#' @param ncensus The number of the census to be considered
#' @return A list with SSMD in the first element and p-values in the second. 
#'   These resutls are also saved to disk as two tabs in an excel file.
#' @references
#' Zhang, X. D. 2007. A pair of new statistical parameters for quality control
#' in RNA interference high-throughput screening assays. Genomics 89:552-561.
#'
#' @import XLConnect
#' @export

SSMD.census <- function(path.results=NULL, scenarios="all", base=NULL, ncensus=0) {
  
  read.means <- function(scenario, path.results, ncensus) {
    mean_data <- readWorksheetFromFile(
      paste0(path.results, "/", scenario, "/", scenario, ".", 
             ncensus, ".", "all", ".", "xlsx"), 
      sheet="means")
    return(mean_data)
  }
  
  read.sds <- function(scenario, path.results, ncensus) {
    mean_data <- readWorksheetFromFile(
      paste0(path.results, "/", scenario, "/", scenario, ".", 
             ncensus, ".", "all", ".", "xlsx"), 
      sheet="sd")
    return(mean_data)
  }
  
  ssmd_census <- function(i, means, sds) {
    r <- (means[[i]][-c(1, 2)] - mean_base[-c(1, 2)]) / 
      sqrt(sds[[i]][-c(1, 2)]^2 + sd_base[-c(1, 2)]^2)
    return(r)
  }
  
  pval <- function(x) pnorm(abs(as.matrix(x)), lower.tail=FALSE)
  
  save.xlsx <- function(i, scenarios, ssmds, pvalues) {
    createSheet(wb, name=paste("SSMD_", scenarios[i]))
    writeWorksheet(wb, ssmds[[i]], sheet=paste("SSMD_", scenarios[i]))
    createSheet(wb, name=paste("pvalues_", scenarios[i]))
    writeWorksheet(wb, pvalues[[i]], sheet=paste("pvalues_", scenarios[i]))
    saveWorkbook(wb)}
  
  if(is.null(base)) stop("Please, provide the name of the base scenario")
  txt <- "Please, select the 'Results' folder within the workspace"
  if(is.null(path.results)) path.results <- choose.dir(caption = txt)
  if(scenarios == "all") {
    scenarios <- list.dirs(path=path.results, full.names=FALSE, recursive=FALSE)
    scenarios <- scenarios[scenarios != base]
  }
  
  wb_base <- loadWorkbook(paste0(path.results, "/", base, "/", base, ".", ncensus, 
                                 ".", "all", ".", "xlsx"))
  mean_base <- readWorksheet(wb_base, sheet="means")
  sd_base <- readWorksheet(wb_base, sheet="sd")
  
  means <-lapply(scenarios, read.means, path.results, ncensus)
  sds <-lapply(scenarios, read.sds, path.results, ncensus)
  
  ssmds <- lapply(seq_along(scenarios), ssmd, means, sds)
  pvalues <- lapply(ssmds, pval)
  
  wb <- loadWorkbook(paste0(path.results, "/", "SSMD_census", ncensus, ".", "xlsx"), 
                     create=TRUE)
  lapply(seq_along(scenarios), save.xlsx, scenarios, ssmds, pvalues)
  
  return(list(ssmds, pvalues))
}

#' Calculates descriptive stats from the HexSim generated report 'movement'
#' 
#' @param rep.move The fully qualified (i.e. including the path) name of the report 
#'   'movement'
#' @return A data.frame where each column is a HexSim's Event (and it is saved 
#'   to disk as .csv file)
#' @import data.table
#' @export  
move <- function(rep.move=NULL) {
  library()
  
  #----------------------------------------------------------------------------#
  # Helper functions
  #----------------------------------------------------------------------------#
  
  descr <- function(data,  lev) {
    setkey(data,  EventName)
    d <- data[lev,  summary(MetersDisplaced)]
    return(d)
  }
  
  #----------------------------------------------------------------------------#
  
  if (is.null(rep.move)) rep.move <- file.choose()
  mov <- fread(rep.move)
  setnames(mov,  names(mov),  gsub(" ",  "",  names(mov)))
  groups <- mov[,  unique(EventName)]
  l <- sapply(X = groups,  descr,  data=mov)
  write.csv(l, file=paste(dirname(rep.move), "summary_move.csv", sep="/"))
  return(l)
}
