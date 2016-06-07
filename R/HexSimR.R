
#' Combine together HexSim census outputs across iterations.
#'
#' Combine together HexSim census outputs across iterations. If the scenarios have 
#' multiple census events, all census files are processed (separately). Mean values 
#' are reported in the final file which is located in Results/scenario_name
#' folder and returned as a list. 
#'
#' If there are several scenarios and \code{scenarios="all"} (default), all
#' scenarios are processed, otherwise it is possible to select a subset of scenarios
#' using a character vector, e.g. scenarios=c("scen1", "scen2").
#'
#' If \code{path.results=NULL} an interactive dialog box is used to select the 
#'   path where the results are located
#'
#' Note, when there is a large number of files, this function may be memory hungry 
#'
#' @param path.results The path to the 'Results' folder 
#' @param scenarios A character vector with the scenarios to be processed or "all"
#' @return A list with three elements: the combined data, the mean and standard
#'   deviation. An xls file is also saved with the descriptive statistics and a 
#'   .rda file is saved with the combined data 
#' @import data.table
#' @import XLConnect
#' @export

collate.census <- function(path.results=NULL, scenarios="all") {
  
  #----------------------------------------------------------------------------#
  # Helper functions
  #----------------------------------------------------------------------------#
  
  byiter <- function(iter, l.iter.folders, census.list, census, nscen) {
    f <- paste(l.iter.folders[[nscen]][iter], census.list[census], sep="/")
    census.data <- fread(f)
    setnames(census.data, make.names(names(census.data)))
    return(census.data)
  }
  
  # Return a data.table with all iterations for one census type and one scenario
  bycensus <- function(census, iters, l.iter.folders, file.list, nscen) {
    # a list with data from one census type and one scenario for each iterations
    l.census.data <- lapply(iters, byiter, census=census, census.list=file.list, 
                            l.iter.folders=l.iter.folders, nscen)
    census.data.comb <- rbindlist(l.census.data, use.names=TRUE)
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
  suppressWarnings(if(scenarios == "all") 
    scenarios <- list.dirs(path=path.results, full.names=FALSE, recursive=FALSE))
  
  l.iter.folders <- lapply(scenarios, iter.folders, dir.path=path.results)
  nscens <- seq_along(scenarios)
  # A list of lists, each being a scenario. Each scenario has census types for 
  # elements
  data.comb <- lapply(nscens, byscen, scenarios=scenarios, 
                      l.iter.folders=l.iter.folders)
  names(data.comb) <- scenarios
  
  scen.means <- lapply(data.comb, census.mean)
  scen.sds <- lapply(data.comb, census.sd)
  
  lapply(nscens, save2disk, dir.path=path.results, scen.means, scen.sds, scenarios)
  save(data.comb, file=paste(path.results, "collated.census.rda", sep="/"), 
       compress="xz")
  
  return(list(data=data.comb, means=scen.means, sds=scen.sds))
}

#' Carry out calculations on a subset of variables in a specific census file
#' 
#' \code{census.calc} carries out calculations indicated with the argument \code{bin.f}.
#'   For example, this will be needed when trait are sub-classes and there is
#'   the need to sum a subset of them (e.g. traits are divided by gender and age
#'   and the user wants to have the total by gender) or calculate a proportion 
#'   (e.g. sex-ratio or over the total). Therefore, typically, these operations 
#'   will be sums, subtractions, divisions or 
#'   multiplications. However, \code{bin.f} can take any function that it is
#'   compatible with \code{Reduce()} (default is \code{"+"}).
#'   
#' The existing census file will be overwritten with the new census file, which 
#'   will include the new variable.   
#'   
#' The subset of data to be used by \code{bin.f} is indicated with \code{headers}.
#'   This argument takes a character vector with the headers of the census files. 
#'   These have to match exactly the headers in the census file (including spaces).
#'   \code{headers} is internally modified with \code{make.names()} to guarantee 
#'   that names are syntactically valid. Therefore, a character vector where the
#'   headers were already modified and would match the results of \code{make.names()}
#'   is also acceptable.
#'    
#' The argument \code{var.name} is the name of the new variable being calculated.
#'   If none is passed, then, by default, \code{census.calc} will paste together
#'   the names passed with \code{headers} separated by the operator passed with
#'   \code{bin.f}. If \code{bin.f} is a function (i.e. a R object of class 
#'   'function'), the default behaviour will fail and an error is reported. In 
#'   this case, the user have to pass a valid \code{var.name}. It is recommended 
#'   to use letters, numbers and the dot or underline ("_") characters, to start with 
#'   a letter or the dot not followed by a number. 
#' 
#' If more then one calculations needs to be performed, of the same calculations
#'   needs to be performed on different groups of variables (headers), then 
#'   \code{census.calc} needs to be run multiple times.
#'     
#' @param headers A character vector matching exactly the headers of the census 
#'   file columns that are to be used by \code{bin.f} 
#' @param var.name A character vector to name the new variable. Mandatory if 
#'   \code{bin.f} is of class \code{function}. See details.
#' @param bin.f A character vector to pass the binary function to be applied to 
#'   the columns identified by \code{headers} (Default: \code{"+"})
#' @inheritParams collate.census 
#' @inheritParams invasion.front 
#' @return Save census file to disk and return a list with the new census files
#' @import data.table
#' @export
census.calc <- function(path.results=NULL, ncensus, headers, var.name=NULL, 
                        bin.f="+", scenarios="all") {
  #----------------------------------------------------------------------------#
  # Helper functions
  #----------------------------------------------------------------------------#
  
  byiter <- function(iter, l.iter.folders, census.file, nscen, var.name, bin.f, 
                     headers) {
    f <- paste(l.iter.folders[[nscen]][iter], census.file, sep="/")
    census.data <- fread(f)  
    setnames(census.data, make.names(names(census.data)))   
    census.data[, (var.name) := Reduce(bin.f, .SD), .SDcols=headers, by="Time.Step"]
    write.csv(census.data, f, row.names=FALSE)
    return(census.data)
  }
  
  byscen <- function (nscen, scenarios, l.iter.folders, ncensus, var.name, bin.f, 
                      headers) {
    census.file <- paste0(scenarios[[nscen]], ".", ncensus, ".", "csv")
    iters <- seq_along(l.iter.folders[[nscen]])    
    l.scen.i <- lapply(iters, byiter, l.iter.folders, census.file, nscen, 
                       var.name, bin.f, headers)
    
    return(l.scen.i)
  }
  #----------------------------------------------------------------------------#
  if(is.function(bin.f) & is.null(var.name)) 
    stop("Please, pass a character vector to var.name")
  txt <- "Please, select the 'Results' folder within the workspace"
  if(is.null(path.results)) path.results <- choose.dir(caption = txt)
  suppressWarnings(if(scenarios == "all") 
    scenarios <- list.dirs(path=path.results, full.names=FALSE, recursive=FALSE))
  headers <- make.names(headers)
  if(is.null(var.name)) 
    var.name <- paste0(headers, collapse=bin.f)
  l.iter.folders <- lapply(scenarios, iter.folders, dir.path=path.results)
  nscens <- seq_along(scenarios)
  new.census <- lapply(nscens, byscen, scenarios, l.iter.folders, ncensus, 
                       var.name, bin.f, headers)
  return(new.census)
}


#' Compare census values against a baseline scenario.
#' 
#' \code{SSMD.census} carries out pairwise comparisons of the census values 
#'   against a baseline scenario using Strictly Standardised Mean Difference 
#'   (SSMD, Zhang 2007).  
#'   
#' It takes as data input the output from \code{collate.census} (it reads data
#'   directly from xls files). 
#'   
#' @param base A character vector with the name of the scenario to be used as 
#'   term of comparison
#' @param ncensus The number of the census to be considered
#' @inheritParams collate.census
#' @return A list with SSMD in the first element and p-values in the second. 
#'   These results are also saved to disk as two tabs in an excel file named 
#'   "SSMD_census[ncensus].xlsx", where [ncensus] is the number of the census file.
#' @references
#' Zhang, X. D. 2007. A pair of new statistical parameters for quality control
#' in RNA interference high-throughput screening assays. Genomics 89:552-561.
#'
#' @import XLConnect
#' @export

SSMD.census <- function(path.results=NULL, scenarios="all", base=NULL, ncensus=0) {
 
  #----------------------------------------------------------------------------#
  # Helper functions
  #----------------------------------------------------------------------------#
  
  
  ssmd_census <- function(i, means, sds, mean_base, sd_base, scenarios) {
    r <- calc.ssmd(mdata=means[[i]][-c(1, 2)], 
                   mbase=mean_base[-c(1, 2)], 
                   sddata=sds[[i]][-c(1, 2)], 
                   sdbase=sd_base[-c(1, 2)], 
                   scenarios[i])
    return(r)
    
  }
  
  #----------------------------------------------------------------------------#
  if(is.null(base)) stop("Please, provide the name of the base scenario")
  txt <- "Please, select the 'Results' folder within the workspace"
  if(is.null(path.results)) path.results <- choose.dir(caption = txt)
  suppressWarnings(if(scenarios == "all") {
    scenarios <- list.dirs(path=path.results, full.names=FALSE, recursive=FALSE)
  })
  scenarios <- scenarios[scenarios != base]
  wb_base <- loadWorkbook(paste0(path.results, "/", base, "/", base, ".", ncensus, 
                                 ".", "all", ".", "xlsx"))
  mean_base <- readWorksheet(wb_base, sheet="means")
  sd_base <- readWorksheet(wb_base, sheet="sd")
  
  means <-lapply(scenarios, read.means, path.results, ncensus)
  sds <-lapply(scenarios, read.sds, path.results, ncensus)
  
  ssmds <- lapply(seq_along(scenarios), ssmd_census, means, sds, 
                  mean_base, sd_base, scenarios)
  names(ssmds) <- scenarios
  pvalues <- lapply(ssmds, pval)
  names(pvalues) <- scenarios
  
  wb.name <- paste0(path.results, "/", "SSMD_census", ncensus, ".", "xlsx")
  if(file.exists(wb.name)) file.remove(wb.name)
  wb <- loadWorkbook(wb.name, create=TRUE)
  lapply(seq_along(scenarios), ssmd2xlsx, scenarios, ssmds, pvalues, wb)
  
  return(list(ssmds, pvalues))
}

#' Compare mean movement distances against those of a baseline scenario.
#' 
#' \code{SSMD.move} carries out pairwise comparisons of the mean movement distances 
#'   against those of a baseline scenario using Strictly Standardised Mean Difference 
#'   (SSMD, Zhang 2007).  
#'   
#' It directly reads the .csv files written with \code{move}. It assumes that all 
#'   files have the same name, which, if different from the default, can be passed
#'   with the argument\code{sum.move}.
#'   
#' @param sum.move The name of the file where the data are (including the extension).
#' @inheritParams SSMD.census
#' @return A list with SSMD in the first element and p-values in the second. 
#'   These results are also saved to disk as two tabs in an excel file.
#' @references
#' Zhang, X. D. 2007. A pair of new statistical parameters for quality control
#' in RNA interference high-throughput screening assays. Genomics 89:552-561.
#'
#' @import XLConnect
#' @export
SSMD.move <- function(path.results=NULL, scenarios="all", base=NULL, 
                      sum.move="summary_move.csv") {
  #----------------------------------------------------------------------------#
  # Helper functions
  #----------------------------------------------------------------------------#
  
  read.data <- function(scenario, path.results, sum.move) {
    data <- read.csv(
      paste0(path.results, "/", scenario, "/", sum.move))
    rownames(data) <- data[, 1] 
    return(data[, 2:dim(data)[2]])
  }
    
  ssmd_move <- function(i, data, base, scenarios) {
    
    r <- calc.ssmd(mdata=data[[i]]["Mean", ], 
                   mbase=base["Mean", ], 
                   sddata=data[[i]]["Std", ], 
                   sdbase=base["Std", ], 
                   scenarios[i])
    row.names(r) <- NULL
    return(r)
  }
  #----------------------------------------------------------------------------#
  
  if(is.null(base)) stop("Please, provide the name of the base scenario")
  txt <- "Please, select the 'Results' folder within the workspace"
  if(is.null(path.results)) path.results <- choose.dir(caption = txt)
  suppressWarnings(if(scenarios == "all") {
    scenarios <- list.dirs(path=path.results, full.names=FALSE, recursive=FALSE)
  })
  scenarios <- scenarios[scenarios != base]
  base.data <- read.data(scenario=base, path.results, sum.move)
   
  data <-lapply(scenarios, read.data, path.results, sum.move)
  
  ssmds <- lapply(seq_along(scenarios), ssmd_move, data, base.data, scenarios)
  names(ssmds) <- scenarios
  pvalues <- lapply(ssmds, pval)
  names(pvalues) <- scenarios
  wb.name <- paste0(path.results, "/", "SSMD_move", ".", "xlsx")
  if(file.exists(wb.name)) file.remove(wb.name)
  wb <- loadWorkbook(wb.name, create=TRUE)
  lapply(seq_along(scenarios), ssmd2xlsx, scenarios, ssmds, pvalues, wb)
  res <- list(SSMD=ssmds, pvalues=pvalues)
  return(res)
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
  
  #----------------------------------------------------------------------------#
  # Helper functions
  #----------------------------------------------------------------------------#
  
  descr <- function(data,  lev) {
    setkey(data,  EventName)
    d <- data[lev,  summary(MetersDisplaced)]
    std <- data[lev,  sd(MetersDisplaced)]
    names(std) <- "Std"
    d <- c(d, std)
    return(d)
  }
  
  #----------------------------------------------------------------------------#
  
  if (is.null(rep.move)) rep.move <- file.choose()
  message(paste("Parsing the report", basename(rep.move)))
  mov <- fread(rep.move)
  setnames(mov,  names(mov),  gsub(" ",  "",  names(mov)))
  message("Done!")
  groups <- mov[,  unique(EventName)]
  l <- sapply(X = groups,  descr,  data=mov)
  write.csv(l, file=paste(dirname(rep.move), "summary_move.csv", sep="/"))
  return(l)
}

#' Calculates descriptive stats from the HexSim generated report 'ranges'
#' 
#' \code{ranges} calculates descriptive stats, for each year, from the HexSim
#' generated report 'ranges'. In addition to HexSim items, \code{ranges} also
#' calculates the mean number of groups and the mean size of territory in ha and
#' sqkm based on the value passed with \code{hx}. From this table \code{ranges}
#' then calculates the mean across all years in the window of years 
#' \code{start} - \code{end}.
#' 
#' @param rep.ranges The fully qualified (i.e. including the path) name of the 
#'   report
#' @param hx The size of one hexagon in hectares
#' @param events A character vector with the name of the events for which the 
#'   mean and standard deviation across years needs to be calculated. If NULL 
#'   (default), all events are considered
#' @param start The first time step to be included
#' @param end The last time step to be include
#' @return A list with three elements:
#'   \itemize{ 
#'       \item $descriptive: A \code{data.frame} (\code{data.table}) with the 
#'              summary statistics with mean for each time step, for each event
#'        \item $means: A \code{data.frame} (\code{data.table}) with the mean 
#'              across the selected time steps
#'        \item $sds: A \code{data.frame} (\code{data.table}) with the standard 
#'              deviation across the selected time steps      
#'          }
#'    These results are also saved to disk in two files:
#'    \itemize{ 
#'       \item $descriptive_ranges.csv:  The summary statistics for each time 
#'               step, for each event     
#'        \item $summary_ranges.xlsx:  the mean and standard deviation across 
#'               the selected time steps (in two separate tabs)
#'            }
#'              
#' @import data.table
#' @import XLConnect
#' @export

ranges <- function(rep.ranges=NULL, hx=NULL, events=NULL, start="min", end="max") {
  #----------------------------------------------------------------------------#
  # Helper functions
  #----------------------------------------------------------------------------#
    
  mean.ranges <- function(event, summary, start, end) {
    setkeyv(summary, c("TimeStep", "Events"))
    var <- c("GroupSize", "Resources", "nGroups", "ha", "sqkm")
    m <- summary[J(start:end, event), lapply(.SD, mean, na.rm=TRUE), .SDcols=var]
    m[, Event := event]
    m[, Stat := "Mean"]
    setcolorder(m, 
          c("Event", "Stat", "GroupSize", "Resources", "nGroups", "ha", "sqkm"))
    return(m)
  }
  
  sd.ranges <- function(event, summary, start, end) {
    setkeyv(summary, c("TimeStep", "Events"))
    var <- c("GroupSize", "Resources", "nGroups", "ha", "sqkm")
    s <- summary[J(start:end, event), lapply(.SD, sd, na.rm=TRUE), .SDcols=var]
    s[, Event := event]
    s[, Stat := "SD"]
    setcolorder(s, 
          c("Event", "Stat", "GroupSize", "Resources", "nGroups", "ha", "sqkm"))
    return(s)
  }
  
  #----------------------------------------------------------------------------#
  
  if (!is.numeric(hx)) stop("Please provide a suitable value for hx")
  if (is.null(rep.ranges)) rep.ranges <- file.choose()
  message(paste("Parsing the report", basename(rep.ranges)))
  h <- read.csv(rep.ranges, header=FALSE, nrow=1, colClasses="character")
  h <- gsub(" ", "", h)
  message("Inspecting the file to detect the maximum number of columns")
  ncols <- max(count.fields(rep.ranges, sep=","))
  h <- c(h, rep("NA", ncols - length(h)))
  col.class <- c(rep("integer", 2), rep("character", 2), rep("integer", 3), 
                 "numeric", "integer", rep("NULL", ncols - 9))
  range <- read.csv(rep.ranges, header=FALSE, skip=1, colClasses=col.class, 
                    col.names=h)
  message("Done!")
  range <- data.table(range)
  summary <- range[, lapply(.SD, mean), .SDcol=h[7:9], by=list(EventName,TimeStep)]
  nGroups <- range[, .N, by=list(EventName,TimeStep, Replicate)]
  nGroups <- nGroups[, mean(N, na.rm = TRUE), by=list(EventName,TimeStep)]
  summary[, nGroups := nGroups[, V1]]
  summary[, Events := gsub(" ", "", EventName)]
  summary[, ha := NumberofHexagons * hx]
  summary[, sqkm := ha * 0.01]
  if(is.null(events)) {
    events <- summary[, unique(Events)]
  } else {
    events <- gsub(" ", "", events)
  }
  if(start == "min") start <- summary[, min(TimeStep)]
  if(end == "max") end <- summary[, max(TimeStep)]
  l.means <- lapply(events, mean.ranges, summary, start, end)
  means <- rbindlist(l.means, use.names=TRUE)
  l.std <- lapply(events, sd.ranges, summary, start, end)
  std <- rbindlist(l.std, use.names=TRUE)
  write.csv(summary, file=paste(dirname(rep.ranges), "descriptive_ranges.csv", 
                                sep="/"))
  wb.name <- paste(dirname(rep.ranges), "summary_ranges.xlsx", sep="/")
  if(file.exists(wb.name)) file.remove(wb.name)
  wb <- loadWorkbook(wb.name, create=TRUE)
  createSheet(wb, name="means")
  writeWorksheet(wb, means, sheet="means")
  createSheet(wb, name="sd")
  writeWorksheet(wb, std, sheet="sd")
  saveWorkbook(wb)
  return(list(descriptive=summary, means=means, sds=std))
}


#' Compare ranges descriptive statistics against a baseline scenario.
#' 
#' \code{SSMD.ranges} carries out pairwise comparisons of the descriptive 
#' statistics calculated froma Ranges report 
#'   against a baseline scenario using Strictly Standardised Mean Difference 
#'   (SSMD, Zhang 2007).  
#'   
#' It takes as data input the output from \code{ranges} (it reads data
#'   directly from xls files). 
#'   
#' @param sum.ranges The name of the file where the data are (including the extension).
#' @inheritParams SSMD.census
#' @return A list with SSMD in the first element and p-values in the second. 
#'   These results are also saved to disk as two tabs in an excel file.
#' @references
#' Zhang, X. D. 2007. A pair of new statistical parameters for quality control
#' in RNA interference high-throughput screening assays. Genomics 89:552-561.
#'
#' @import XLConnect
#' @export

SSMD.ranges <- function(path.results=NULL, scenarios="all", base=NULL, 
                        sum.ranges="summary_ranges.xlsx") {
  
  #----------------------------------------------------------------------------#
  # Helper functions
  #----------------------------------------------------------------------------#
  
  read.means <- function(scenario, path.results, sum.ranges) {
    mean_data <- readWorksheetFromFile(
      paste(path.results, scenario, sum.ranges, sep="/"), 
      sheet="means")
    return(mean_data)
  }
  
  read.sds <- function(scenario, path.results, ncensus) {
    std_data <- readWorksheetFromFile(
      paste(path.results, scenario, sum.ranges, sep="/"), 
      sheet="sd")
    return(std_data)
  }
  
  ssmd_ranges <- function(i, means, sds, mean_base, sd_base, scenarios) {
    r <- calc.ssmd(mdata=means[[i]][-c(1, 2)], 
                   mbase=mean_base[-c(1, 2)], 
                   sddata=sds[[i]][-c(1, 2)], 
                   sdbase=sd_base[-c(1, 2)], 
                   scenarios[i])
    return(r)
  }
  #----------------------------------------------------------------------------#
  if(is.null(base)) stop("Please, provide the name of the base scenario")
  txt <- "Please, select the 'Results' folder within the workspace"
  if(is.null(path.results)) path.results <- choose.dir(caption = txt)
  suppressWarnings(if(scenarios == "all") {
    scenarios <- list.dirs(path=path.results, full.names=FALSE, recursive=FALSE)
  })
  scenarios <- scenarios[scenarios != base]
  wb_base <- loadWorkbook(paste(path.results, base, sum.ranges, sep="/"))
  mean_base <- readWorksheet(wb_base, sheet="means")
  sd_base <- readWorksheet(wb_base, sheet="sd")
  
  means <-lapply(scenarios, read.means, path.results, sum.ranges)
  sds <-lapply(scenarios, read.sds, path.results, sum.ranges)
  
  ssmds <- lapply(seq_along(scenarios), ssmd_ranges, means, sds, 
                  mean_base, sd_base, scenarios)
  names(ssmds) <- scenarios
  pvalues <- lapply(ssmds, pval)
  names(pvalues) <- scenarios
  
  wb.name <- paste0(path.results, "/", "SSMD_ranges", ".", "xlsx")
  if(file.exists(wb.name)) file.remove(wb.name)
  wb <- loadWorkbook(wb.name, create=TRUE)
  lapply(seq_along(scenarios), ssmd2xlsx, scenarios, ssmds, pvalues, wb)
  
  return(list(SSMD=ssmds, pvalues=pvalues))
}

#' Generate summary statistics for multiple reports
#' 
#' \code{multi.reports} is basically a wrapper for the two functions that process
#'   HexSim generated reports. It assumes that the names of the report files were 
#'   not changed from defaults and that each report was generated in a similar 
#'   fashion. That is, either they are all generated from a combined log file, or
#'   they are generated from each iteration's log file. It reads the reports in 
#'   each of the scenario's folder (or iteration subfolders) and process them as  
#'   using either \code{move} or \code{ranges}.
#'   
#' If the reports file is generated from each iteration's log file, then the 
#'   reports are sitting within each iteration's subfolder and \code{path.results} 
#'   has to point to the scenario's folder (i.e. the subfolders will be the 
#'   scenario's name with the suffix [x], where x is the number of the iteration).
#'   
#' @param pop.name The name of the population
#' @param type Whether movement ("move") or ranges ("ranges") reports should be 
#'     processed
#' @param all Whether the reports were generated from a combined log file
#' @inheritParams SSMD.census
#' @inheritParams ranges
#' @seealso \code{\link{move}}, \code{\link{ranges}}
#' @return A list where each element is the output from either \code{move} or
#'     \code{ranges}.
#' @export
multi.reports <- function(path.results=NULL, scenarios="all", pop.name=NULL, 
                          type="move", all=TRUE, hx=NULL, events=NULL, 
                          start="min", end="max") {
  #----------------------------------------------------------------------------#
  # Helper functions
  #----------------------------------------------------------------------------#
  
  file.names <- function(scenario, path.results, pop.name) {
    n <- paste0(path.results, "/", scenario, "/", scenario,
                if(all == TRUE) "-[all]", "_REPORT_", type, "_", pop.name, ".csv")
    return(n)
  }
  #----------------------------------------------------------------------------#
  
  if(is.null(pop.name)) stop("Please, provide the name of the population")
  txt <- "Please, select the 'Results' folder within the workspace"
  if(is.null(path.results)) path.results <- choose.dir(caption = txt)
  suppressWarnings(if(scenarios == "all") 
    scenarios <- list.dirs(path=path.results, full.names=FALSE, recursive=FALSE))
  
  rep.names <- lapply(scenarios, file.names, path.results, pop.name)
  if(type == "move") {
    rep.summaries <- lapply(rep.names, move)
  } else {
    rep.summaries <- lapply(rep.names, ranges, hx, events, start, end)
  }
  
  return(rep.summaries)
}
