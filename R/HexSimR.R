
#'Combine together HexSim census outputs across iterations.
#'
#'Combine together HexSim census outputs across iterations. If the scenarios
#'have multiple census events, all census files are processed (separately). Mean
#'values are reported in the final file which is located in
#'Results/scenario_name folder and returned as a list.
#'
#'If there are several scenarios and \code{scenarios="all"} (default), all
#'scenarios are processed, otherwise it is possible to select a subset of
#'scenarios using a character vector, e.g. scenarios=c("scen1", "scen2").
#'
#'If \code{path.results=NULL} an interactive dialog box is used to select the
#'path where the results are located
#'
#'\code{start} controls the first time step that should be considered in each
#'iteration. This is intended to be used when the user wants to discard the
#'first part of the simulations, for example because the model needs to reach a
#'steady-state.
#'
#'\code{end} controls the last time step that should be considered in each
#'iteration. This is typically the last time step the simulations were set for,
#'or it can be used to discard the time steps after a certain point.
#'\strong{Note} that if the population goes extinct, HexSim may stop recording
#'data after the time step in which the population went extinct, hence the last
#'time step may be different across iteration. Explicitly setting \code{end} is
#'particularly important in this situation to ensure correct mean and sd
#'calculations. \code{collate.census} will fill in the data with "0" if the
#'population goes extinct before \code{end} when the latter is a number.
#'
#'The default setting (i.e. \code{end="max"}) is for convenience and to ensure
#'back-compatibility, but its use is not recommended. When  \code{end="max"}
#'\code{collate.census} will use the last time step of each replicate as end
#'point, nut, if the iterations have different number of time steps, the mean
#'and sd calculations will be a mixture between the extant and all populations
#'(across all iterations, see below).
#'
#'\code{keep.zeros} is a logical (\code{TRUE}, default, or \code{FALSE}) and
#'indicates whether the zero-values should be used in the mean and sd
#'calculations. If not, the zeros are stripped out before calculations. This
#'could make sense, for example, when the trait (or other variables) being
#'considered indicates the population size and the user wants to consider only
#'extent populations (i.e. ignore extinct populations). When
#'\code{keep.zeros=TRUE} an "all" suffix is appended to the file name,
#'otherwise, "extant" is used.
#'
#'Note, when there is a large number of files, this function may be memory
#'hungry
#'
#'@param path.results The path to the 'Results' folder
#'@param scenarios A character vector with the scenarios to be processed or
#'  "all"
#'@param keep.zeros Whether zeros are retained in the mean calculations (TRUE,
#'  default), or are excluded (FALSE)
#'@param verbose Should progress be printed in the console? Default: FALSE 
#'@inheritParams ranges
#'@return A list with three elements: the combined data, the mean and standard
#'  deviation. Each of these elements is in itself a list with  each scenario as
#'  a element. Nested within the scenarios is a list with each census file. An
#'  xls file is also saved with the descriptive statistics and a .rda file is
#'  saved with the combined data
#'@import data.table
#'@import XLConnect
#'@importFrom tcltk tk_choose.dir
#'@export

collate.census <- function(path.results=NULL, scenarios="all", start="min", end="max",
                           keep.zeros=TRUE, verbose=FALSE) {
  
  #----------------------------------------------------------------------------#
  # Helper functions
  #----------------------------------------------------------------------------#
  
  byiter <- function(iter, l.iter.folders, census.list, census, nscen, start, end, verbose) {
    f <- paste(l.iter.folders[[nscen]][iter], census.list[census], sep="/")
    if(verbose) message(paste("Processing iteration:", basename(l.iter.folders[[nscen]][iter])))
    census.data <- fread(f)
    headers <- make.names(names(census.data))
    setnames(census.data, headers)
    if(is.numeric(start)) {
      census.data <- census.data[match(start, Time.Step):dim(census.data)[1],]
    }
    if(is.numeric(end)) {
      if(census.data[, max(Time.Step)] < end) {
        TS <- (census.data[, max(Time.Step)] + 1):end
        m <- matrix(rep(0, length(TS) * (length(census.data) - 2)), 
                    ncol=length(census.data) - 2)
        mdt <- data.table(m)
        setnames(mdt, headers[-match(c("Run", "Time.Step"), headers)])
        mdt[, Time.Step := TS]
        mdt[, Run := census.data[, unique(Run)]]
        census.data <- rbindlist(list(census.data, mdt), use.names = TRUE)
      } else {
        if(census.data[, max(Time.Step)] > end) 
          census.data <- census.data[1:match(end, Time.Step),]
      }
    }
    return(census.data)
  }
  
  # Return a data.table with all iterations for one census type and one scenario
  bycensus <- function(census, iters, l.iter.folders, file.list, nscen, start, end, verbose) {
    if(verbose) message(paste("Processing census:",  file.list[census]))
    # a list with data from one census type and one scenario for each iterations
    l.census.data <- lapply(iters, byiter, census=census, census.list=file.list, 
                            l.iter.folders=l.iter.folders, nscen=nscen, 
                            start=start, end=end, verbose=verbose)
    census.data.comb <- rbindlist(l.census.data, use.names=TRUE)
    return(census.data.comb)
  }
  
  
  # Return a list with one scenario with each census type for element
  byscen <- function (nscen, scenarios, l.iter.folders, start, end, verbose) {
    file.list <- list.files(l.iter.folders[[nscen]][1], 
                            pattern=paste0(scenarios[[nscen]], "\\.", "[0-9]+", 
                                           "\\.", "csv$"))
    if(verbose) message(paste("Processing scenario:", scenarios[[nscen]]))
    iters <- seq_along(l.iter.folders[[nscen]])
    ncensus <- seq_along(file.list)
    # A list with one scenario with each census type for element
    scen.i <- lapply(ncensus, bycensus, iters, l.iter.folders, file.list, nscen, 
                     start=start, end=end, verbose=verbose)
    census.names <- sub(pattern = ".csv", "", x = file.list)
    names(scen.i) <- census.names
    return(scen.i)
  }
  
  rep.zeros <- function(x) replace(x, which(x==0), NA)
  
  mean.iter <- function(census, keep.zeros) {
    if(isFALSE(keep.zeros)) census <- census[, lapply(.SD, rep.zeros)]
    each.census <- census[, lapply(.SD, mean, na.rm=TRUE), by="Time.Step" ]
    return(each.census)
  }
  
  census.mean <- function(scen, keep.zeros) {
    census.names <- names(scen)
    census.means <- lapply(scen, mean.iter, keep.zeros=keep.zeros)
    names(census.means) <- census.names
    return(census.means)
  }
  
  sd.iter <- function(census, keep.zeros) {
    if(isFALSE(keep.zeros)) census <- census[, lapply(.SD, rep.zeros)]
    each.census <- census[, lapply(.SD, sd, na.rm=TRUE), by="Time.Step" ]
    return(each.census)
  }
  
  census.sd <- function(scen, keep.zeros) {
    census.names <- names(scen)
    census.sds <- lapply(scen, sd.iter, keep.zeros=keep.zeros)
    names(census.sds) <- census.names
    return(census.sds)
  }
  
  save.xlsx <- function(census, dir.path, nscen, scen.means, scen.sds, scenarios, 
                        census.names, keep.zeros) {
    wb <- loadWorkbook(file.path(dir.path, scenarios[[nscen]], 
                             paste(census.names[census], 
                                   if(isFALSE(keep.zeros)) "extant" else "all", "comb", 
                                    "xlsx", sep=".")), create=TRUE)
    createSheet(wb, name="means")
    writeWorksheet(wb, scen.means[[nscen]][[census]], sheet="means")
    createSheet(wb, name="sd")
    writeWorksheet(wb, scen.sds[[nscen]][[census]], sheet="sd")
    saveWorkbook(wb)}
  
  save2disk <- function(nscen, dir.path, scen.means, scen.sds, scenarios, keep.zeros) {
    ncensus <- seq_along(scen.means[[nscen]])
    census.names <- names(scen.means[[nscen]])
    lapply(ncensus, save.xlsx, dir.path, nscen, scen.means, scen.sds, scenarios, 
           census.names, keep.zeros)
  }
  
  #--------------------------------------------------------------------------#
  if(end == "max") {
    warning("The value 'max' for the argument 'end' is a convenience option, \nbut can lead to spurious results if used improperly. \nPlease, see 'Details' in '?collate.census'.")
    }
  txt <- "Please, select the 'Results' folder within the workspace"
  if(is.null(path.results)) path.results <- tk_choose.dir(caption = txt)
  if(length(scenarios) == 1) {
    if(scenarios == "all") 
      scenarios <- list.dirs(path=path.results, full.names=FALSE, recursive=FALSE)
  }
  
  l.iter.folders <- lapply(scenarios, iter.folders, dir.path=path.results)
  nscens <- seq_along(scenarios)
  # A list of lists, each being a scenario. Each scenario has census types for 
  # elements
  data.comb <- lapply(nscens, byscen, scenarios=scenarios, 
                      l.iter.folders=l.iter.folders, start=start, end=end, verbose=verbose)
  names(data.comb) <- scenarios
  
  scen.means <- lapply(data.comb, census.mean, keep.zeros=keep.zeros)
  scen.sds <- lapply(data.comb, census.sd, keep.zeros=keep.zeros)
  
  lapply(nscens, save2disk, dir.path=path.results, scen.means, scen.sds, 
         scenarios, keep.zeros=keep.zeros)
  coll.census <- list(data=data.comb, means=scen.means, sds=scen.sds)
  save(coll.census, file=file.path(path.results, 
                                   paste(if(isFALSE(keep.zeros)) "extant" else "all", 
                                         "collated.census.rda", sep=".")), 
       compress="xz")
  
  return(coll.census)
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
#' @importFrom tcltk tk_choose.dir 
#' @export
census.calc <- function(path.results=NULL, ncensus, headers, var.name=NULL, 
                        bin.f="+", scenarios="all", verbose=FALSE) {
  #----------------------------------------------------------------------------#
  # Helper functions
  #----------------------------------------------------------------------------#
  
  byiter <- function(iter, l.iter.folders, census.file, nscen, var.name, bin.f, 
                     headers, verbose=verbose) {
    if(verbose) message(paste("Processing iteration:", basename(l.iter.folders[[nscen]][iter])))
    f <- paste(l.iter.folders[[nscen]][iter], census.file, sep="/")
    census.data <- fread(f)  
    setnames(census.data, make.names(names(census.data)))   
    census.data[, (var.name) := Reduce(bin.f, .SD), .SDcols=headers, by="Time.Step"]
    write.csv(census.data, f, row.names=FALSE)
    return(census.data)
  }
  
  byscen <- function (nscen, scenarios, l.iter.folders, ncensus, var.name, bin.f, 
                      headers, verbose) {
    census.file <- paste0(scenarios[[nscen]], ".", ncensus, ".", "csv")
    if(verbose) message(paste("Processing scenario:", scenarios[[nscen]]))
    iters <- seq_along(l.iter.folders[[nscen]])    
    l.scen.i <- lapply(iters, byiter, l.iter.folders, census.file, nscen, 
                       var.name, bin.f, headers, verbose=verbose)
    
    return(l.scen.i)
  }
  #----------------------------------------------------------------------------#
  if(is.function(bin.f) & is.null(var.name)) 
    stop("Please, pass a character vector to var.name")
  txt <- "Please, select the 'Results' folder within the workspace"
  if(is.null(path.results)) path.results <- tk_choose.dir(caption = txt)
  if(length(scenarios) == 1) {
    if(scenarios == "all") 
      scenarios <- list.dirs(path=path.results, full.names=FALSE, recursive=FALSE)
  }
  headers <- make.names(headers)
  if(is.null(var.name)) 
    var.name <- paste0(headers, collapse=bin.f)
  l.iter.folders <- lapply(scenarios, iter.folders, dir.path=path.results)
  nscens <- seq_along(scenarios)
  new.census <- lapply(nscens, byscen, scenarios, l.iter.folders, ncensus, 
                       var.name, bin.f, headers, verbose=verbose)
  return(new.census)
}


#' Compare census values against a baseline scenario.
#'
#' \code{SSMD.census} carries out pairwise comparisons of the census values
#' against a baseline scenario using Strictly Standardised Mean Difference
#' (SSMD, Zhang 2007).
#'
#' It takes as data input the output from \code{collate.census} (it reads data
#' directly from xls files). The argument \code{keep.zeros} needs to be set
#' exactly as it was in \code{collate.census} call.
#'
#' @param base A character vector with the name of the scenario to be used as
#'   term of comparison
#' @param ncensus The number of the census to be considered
#' @param keep.zeros Whether zeros were kept or not when census files were
#'   collated
#' @inheritParams collate.census
#' @return A list with SSMD in the first element and p-values in the second.
#'   These results are also saved to disk as two tabs in an excel file named
#'   "SSMD_census[ncensus].xlsx", where [ncensus] is the number of the census
#'   file.
#' @references Zhang, X. D. 2007. A pair of new statistical parameters for
#' quality control in RNA interference high-throughput screening assays.
#' Genomics 89:552-561.
#'
#' @import XLConnect
#' @importFrom tcltk tk_choose.dir
#' @export

SSMD.census <- function(path.results=NULL, scenarios="all", base=NULL, ncensus=0, 
                        keep.zeros="TRUE") {
 
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
  if(is.null(path.results)) path.results <- tk_choose.dir(caption = txt)
  if(length(scenarios) == 1) {
    if(scenarios == "all") 
      scenarios <- list.dirs(path=path.results, full.names=FALSE, recursive=FALSE)
  }
  
  scenarios <- scenarios[scenarios != base]
  wb_base <- loadWorkbook(file.path(path.results, base, paste(base, ncensus, 
                            if(isFALSE(keep.zeros)) "extant" else "all", "comb", 
                            "xlsx", sep=".")))
  
  mean_base <- readWorksheet(wb_base, sheet="means")
  sd_base <- readWorksheet(wb_base, sheet="sd")
  
  means <-lapply(scenarios, read.means, path.results, ncensus, keep.zeros=keep.zeros)
  sds <-lapply(scenarios, read.sds, path.results, ncensus, keep.zeros=keep.zeros)
  
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
#' @importFrom stats pnorm sd
#' @importFrom utils  count.fields read.csv write.csv
#' @importFrom tcltk tk_choose.dir
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
  if(is.null(path.results)) path.results <- tk_choose.dir(caption = txt)
  if(length(scenarios) == 1) {
    if(scenarios == "all") 
      scenarios <- list.dirs(path=path.results, full.names=FALSE, recursive=FALSE)
  }
  
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
#' @param end The last time step to be included
#' @return A list with three elements:
#'   \itemize{ 
#'       \item descriptive: A \code{data.frame} (\code{data.table}) with the 
#'              summary statistics with mean for each time step, for each event
#'        \item means: A \code{data.frame} (\code{data.table}) with the mean 
#'              across the selected time steps
#'        \item sds: A \code{data.frame} (\code{data.table}) with the standard 
#'              deviation across the selected time steps      
#'          }
#'    These results are also saved to disk in two files:
#'    \itemize{ 
#'       \item descriptive_ranges.csv:  The summary statistics for each time 
#'               step, for each event     
#'        \item summary_ranges.xlsx:  the mean and standard deviation across 
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
#' @importFrom tcltk tk_choose.dir
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
  if(is.null(path.results)) path.results <- tk_choose.dir(caption = txt)
  if(length(scenarios) == 1) {
    if(scenarios == "all") 
      scenarios <- list.dirs(path=path.results, full.names=FALSE, recursive=FALSE)
  }
  
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
#' @importFrom tcltk tk_choose.dir
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
  if(is.null(path.results)) path.results <- tk_choose.dir(caption = txt)
  if(length(scenarios) == 1) {
    if(scenarios == "all") 
      scenarios <- list.dirs(path=path.results, full.names=FALSE, recursive=FALSE)
  }
  
  rep.names <- lapply(scenarios, file.names, path.results, pop.name)
  if(type == "move") {
    rep.summaries <- lapply(rep.names, move)
  } else {
    rep.summaries <- lapply(rep.names, ranges, hx, events, start, end)
  }
  
  return(rep.summaries)
}
