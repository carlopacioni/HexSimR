#' Probability of extinction
#'
#' Calculates the probability of extinction for each time step and a cumulative
#' probability between a specified interval. If the number of the requested
#' scenarios is > 1, it is possible to indicate a baseline scenario and
#' \code{Pext} will also calculate the Strictly Standardised Mean Difference
#' (SSMD, Zhang 2007).
#'
#' By default, \code{Pext} calculates the probability of extinction using the
#' column "Population Size", however any column(s) in the census file can be
#' passed with the argument \code{headers}. This allows to set different
#' criteria for extinction (e.g. no females are left) or to calculate the
#' probability of extinction for several populations at once (if these are
#' captured in different columns in the census file, see tutorial for an
#' example). By extension, \code{Pext} can therefore be used to calculate the
#' probability of any trait(s) to be zero (or non-zero by calculating 1 - Pext).
#'
#' \code{Pext} takes as input the output of \code{collate.census}, which can be
#' passed as R object with \code{data}. If  \code{data=NULL}, then a fully
#' qualified file name of the .rda file saved by \code{collate.census} needs to
#' be passed with the argument \code{rda.in}.
#'
#' When \code{start="min"} (default), it is automatically set to 1 when the
#' first time step is zero, otherwise it will be set to the minimum Time Step >
#' 0. When \code{end="max"} it is set to the maximum Time Step value.
#'
#' \strong{NOTE} that in this function SSMD is calculated as the difference
#' between the parameter of the base scenario minus the parameter of the
#' alternative scenario. This means that a negative SSMD indicates an increase
#' probability of extinction of the alternative scenario against the base
#' scenario. See the vignette for further discussion on this point.
#'
#' It is \strong{also important to note} that if both standard deviations of the
#' probabilities used to calculate the SSMD are zero (this may happen, for
#' example, when the populations never go extinct, or always go extinct), it is
#' impossible to calculate the SSMD (the denominator is zero) and the cells in
#' the result file will be blank, while if the mean probabilities are the same,
#' then SSMD=0 and p-value=0.5.
#'
#' @param data The output from \code{collate.census}
#' @param rda.in The fully qualified (i.e. including the path) name of the .rda
#'   file where the output from \code{collate.census} is saved
#' @inheritParams ranges
#' @inheritParams SSMD.census
#' @inheritParams census.calc
#' @seealso \code{\link{collate.census}}
#' @return A list with four elements: \itemize{ \item $extTable.means: A
#'   \code{data.frame} (\code{data.table}) with the mean probability of
#'   extinction for each year, for each scenario \item $extTable.sds: A
#'   \code{data.frame} (\code{data.table}) with the standard deviation of the
#'   probability of extinction for each Time Step, for each scenario \item
#'   $cumul.ext.means: A \code{data.frame} (\code{data.table}) with the mean
#'   cumulative probability of extinction from the Time Step \code{start} to
#'   \code{end}, for each scenario \item $cumul.ext.sds: A \code{data.frame}
#'   (\code{data.table}) with standard deviation of the cumulative probability
#'   of extinction from the Time Step \code{start} to \code{end}, for each
#'   scenario } These results are also saved to disk in two xls files: \itemize{
#'   \item $Pext_census:  The mean and standard deviation for each Time Step (in
#'   two separate tabs) \item $Cumulative_Pext_census:  the mean and standard
#'   deviation across the selected time steps (in two separate tabs) } The
#'   results for the SSMD comparisons are saved in the file starting with
#'   "SSMD_Cumul_Pext_census". The census number is appended to the name of all
#'   xls file.
#'
#' @references Zhang, X. D. 2007. A pair of new statistical parameters for
#' quality control in RNA interference high-throughput screening assays.
#' Genomics 89:552-561.

#' @import data.table
#' @import XLConnect
#' @export
Pext <- function(data=NULL, path.results, rda.in="collated.census.rda", 
                 scenarios="all", ncensus=0, start="min", end="max", 
                 headers="Population.Size", base=NULL) {
  #----------------------------------------------------------------------------#
  # Helper functions
  #----------------------------------------------------------------------------#
  save.xlsx <- function(path.results, wb, var1, var2, 
                        tabvar1="means", tabvar2="sd") {
    createSheet(wb, name=tabvar1)
    writeWorksheet(wb, var1, sheet=tabvar1)
    createSheet(wb, name=tabvar2)
    writeWorksheet(wb, var2, sheet=tabvar2)
    saveWorkbook(wb)
  }
  #----------------------------------------------------------------------------#
  if(is.null(data)) {
    temp.space <- new.env()
    data <- load(paste(path.results, rda.in, sep="/"), temp.space)
    data <- get(data, temp.space)
    rm(temp.space)
  }
  data <- data[[1]]
  if(scenarios != "all") data <- data[[scenarios]]
  if(scenarios == "all") scenarios <- names(data)
  headers <- make.names(headers)
  
  extTable <- list()
  extTable.means <- list()
  extTable.sds <- list()
  cumul.ext <- list()
  cumul.ext.means <- list()
  cumul.ext.sds <- list()
  for (scenario in scenarios) {
    census <- data[[scenario]][[ncensus + 1]]
    if(start == "min") {
      if(census[, min(Time.Step)] == 0) {
        s_start <- 1
        } else {
          s_start <- census[, min(Time.Step)]
          }
    } else {
      s_start <- start
  }
    if(end == "max") s_end <- census[, max(Time.Step)] else s_end <- end
    extTable[[scenario]] <- cbind(census[, .(Run, Time.Step)], 
                      census[, headers, with=FALSE] == 0)
    
    extTable.means[[scenario]] <- extTable[[scenario]][, 
                                               lapply(.SD, mean, na.rm=TRUE), 
                                               by="Time.Step", .SDcols=headers]
    extTable.means[[scenario]][, Scenario := scenario]
    setcolorder(extTable.means[[scenario]], c("Scenario", "Time.Step", headers))
    
    extTable.sds[[scenario]] <- extTable[[scenario]][, 
                                               lapply(.SD, sd, na.rm=TRUE), 
                                               by="Time.Step", .SDcols=headers]
    extTable.sds[[scenario]][, Scenario := scenario]
    setcolorder(extTable.sds[[scenario]], c("Scenario", "Time.Step", headers))
    
    setkey(extTable[[scenario]], Time.Step)
    cumul.ext[[scenario]] <- extTable[[scenario]][J(s_start:s_end), 
                                               lapply(.SD, sum, na.rm=TRUE), 
                                               by="Run", .SDcols=headers]
    cumul.ext[[scenario]][, Scenario := scenario]
    cumul.ext[[scenario]] <-  cbind(cumul.ext[[scenario]][, .(Scenario)],
                        cumul.ext[[scenario]][, headers, with=FALSE] > 0)
    cumul.ext.means[[scenario]] <- cumul.ext[[scenario]][, 
                                              lapply(.SD, mean, na.rm=TRUE), 
                                              by="Scenario"]
    cumul.ext.sds[[scenario]] <- cumul.ext[[scenario]][, 
                                              lapply(.SD, sd, na.rm=TRUE), 
                                              by="Scenario"]
  }
  
  extTable.means <- rbindlist(extTable.means, use.names=TRUE)
  extTable.sds <- rbindlist(extTable.sds, use.names=TRUE)
  
  cumul.ext.means <- rbindlist(cumul.ext.means, use.names=TRUE)
  cumul.ext.sds <- rbindlist(cumul.ext.sds, use.names=TRUE)
  
  # SSMD
  if(length(scenarios) > 1 & !is.null(base)) {
    setkey(cumul.ext.means, Scenario)
    mbase <- cumul.ext.means[base, (headers), with=FALSE]
    setkey(cumul.ext.sds, Scenario)
    sdbase <- cumul.ext.sds[base, (headers), with=FALSE]
    
    mdata <- cumul.ext.means[!base, (headers), with=FALSE]
    sddata <- cumul.ext.sds[!base, (headers), with=FALSE]
    
    ssmd <- (mbase - mdata) / sqrt(sddata^2 + sdbase^2)
    
    pv <-  data.table(HexSimR:::pval(ssmd))
    pv[, Scenario := cumul.ext.means[!base, Scenario]]
    setcolorder(pv, c("Scenario", headers))
    
    ssmd[, Scenario := cumul.ext.means[!base, Scenario]]
    setcolorder(ssmd, c("Scenario", headers))
    
    wb <- loadWorkbook(paste(path.results, sep="/",
                             paste0("SSMD_Cumul_Pext_census", ncensus, ".xlsx")), 
                       create=TRUE)
    save.xlsx(path.results, wb, var1=ssmd, var2=pv, tabvar1="SSMD", tabvar2="pvalues")
  }
  
  
  wb <- loadWorkbook(paste(path.results, sep="/",
                           paste0("Pext_census", ncensus, ".xlsx")), 
                     create=TRUE)
  save.xlsx(path.results, wb, var1=extTable.means, var2=extTable.sds)
  
  wb <- loadWorkbook(paste(path.results, sep="/",
                           paste0("Cumulative_Pext_census", ncensus, ".xlsx")), 
                     create=TRUE)
  save.xlsx(path.results, wb, var1=cumul.ext.means, var2=cumul.ext.sds)
  
  return(list(extTable.means=extTable.means, extTable.sds=extTable.sds, 
              cumul.ext.means=cumul.ext.means, cumul.ext.sds=cumul.ext.sds))
}