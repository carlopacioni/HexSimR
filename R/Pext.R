#' Probability of extinction
#'
#' Calculates the probability of extinction for each time step, the mean
#' probability of extinction across all time steps (and across all replicates),
#' and the cumulative probability of extinction (that is, the probability that a
#' populaition goes extinct in at least one time step). Specific time step
#' intervals can be specified. If the number of the requested scenarios is > 1,
#' it is possible to indicate a baseline scenario and \code{Pext} will also
#' calculate the Strictly Standardised Mean Difference (SSMD, Zhang 2007).
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
#' When \code{start="min"} (default), the first time step considered is
#' automatically set to 1 if the first time step is zero, otherwise it will be
#' set to the minimum Time Step > 0. When \code{end="max"}, the last time step
#' considered is set to the maximum Time Step value.
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
#' @return A list with four elements: \itemize{ \item $means.Pext.time.step: A
#'   \code{data.frame} (\code{data.table}) with the mean probability of
#'   extinction for each time step, for each scenario \item $sds.Pext.time.step:
#'   A \code{data.frame} (\code{data.table}) with the standard deviation of the
#'   probability of extinction for each time step, for each scenario \item
#'   $cumul.Pext.means: A \code{data.frame} (\code{data.table}) with the mean
#'   cumulative probability of extinction from the Time Step \code{start} to
#'   \code{end}, for each scenario \item $cumul.Pext.sds: A \code{data.frame}
#'   (\code{data.table}) with standard deviation of the cumulative probability
#'   of extinction from the Time Step \code{start} to \code{end}, for each
#'   scenario  \item $means.time.step.Pext: A \code{data.frame}
#'   (\code{data.table}) with the mean probability of extinction across all time
#'   steps, for each scenario \item $sds.time.step.Pext: A \code{data.frame}
#'   (\code{data.table}) with the standard deviation of the probability of
#'   extinction across all time steps, for each scenario}
#'
#'   These results are also saved to disk in two xls files: \itemize{ \item
#'   $Pext/time.step_census:  The mean and standard deviation for each Time Step
#'   (in two separate tabs) \item $Cumulative_Pext_census:  the mean and
#'   standard deviation of the cumulative probability of extinction for the
#'   selected time step interval (in two separate tabs) \item
#'   $Pext.time.step_census: The mean and standard deviation of the time step
#'   probability of extinction across all time steps within the selected
#'   interval (in two separate tabs) } The results for the SSMD comparisons are
#'   saved in the file starting with "SSMD_Cumul_Pext_census" or
#'   SSMD_means.time.step.Pext. The census number is appended to the name of all
#'   xls file.
#'
#' @references Zhang, X. D. 2007. A pair of new statistical parameters for
#'   quality control in RNA interference high-throughput screening assays.
#'   Genomics 89:552-561.

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
  
  apply.ssmd <- function(means, sds, base, headers) {
    setkey(means, Scenario)
    mbase <- means[base, (headers), with=FALSE]
    setkey(sds, Scenario)
    sdbase <- sds[base, (headers), with=FALSE]
    
    mdata <- means[!base, (headers), with=FALSE]
    sddata <- sds[!base, (headers), with=FALSE]
    
    ssmd <- (mbase - mdata) / sqrt(sddata^2 + sdbase^2)
    
    pv <-  data.table(HexSimR::pval(ssmd))
    pv[, Scenario := means[!base, Scenario]]
    setcolorder(pv, c("Scenario", headers))
    
    ssmd[, Scenario := means[!base, Scenario]]
    setcolorder(ssmd, c("Scenario", headers))
    
    wb <- loadWorkbook(
      file.path(path.results, paste0("SSMD_", deparse(substitute(means)), 
                    "_census", ncensus, ".xlsx")), 
                       create=TRUE)
    save.xlsx(path.results, wb, var1=ssmd, var2=pv, tabvar1="SSMD", tabvar2="pvalues")
    
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
  
  time.step.Pext <- list()
  mean.Pext.time.step <- list()
  sd.Pext.time.step <- list()
  cumul.Pext <- list()
  cumul.Pext.means <- list()
  cumul.Pext.sds <- list()
  mean.time.step.Pext <- list()
  sd.time.step.Pext <- list()
  
  for (scenario in scenarios) {
    census <- data[[scenario]][[grep(paste0("\\.", ncensus, "$"), names(data[[scenario]]))]]
    if(start == "min") {
      if(census[, min(Time.Step)] == 0) {
        s_start <- 1
        } else {
          s_start <- census[, min(Time.Step)]
          }
    } else {
      if(start < census[, min(Time.Step)]) 
        stop(paste("The value of 'start' is lower than the first Time Step in the scenario",
                   scenario))
      s_start <- start
  }
    if(end == "max") {
      s_end <- census[, max(Time.Step)] 
      } else {
        if(end > census[, max(Time.Step)]) 
          stop(paste("The value of 'end' is higher than the last Time Step in the scenario",
                     scenario))
        s_end <- end
      }
        
    
    mTS <- census[, max(Time.Step), by=Run]
    if(length(mTS[, unique(V1)]) > 1)
      stop(paste("Different number of time steps between replicates in", scenario),
           "\nPlease, ensure you ran 'collate.census' with the appropriate value for the argument 'max'")
    
    
    time.step.Pext[[scenario]] <- cbind(census[, .(Run, Time.Step)], 
                      census[, headers, with=FALSE] == 0)
    
    mean.Pext.time.step[[scenario]] <- time.step.Pext[[scenario]][, 
                                               lapply(.SD, mean, na.rm=TRUE), 
                                               by="Time.Step", .SDcols=headers]
    mean.Pext.time.step[[scenario]][, Scenario := scenario]
    setcolorder(mean.Pext.time.step[[scenario]], c("Scenario", "Time.Step", headers))
    
    sd.Pext.time.step[[scenario]] <- time.step.Pext[[scenario]][, 
                                               lapply(.SD, sd, na.rm=TRUE), 
                                               by="Time.Step", .SDcols=headers]
    sd.Pext.time.step[[scenario]][, Scenario := scenario]
    setcolorder(sd.Pext.time.step[[scenario]], c("Scenario", "Time.Step", headers))
    
    setkey(time.step.Pext[[scenario]], Time.Step)
   cumul.Pext[[scenario]] <- time.step.Pext[[scenario]][J(s_start:s_end), 
                                               lapply(.SD, sum, na.rm=TRUE), 
                                               by="Run", .SDcols=headers]
   cumul.Pext[[scenario]][, Scenario := scenario]
   cumul.Pext[[scenario]] <-  cbind(cumul.Pext[[scenario]][, .(Scenario)],
                       cumul.Pext[[scenario]][, headers, with=FALSE] > 0)
   cumul.Pext.means[[scenario]] <-cumul.Pext[[scenario]][, 
                                              lapply(.SD, mean, na.rm=TRUE), 
                                              by="Scenario"]
   cumul.Pext.sds[[scenario]] <-cumul.Pext[[scenario]][, 
                                              lapply(.SD, sd, na.rm=TRUE), 
                                              by="Scenario"]
    
   mean.time.step.Pext[[scenario]] <- time.step.Pext[[scenario]][J(s_start:s_end), 
                                                    lapply(.SD, mean, na.rm=TRUE),
                                                    .SDcols=headers]
   mean.time.step.Pext[[scenario]][, Scenario := scenario]
    setcolorder(mean.time.step.Pext[[scenario]], c("Scenario", headers))
    
    sd.time.step.Pext[[scenario]] <- time.step.Pext[[scenario]][J(s_start:s_end), 
                                                    lapply(.SD, sd, na.rm=TRUE), 
                                                    .SDcols=headers]
    sd.time.step.Pext[[scenario]][, Scenario := scenario]
    setcolorder(sd.time.step.Pext[[scenario]], c("Scenario", headers))
  }
  
  means.Pext.time.step <- rbindlist(mean.Pext.time.step, use.names=TRUE)
  sds.Pext.time.step <- rbindlist(sd.Pext.time.step, use.names=TRUE)
  
 cumul.Pext.means <- rbindlist(cumul.Pext.means, use.names=TRUE)
 cumul.Pext.sds <- rbindlist(cumul.Pext.sds, use.names=TRUE)
  
 means.time.step.Pext <- rbindlist(mean.time.step.Pext, use.names=TRUE)
 sds.time.step.Pext <- rbindlist(sd.time.step.Pext, use.names=TRUE)
  
  # SSMD
  if(length(scenarios) > 1 & !is.null(base)) {
    apply.ssmd(means=cumul.Pext.means, sds=cumul.Pext.sds, base, headers)
    apply.ssmd(means=means.time.step.Pext, sds=sds.time.step.Pext, base, headers)
  }
  
  wb <- loadWorkbook(file.path(path.results, 
                          paste0("Pext.time.step_census", ncensus, ".xlsx")), 
                     create=TRUE)
  save.xlsx(path.results, wb, var1=means.Pext.time.step, var2=sds.Pext.time.step)
  
  wb <- loadWorkbook(file.path(path.results, 
                               paste0("Cumulative_Pext_census", ncensus, ".xlsx")), 
                     create=TRUE)
  save.xlsx(path.results, wb, var1=cumul.Pext.means, var2=cumul.Pext.sds)

  wb <- loadWorkbook(file.path(path.results, 
                               paste0("Time_step_Pext_census", ncensus, ".xlsx")), 
                     create=TRUE)
  save.xlsx(path.results, wb, var1=means.time.step.Pext, var2=sds.time.step.Pext)
  
  return(list(means.Pext.time.step=means.Pext.time.step, sds.Pext.time.step=sds.Pext.time.step, 
              cumul.Pext.means=cumul.Pext.means, cumul.Pext.sds=cumul.Pext.sds,
             means.time.step.Pext=means.time.step.Pext, sds.time.step.Pext=sds.time.step.Pext))
}