
#' Make a plot from a list of data frames
#' 
#' \code{make.plot} groups together \code{nscens_group} scenarios and plot them
#'   as facets of the same plot. If multiple groups are processed, it is possible 
#'   to indicate the sequential number of the group so that the next lot of 
#'   scenarios is process. For example, if there are 20 scenarios and the user
#'   wants 4 groups, setting \code{nscens_group=5} and passing iteratively 
#'   \code{ngroup=1:4}, the scenarios 1 to 5 will be processed first, then 6 to 
#'   10, then 11 to 15 and lastly 16 to 20.
#'    
#' @param ngroup The group number being processed 
#' @param nscens_group The number of scenarios within one group
#' @param means A list of data frames containing the mean values
#' @param sds A list of data frames containing the standard deviation values
#' @param scenarios A character vector with the scenarios' names
#' @inheritParams SSMD.census
#' @inheritParams w.genepop.batch
#' @inheritParams census.plot
#' @import ggplot2
#' @import data.table
#' @export
make.plot <- function(ngroup, nscens_group, means, sds, traits, scenarios, 
                      ncensus, rm.T0) {
  seq_scens <- 1:nscens_group + nscens_group * (ngroup - 1)
  l.d <- lapply(seq_scens[seq_scens <= length(scenarios)], 
                prep.data, means, sds, traits, scenarios, rm.T0)
  dat <- rbindlist(l=l.d, use.names=TRUE)
  limits <- aes(ymax=max, ymin=min)
  p <- ggplot(dat, aes(color=Trait, x=Time.Step, y=Mean)) +
    geom_point(size=1.2) +
    facet_wrap(~Scenario, ncol=2) +
    xlab("Time Step") +
    geom_errorbar(limits, width=0.5, size=0.1)
  save(p, file=paste0(path.results, "/", "plots_census.", ncensus, ".group.",
                      ngroup, ".rda"))
  ggsave(paste0(path.results, "/", "plots_census.", ncensus, ".group.",
             ngroup, ".pdf"), plot=p, heigh=297, width=210, unit="mm")
  return(p)
}

#' Prepare census data to generate plots
#' 
#' This function is used by other \code{HexSimR}'s functions. It combines mean 
#'   and standard deviation values in one data.table and add a column with the 
#'   scenario name. It operates on one scenario at the time.
#' 
#' @param i The scenario position to extract
#' @inheritParams make.plot
#' @inheritParams SSMD.census
#' @inheritParams census.plot
#' @import reshape2
#' @import data.table
#' @export
prep.data <- function(i, means, sds, traits, scenarios, rm.T0) {
  scen_means <- data.table(means[[i]][if(rm.T0 == TRUE) -1, ])
  scen_sds <- data.table(sds[[i]][if(rm.T0 == TRUE) -1, ])
  
  scen_m.melted <- melt(scen_means, id.vars="Time.Step", measure.vars=traits,
                        variable.name="Trait", value.name="Mean")
  scen_m.melted[, Scenario := scenarios[i]]
  
  scen_sd.melted <- melt(scen_sds, id.vars="Time.Step", measure.vars=traits,
                         variable.name="Trait", value.name="SD")

  d <- cbind(scen_m.melted, SD=scen_sd.melted[, SD])
  d[, min := Mean - SD]
  d[, max := Mean + SD]
  return(d)
}


#' Plots census means across simulated time steps
#'   
#' It takes as data input the output from \code{collate.census} (it reads data
#'   directly from xls files). 
#'   
#' @param ngroups The number of groups in which the scenarios are to be divided
#' @param rm.T0 Whether to remove the Time Step '0' (default=TRUE)
#' @inheritParams collate.census
#' @inheritParams SSMD.census
#' @inheritParams w.genepop.batch
#' @return Save to disk ggplot objects (with extension .rda) and pdf with the 
#'   plots (one for each group). Return a list of plots
#' @import XLConnect
#' @export

census.plot <- function(path.results=NULL, scenarios="all", traits, ncensus=0, 
                        ngroups=1, rm.T0=TRUE) {
  txt <- "Please, select the 'Results' folder within the workspace"
  if(is.null(path.results)) path.results <- choose.dir(caption = txt)
  suppressWarnings(if(scenarios == "all") {
    scenarios <- list.dirs(path=path.results, full.names=FALSE, recursive=FALSE)
  })
  traits <- make.names(traits)
  nscens_group <- ceiling(length(scenarios) / ngroups)
  means <-lapply(scenarios, read.means, path.results, ncensus)
  sds <-lapply(scenarios, read.sds, path.results, ncensus)
  
  l.plots <- lapply(1:ngroups, make.plot, nscens_group, means, sds, traits, 
                    scenarios, ncensus, rm.T0) 
  return(l.plots)
}

#' Plots mean progress of invasion front
#' 
#' \code{invasion.plot} plots the mean progress of invasion front for each 
#'   scenario included in the output of \code{invasion.front} (it reads data
#'   directly from xls files). 
#'
#' @inheritParams clean.genepop   
#' @return Save to disk ggplot objects (with extension .rda), a pdf with the 
#'   plot and return the plot
#' @import XLConnect
#' @import data.table
#' @import ggplot2
#' @export
invasion.plot <- function(fname=NULL) {
  if(is.null(fname)) fname <- choose.files()
  mean_data <- data.table(readWorksheetFromFile(fname, sheet="overall"))
  mean_data[, min := Mean - Std]
  mean_data[, max := Mean + Std]
  limits <- aes(ymax=max, ymin=min)
  p <- ggplot(mean_data, aes(x=Scenario, y=Mean)) +
    geom_point() +
    theme(axis.text.x=element_text(angle=-90)) +
    geom_errorbar(limits)
  save(p, file=paste0(dirname(fname), "/", "plot_invasion", ".rda"))
  ggsave(paste0(dirname(fname), "/", "plot_invasion", ".pdf"), plot=p, 
         heigh=297, width=210, unit="mm")
  return(p)
}
