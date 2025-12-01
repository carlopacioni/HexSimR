#' Read one genepop
#' 
#' This function takes the output from the function \code{clean.genepop} 
#'   (or \code{multi.clean.genepop}) as input and uses the R package \code{adegenet}
#'   to convert the input in a genind object. 
#'   
#' @inheritParams clean.genepop
#' @import adegenet
#' 
read.one.gen <- function(fname) {
  suppressWarnings(
  adegen.data <- adegenet::read.genepop(fname, ncode=3L, quiet=TRUE)
  )
}

#' Genetic Diversity
#' 
#' \code{gen.div} calculates genetic diversity measurements
#'    using the R package \code{dartRverse}.
#'    
#' This function takes as input a genind object
#'   
#' @param gi A genind object
#' @import dartRverse
#' @import dartR.base
#' @return The output from gl.report.heterozygosity
#' @export
gen.div <- function(gi) {
  gl <- gi2gl(gi, verbose=0)
  gen.d <- gl.report.heterozygosity(gl, plot.display=FALSE, verbose=0)
  return(gen.d)
  }

#' Read multiple genepop files 
#' 
#' \code{multi.read.gen} is a wrapper for \code{read.one.gen} to read all genepop
#' files for one or more scenarios
#'   
#'   
#' @inheritParams multi.reports
#' @return A list with one element for each Scenario,with withing a list of genind obj
#' @seealso gen.dist
#' @importFrom tcltk tk_choose.dir
#' @export
read.multi.gen <- function(path.results=NULL, scenarios="all", pop.name=NULL) {
  #----------------------------------------------------------------------------#
  # Helper functions
  #----------------------------------------------------------------------------#
  
  apply.read.gen <- function(nscen, l.iter.folders, scenarios, pop.name) {
    message(paste("Processing scenario:", scenarios[nscen]))
    iters <- seq_along(l.iter.folders[[nscen]])
    gen.names <- lapply(iters, file.names, nscen, l.iter.folders, scenarios, 
                        pop.name)
    genpops <- lapply(gen.names, read.one.gen)
    return(genpops)
  }
  
  # byTS <- function (gen.name) {
  #   l.infiles <- lapply(gen.name, read.one.gen)
  #   return(l.infiles)
  # }
  
  file.names <- function(iter, nscen, l.iter.folders, scenarios, pop.name) {
    fn <- list.files(l.iter.folders[[nscen]][iter], 
                    pattern=paste0(scenarios[nscen], "_REPORT_genepop_", pop.name,
                                    ".*", "_cleaned.gen$"), full.names=TRUE)
    return(fn)
  }
  
  #----------------------------------------------------------------------------#
  
  if(is.null(pop.name)) stop("Please, provide the name of the population")
  txt <- "Please, select the 'Results' folder within the workspace"
  if(length(scenarios) == 1) {
    if(scenarios == "all") 
      scenarios <- list.dirs(path=path.results, full.names=FALSE, recursive=FALSE)
  }
  
  l.iter.folders <- lapply(scenarios, iter.folders, dir.path=path.results)
  nscens <- seq_along(scenarios)
  
  lapply(nscens, apply.read.gen, l.iter.folders, scenarios, pop.name)
  }

#' Mean genetic diversity
#' 
#' This function calculates the mean and standard deviation of the genetic 
#'   diversity (calculated with \code{gen.div}) across all replicates for the same
#'   scenario, possibly repeated for multiple scenarios.
#' 
#' @inheritParams collate.census 
#' @inheritParams multi.reports
#' @inheritParams invasion.front
#' @inheritParams w.genepop.batch
#' @return A list with three elements: the mean and standard
#'   deviation for each time step and overall. \code{m.gen.dist} also saves to 
#'   disk a .xlsx with the same name of the input file with the suffix "_means". 
#' @import XLConnect
#' @importFrom tcltk tk_choose.dir
#' @export
m.gen.div <- function(path.results=NULL, scenarios="all", pop.name, 
                          traits) {

  txt <- "Please, select the 'Results' folder within the workspace"
  if(is.null(path.results)) path.results <- tk_choose.dir(caption=txt)
  if(length(scenarios) == 1) {
    if(scenarios == "all") 
      scenarios <- list.dirs(path=path.results, full.names=FALSE, recursive=FALSE)
  }
  
  gis <- read.multi.gen(path.results=path.results, scenarios=scenarios, pop.name=pop.name)
  
  l.gen.div <- lapply(gis, function(x) {
    return(lapply(x, gen.div))
  })
  
  hetResSim <- lapply(l.gen.div, rbindlist, use.names=TRUE)
  hetResSum <- lapply(hetResSim, FUN=function(x) {
    x[, .(n.Loc=mean(n.Loc), Ho=round(mean(Ho), 4), HoSD=round(sd(Ho), 4), 
          He=round(mean(He), 4), HeSD=round(sd(He), 4),
          uHe=round(mean(uHe), 4), uHeSD=round(sd(uHe), 4)), by=pop]
  })
  names(hetResSum) <- scenarios
  hetResSum <- rbindlist(hetResSum, idcol="Scenario")
  hetResSum[, `:=`(Ho.ul=round(Ho + 1.96*HoSD, 4), Ho.ll=round(Ho - 1.96*HoSD, 4), 
                   He.ul=round(He + 1.96*HeSD, 4), He.ll=round(He - 1.96*HeSD, 4),
                   uHe.ul=round(uHe + 1.96*uHeSD, 4), uHe.ll=round(uHe - 1.96*uHeSD, 4))]
  setcolorder(hetResSum, c("Scenario", "pop", "n.Loc", 
                           "Ho", "HoSD","Ho.ul", "Ho.ll",
                           "He", "HeSD", "He.ul", "He.ll",
                           "uHe", "uHeSD", "uHe.ul", "uHe.ll"))
  
  return(hetResSum)
}

