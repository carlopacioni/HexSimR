#' Remove illegal characters from HexSim generated genepop input file
#' 
#' HexSim includes a 'structure block' at the end of its genepop input file
#'   that are not recognised by genepop. This block is removed by \code{clean.genepop}
#'   Also, while the animal IDs between brackets should not create a problem in
#'   genepop, these may be an 'unexpected column' for other applications that 
#'   reads genepop input files (e.g. the R package diveRsity). Similarly, the 
#'   space between the word 'Trait' and the trait number, and the equal and column
#'   symbols in the first line of the file can cause unexpected 
#'   behaviours in some applications. All these unusual features are removed from
#'   the HexSim generated files, which are then re-saved with the same name,  
#'   a suffix "cleaned" and an extensin ".gen".
#'    
#' @param fname A character vector with the name of the input file including the 
#'   path
#' @param title A character vector to be used to replace the first line in the file
#' @return Save to disk a cleaned up genepop input file and a character vector
#' @export      
  clean.genepop  <-  function(fname, title=NULL) {
    if(is.null(title)) title <- "Title missing"
    rl <- readLines(fname)
    infile <- gsub(pattern=" \\(.*\\)", replacement="", rl)
    infile <- gsub(pattern="Trait ", replacement="Trait", infile)
    cut.off <- grep(pattern="^\\[\\[Structure", infile)
    if(length(cut.off) > 0) infile <- infile[1:cut.off - 1]
    
    infile[1] <- title
    writeLines(infile, con=paste0(dirname(fname),
                                  "/",
                                  sub(".txt", "", basename(fname)), 
                                  "_cleaned", ".gen"))
  }

#' Remove illegal characters from several HexSim generated genepop input files
#' 
#' \code{multi.clean.genepop} is a wrapper for \code{clean.genepop} that processes
#'   all HexSim generated genepop files within a given scenario(s). It assumes 
#'   that the names of the report files were 
#'   not changed from defaults. It reads the files in 
#'   each of the scenario's iteration subfolder and processes them   
#'   using \code{clean.genepop}.
#'   
#' \code{multi.clean.genepop} identifies the files searching for a text file 
#'   whose root's name is the \emph{scenario_name} of the file and the text 
#'   'REPORT_genepop', so if you have modified the name of the files, this 
#'   function won't work. Similarly, if you have added files named with a pattern
#'   consistent with what described above, the function will try to process these
#'   as well.   
#'   
#' When using \code{multi.clean.genepop} there is no option to choose the title 
#'   of the genepop files 
#'   (i.e. the first line of the genepop input file).
#'   
#' @inheritParams multi.reports
#' @return Save to disk a genepop file with a suffix 'cleaned' and an extensin
#'      ".gen" for each iteration within eachscenario
#' @seealso clean.genepop
#' @export
multi.clean.genepop <- function(path.results=NULL, scenarios="all", 
                                pop.name=NULL) {
  #----------------------------------------------------------------------------#
  # Helper functions
  #----------------------------------------------------------------------------#
  
  apply.clean.gen <- function(nscen, l.iter.folders, scenarios, pop.name) {
    message(paste("Processing scenario:", scenarios[nscen]))
    iters <- seq_along(l.iter.folders[[nscen]])
    gen.names <- lapply(iters, file.names, nscen, l.iter.folders, scenarios, 
                        pop.name)
    lapply(gen.names, byTS)
  }
  
  byTS <- function (gen.name) {
    m<-regexpr("\\[[[:digit:]]+\\]", text = gen.name[1])
    txt <- regmatches(gen.name[1], m)
    message(paste("Replicate:", txt))
    lapply(gen.name, clean.genepop)
  }
  
  file.names <- function(iter, nscen, l.iter.folders, scenarios, pop.name) {
    n <- list.files(l.iter.folders[[nscen]][iter], 
                    pattern=paste0(scenarios[nscen], "_REPORT_genepop_", 
                                   pop.name, ".*", "txt$"), full.names=TRUE)
    return(n)
  }
  #----------------------------------------------------------------------------#
  
  if(is.null(pop.name)) stop("Please, provide the name of the population")
  txt <- "Please, select the 'Results' folder within the workspace"
  if(is.null(path.results)) path.results <- choose.dir(caption = txt)
  suppressWarnings(if(scenarios == "all") 
    scenarios <- list.dirs(path=path.results, full.names=FALSE, recursive=FALSE))
  
  l.iter.folders <- lapply(scenarios, iter.folders, dir.path=path.results)
  nscens <- seq_along(scenarios)
  
  lapply(nscens, apply.clean.gen, l.iter.folders, scenarios, pop.name)
}


#' Write a .xml file to generate genepop input files in batch mode for all 
#' replicates of given scenario(s)
#' 
#' Generate a batch .xml file in the workspace directory that will instruct 
#'   OutputTransformer.exe to generate genepop input files for all replicates of 
#'   the scenario(s) passed with \code{scenarios}. If \code{scenarios="all"} is
#'   used, then all the scenarios will be included.
#' 
#' @param time.steps A numeric vector to indicate the time step to be included
#' @param traits A character vector with the name of the traits to be included 
#' @inheritParams collate.census 
#' @inheritParams multi.reports 
#' @return A .xml file named batchFile_genepop_Reports.xml
#' @export
w.genepop.batch <- function(path.results=NULL, scenarios="all", time.steps=1, 
                              pop.name, traits) {
  #----------------------------------------------------------------------------#
  # Helper functions
  #----------------------------------------------------------------------------#
  gen.input <- function(nscen, l.iter.folders, time.steps, pop.name, traits, 
                        scenarios) {
    iters <- seq_along(l.iter.folders[[nscen]])
    scenario <- scenarios[nscen]
    iter_TS <- lapply(iters, byiter, paths=l.iter.folders[[nscen]], time.steps, 
                      pop.name, traits, scenario)
    return(iter_TS)
  }
  
  byiter <- function(iter, paths, time.steps, pop.name, traits, scenario) {
    path <- paths[iter]
    TS <- lapply(time.steps, gen.block, pop.name, traits, scenario, path)
    return(TS)
  }
  
  gen.block <- function(time.step, pop.name, traits, scenario, path) {
    open.args <- "  <args>" 
    open.arg <- "    <arg>"
    close.arg <- "</arg>"
    close.args <- "  </args>"
    arg1 <- paste0("-genepop:", time.step, ":\"", pop.name, "\":\"", traits, "\"")
    log.file <- paste0(scenario, ".log")
    arg2 <- paste(path, log.file, sep="\\")
    block <- c(open.args, 
               paste0(open.arg, arg1, close.arg),
               paste0(open.arg, arg2, close.arg),
               close.args)
    return(block)
  }
  
  #----------------------------------------------------------------------------#
  
  if(is.null(path.results)) path.results <- choose.dir(caption = txt)
  suppressWarnings(if(scenarios == "all") 
    scenarios <- list.dirs(path=path.results, full.names=FALSE, recursive=FALSE))
  l.iter.folders <- lapply(scenarios, iter.folders, dir.path=path.results)
  nscens <- seq_along(scenarios)
  traits <- paste0(traits, collapse="\":\"" )
  
  fl <- "<?xml version=\"1.0\"?>"
  open.block <- "<OutputTransform>"
  close.block <- "</OutputTransform>"
  scen_iter_TS <- lapply(nscens, gen.input, l.iter.folders, time.steps, pop.name, 
                         traits, scenarios)
  
  wspace <- sub(pattern = "Results", replacement = "", path.results)
  writeLines(c(fl, open.block, unlist(scen_iter_TS), close.block),
             con=paste0(wspace, "batchFile_genepop_Reports.xml")) 
}
#' Genetic distance
#' 
#' \code{gen.dist} calculates the Jost's D between all possible population pairs
#'    using the R package \code{mmod}.
#'    
#' This function takes the output from the function \code{clean.genepop} 
#'   (or \code{multi.clean.genepop}) as input and uses the R package \code{adegenet}
#'   to convert the input in a genind object. 
#'     
#' A text file with extension .pairD that contains a pairwise distance  matrix 
#'   is saved to disk.
#'   
#' \code{mean.type} can be set to either "arithmetic" or "harmonic" (default), 
#'   which is used to calculate the global estimates of Hs and Ht (see 
#'   \code{mmod}'s manual for details).
#' @references Jost, L.O.U., 2008. GST and its relatives do not measure 
#'   differentiation. Molecular Ecology 17, 4015-4026.
#' @references Winter, D.J., 2012. mmod: an R library for the calculation of 
#'   population differentiation statistics. Molecular Ecology Resources 12, 
#'   1158-1160.
#'   
#' @param mean.type The type of mean to be calculated over multiple loci
#' @inheritParams clean.genepop
#' @import adegenet
#' @import mmod
#' @return Save to disk a file with same root of the input file with extension 
#'   .pairD
#' @export
gen.dist <- function(fname, mean.type="harmonic") {
  adegen.data <- adegenet::read.genepop(fname, ncode=3L, quiet=TRUE)
  gen.dist <- pairwise_D(adegen.data, hsht_mean=mean.type)
  dir.out <- dirname(fname)
  dist.name <- basename(fname)
  dist.name <- sub("REPORT_genepop_", "", dist.name)
  dist.name <- sub("_cleaned.gen", "", dist.name)
  dist.name <- paste(dist.name, "pairD", sep=".")
  write.csv(as.matrix(gen.dist), file=paste0(dir.out, "/", dist.name))
}

#' Genetic distance for multiple scenarios
#' 
#' \code{multi.gen.dist} is a wrapper for \code{gen.dist} that generates
#'   a matrix of genetic distances for multiple scenarios. 
#'   
#' \code{multi.gen.dist} will generate a matrix for each replicate within the scenario's folder.
#'   
#' @inheritParams multi.reports
#' @inheritParams gen.dist
#' @return See \code{gen.dist} for each scenario
#' @seealso gen.dist
#' @export
multi.gen.dist <- function(path.results=NULL, scenarios="all", pop.name=NULL,
                           mean.type="harmonic") {
  #----------------------------------------------------------------------------#
  # Helper functions
  #----------------------------------------------------------------------------#
  
  apply.gen.dist <- function(nscen, l.iter.folders, scenarios, pop.name) {
    message(paste("Processing scenario:", scenarios[nscen]))
    iters <- seq_along(l.iter.folders[[nscen]])
    gen.names <- lapply(iters, file.names, nscen, l.iter.folders, scenarios, 
                        pop.name)
    distances <- lapply(gen.names, byTS)
    return(distances)
  }
  
  byTS <- function (gen.name) {
    l.infiles <- lapply(gen.name, gen.dist, mean.type)
    return(l.infiles)
  }
  
  file.names <- function(iter, nscen, l.iter.folders, scenarios, pop.name) {
    fn <- list.files(l.iter.folders[[nscen]][iter], 
                    pattern=paste0(scenarios[nscen], "_REPORT_genepop_", pop.name,
                                    ".*", "_cleaned.gen$"), full.names=TRUE)
    return(fn)
  }
  #----------------------------------------------------------------------------#
  
  if(is.null(pop.name)) stop("Please, provide the name of the population")
  txt <- "Please, select the 'Results' folder within the workspace"
  if(is.null(path.results)) path.results <- choose.dir(caption = txt)
  suppressWarnings(if(scenarios == "all") 
    scenarios <- list.dirs(path=path.results, full.names=FALSE, recursive=FALSE))
  
  l.iter.folders <- lapply(scenarios, iter.folders, dir.path=path.results)
  nscens <- seq_along(scenarios)
  
  lapply(nscens, apply.gen.dist, l.iter.folders, scenarios, pop.name)
  }

#' Mean genetic distance
#' 
#' This function calculates the mean and standard deviation of the genetic 
#'   distance (calculated with \code{gen.dist}) across all replicates for the same
#'   scenario.
#' 
#' @inheritParams collate.census 
#' @inheritParams multi.reports
#' @inheritParams invasion.front
#' @inheritParams w.genepop.batch
#' @return A list with three elements: the mean and standard
#'   deviation for each time step and overall. \code{m.gen.dist} also saves to 
#'   disk a .xlsx with the same name of the input file with the suffix "_means". 
#' @import XLConnect
#' @export
m.gen.dist <- function(path.results=NULL, scenarios="all", pop.name, 
                          traits) {
  #----------------------------------------------------------------------------#
  # Helper functions
  #----------------------------------------------------------------------------#
  # Return a list where each element is one scenario
  byscen <- function (nscen, scenarios, l.iter.folders, pop.name, traits, path.results) {
    fnames <- list.files(path=l.iter.folders[[nscen]][1], 
                         pattern=paste0(scenarios[[nscen]], "_", pop.name,
                                        "_[0-9]+_", traits, ".pairD"))
    iters <- seq_along(l.iter.folders[[nscen]]) 
    l.TS.i <- lapply(fnames, byTS, iters, l.iter.folders, nscen, path.results,
                     scenarios)
    return(l.TS.i)
  }
  
  byTS <- function (fname, iters, l.iter.folders, nscen, path.results, scenarios) {
    l.data <- lapply(iters, read.data, fname, l.iter.folders, nscen)
    outer.len <- length(l.data)
    inner.len <-  dim(l.data[[1]])
    arr <- array( unlist(l.data) , c(inner.len, outer.len) )
    dist.means <- apply(arr, 1:2, mean, na.rm=TRUE)
    dist.sds <- apply(arr, 1:2, sd, na.rm=TRUE)
    
    means.dist.name <- sub(pattern=".pairD", replacement="_means", fname)
    wb.name <- paste0(path.results, "/", scenarios[nscen], "/", 
                      means.dist.name, ".xlsx")
    if(file.exists(wb.name)) file.remove(wb.name)
    wb <- loadWorkbook(wb.name, create=TRUE)
    createSheet(wb, name="means")
    writeWorksheet(wb, dist.means, sheet="means")
    createSheet(wb, name="sd")
    writeWorksheet(wb, dist.sds, sheet="sd")
    saveWorkbook(wb)    
    return(list(dist.means=dist.means, dist.means=dist.sds))
  }
  
  read.data <- function(iter, fname, l.iter.folders, nscen) {
    f <- paste(l.iter.folders[[nscen]][iter], fname, sep="/")
    dist.data <- as.matrix(read.csv(f, row.names=1)) 
    return(dist.data)
  }
  
  trait.assembler <- function(traits) {
    comb <- unlist(lapply(traits, function(trait) paste0("\\[", trait, "\\]")))
    comb <- paste0(comb, collapse="")
  }
  #----------------------------------------------------------------------------#
  txt <- "Please, select the 'Results' folder within the workspace"
  if(is.null(path.results)) path.results <- choose.dir(caption = txt)
  suppressWarnings(if(scenarios == "all") 
    scenarios <- list.dirs(path=path.results, full.names=FALSE, recursive=FALSE))
  traits <- trait.assembler(traits)
  
  l.iter.folders <- lapply(scenarios, iter.folders, dir.path=path.results)
  nscens <- seq_along(scenarios)
  means.gen.dist <- lapply(nscens, byscen, scenarios=scenarios, pop.name, 
                           traits=traits, l.iter.folders=l.iter.folders, 
                           path.results)
  
  return(means.gen.dist)
}

