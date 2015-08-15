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
#'   a suffix 'cleaned' and an extensin ".gen".
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
    if(cut.off > 1) {
      infile <- infile[1:cut.off - 1]
    }
    infile[1] <- title
    writeLines(infile, con=paste0(dirname(fname),
                                  "/",
                                  sub(".txt", "", basename(fname)), 
                                  "_cleaned", ".gen"))
    return(infile)
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
#'   @inheritParams multi.reports
#'   @return A list of \code{clean.genepop} for each scenario
#'   @seealso clean.genepop
#'   @export
multi.clean.genepop <- function(path.results=NULL, scenarios="all", 
                                pop.name=NULL) {
  #----------------------------------------------------------------------------#
  # Helper functions
  #----------------------------------------------------------------------------#
  
  apply.clean.gen <- function(nscen, l.iter.folders, scenarios, pop.name) {
    iters <- seq_along(l.iter.folders[[nscen]])
    gen.names <- lapply(iters, file.names, nscen, l.iter.folders, scenarios, 
                        pop.name)
    infiles <- lapply(gen.names, byTS)
    return(infiles)
  }
  
  byTS <- function (gen.name) {
    l.infiles <- lapply(gen.name, clean.genepop)
    return(l.infiles)
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
  
  gen.files <- lapply(nscens, apply.clean.gen, l.iter.folders, scenarios, pop.name)
  
  return(gen.files)
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
#' \code{gen.dist} calculates the Joist's D between all possible population pairs
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
#' 
#' @param mean.type The type of mean to be calculated over multiple loci
#' @inheritParams clean.genepop
#' @import adegenet
#' @import mmod
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

