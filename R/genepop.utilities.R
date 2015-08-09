#' Remove illegal characters from HexSim generated genepop input file
#' 
#' HexSim includes a 'structure block' at the end of its genepop input file
#'   that are not recognised by genepop. This block is removed by \code{clean.genepop}
#'   Also, while the animal IDs between brackets should not create a problem in
#'   genepop, these may be an 'unexpected column' for other applications that 
#'   reads genepop input files (e.g. the R package diveRsity). Similarly, the 
#'   space between the word 'Trait' and the trait number, and the equal and column
#'   symbols in the first line of the file can cause unexpected 
#'   behaviour in some applications. All these unusual features are removed from
#'   the HexSim generated files, which are then re-saved with the same name and 
#'   a suffix 'cleaned'.
#'    
#' @param fname A charater vector with the name of the file
#' @param title A charater vector to be used to replace the first line in the file
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
                                  "_cleaned", ".txt"))
    return(infile)
  }


#' Write a batch .xml file to generate genepop input files for all replicates of 
#'   given scenario(s)
#' 
#' Generate a batch .xml file in the workspace directory that will instruct 
#'   OutputTransformer.exe to generate genepop input files for all replicates of 
#'   the scenario(s) passed with \code{scenarios}. If \code{scenarios="all"} is
#'   used, then all the scenairos will be inlcuded.
#' 
#' @param time.steps A numeric vector to indicate the time step to be included
#' @param traits A character vector with the name of the traints to be included 
#' @inheritParams collate.census 
#' @inheritParams multi.reports 
#' @return A .xml file named batchFile_genepop_Reports.xml
#' @export
w.genepop.batch <- function(path.results, scenarios="all", time.steps=1, 
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
  if(scenarios == "all") 
    scenarios <- list.dirs(path=path.results, full.names=FALSE, recursive=FALSE)
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