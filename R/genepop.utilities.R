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