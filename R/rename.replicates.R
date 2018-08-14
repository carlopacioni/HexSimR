#' Rename (renumber) replicate folders within a scenario
#'
#' This function replaces the numbers between square brackets at the end of the
#' folder names of replicates within one scenario with the numbers provided in
#' \code{suffix}
#'
#' \code{rename.replicates} can process one scenario at the time. Processing of
#' multiple scenarios with the same suffix should be achieved using a loop (e.g.
#' lapply). \code{suffix} should be of the same length as the number of
#' replicates.
#'
#' \bold{Note} that the order in which the original folder names are replaced
#' may not be progressive. For example, normally the replicate '10' is renamed
#' after replicate '1' and before '2'. Also, note that the run number stored in
#' census files or other outputs is not changed.
#'
#' @inheritParams collate.census
#' @param scenario The scenario whose replicates need to be renamed
#' @param suffix An integer vector to rename the replicates
#' @importFrom tcltk tk_choose.dir
#' @export

rename.replicates <- function(path.results=NULL, scenario, suffix) {
  txt <- "Please, select the 'Results' folder within the workspace"
  if(is.null(path.results)) path.results <- tk_choose.dir(caption = txt)
  
  l.iter.folders <- iter.folders(dir.path=path.results, scenario=scenario)
  if(length(l.iter.folders) != length(suffix)) 
    stop(paste("Different number of replicates found and suffixes provided.\n", 
               "Found", length(l.iter.folders), "replicates, but", length(suffix), " suffixes provided."))
  
  new.nms <- mapply(sub, l.iter.folders, 
                    replacement=paste0("[", suffix, "]"), 
                    MoreArgs = list(pattern="[[0-9]+]$"))
  exist.checks <- sapply(new.nms, file.exists)
  
  if(any(exist.checks)) stop("Can't rename folders because at least some already exist")
  message(paste("Found", length(l.iter.folders), "replicates to be renamed."))
  file.rename(from=l.iter.folders, to=new.nms)
  return(list(original.folders=l.iter.folders, new.folders=new.nms))
}