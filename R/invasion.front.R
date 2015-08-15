#' Calculate the progressive occupancy of an array of hexagons
#' 
#' The user has to set up an array of patches (which may comprise of one or more 
#'   hexagons) and then determine the occupancy 
#'   of these patches during the simulations. This can be achieved by generating 
#'   a sequenced trait linked to an accumulator that it is updated with an 
#'   'individual location' updater. Finally, by setting up a census event that 
#'   summarises the number of individuals within each patch, it will be 
#'   possible to use this function to calculate the progress of the front of 
#'   invasion across the array of hexagons.
#'   
#' The census file where the data is stored is identify with the argument 
#'   \code{ncensus}. Remember that by default HexSim names the census files with
#'   the name of the scenario, followed by the 'number' of the census event. This
#'   'number' will depend on how many census event you have in the HexSim sequence.
#'   The first census event will be saved with the number 0, the second with the
#'   number 1 and so forth. The number of the census file that holds the individual
#'   location data is the one that need to be passed to \code{ncensus} so that 
#'   \code{HexSimR} knows which one to pick to carry out the calculations. 
#'     
#' @param ncensus The census number to be processed
#' @param value The threshold occupancy value 
#' @param patch.width The width of the patch 
#' @inheritParams collate.census 
#' @return A list with three elements: the mean and standard
#'   deviation for each time step and overall. 
#' @import data.table
#' @import XLConnect
#' @export
invasion.front <- function(path.results, ncensus, value=1, patch.width, 
                           scenarios="all") {
  #----------------------------------------------------------------------------#
  # Helper functions
  #----------------------------------------------------------------------------#
  
  sel.max <-function(x, value) {
    m <- max(which(x >= value))
    return(m)
  }
  
  # Read census data
  byiter <- function(iter, l.iter.folders, census.file, nscen) {
    f <- paste(l.iter.folders[[nscen]][iter], census.file, sep="/")
    census.data <- fread(f) # This previously didn't work because 
    # there is a comma at the end of header row 
    # census.data <- fread(f, skip=1)
    # h <- c("Run", "Time.Step", "Population.Size", "Group.Members",
    #       "Floaters",   "Lambda" )
    # ncols <- dim(census.data)[2]
    # traits <- paste0("TraitInd", 0:(ncols - 7))
    setnames(census.data, make.names(names(census.data)))
    sel.col <- grep(pattern = "^Trait", x = names(census.data))
    patch <- apply(census.data[, sel.col, with=FALSE], 1, sel.max, value)
    halt <- suppressWarnings(min(which(patch == length(sel.col))))
    if (halt == Inf) halt <- length(patch)
    dist.patch <- diff(patch[1:halt]) * patch.width
    return(dist.patch)
  }
  
  # Return a list where each element is one scenario
  byscen <- function (nscen, scenarios, l.iter.folders, ncensus) {
    census.file <- paste0(scenarios[[nscen]], ".", ncensus, ".", "csv")
    iters <- seq_along(l.iter.folders[[nscen]])    
    l.scen.i <- lapply(iters, byiter, l.iter.folders, census.file,  nscen)
    scen.i <- data.table(suppressWarnings(as.data.frame.list(l.scen.i)))
    setnames(scen.i, paste0("TS", 1:length(names(scen.i))))
    return(scen.i)
  }
  
  scen.mean <- function(scen.i) {
    scen.i.m <- scen.i[, lapply(.SD, mean, na.rm = TRUE)]
    return(scen.i.m)
  }
  
  scen.sd <- function(scen.i) {
    scen.i.sd <- scen.i[, lapply(.SD, sd, na.rm = TRUE)]
    return(scen.i.sd)
  }
  
  scen.overall <- function(scen.i) {
    mtx <- as.matrix(scen.i)    
    Mean <- mean(mtx, na.rm = TRUE)
    Std <- sd(mtx, na.rm = TRUE)
    scen.i.overall <- data.table(Mean, Std)
    return(scen.i.overall)
  }
  
  #----------------------------------------------------------------------------#
  txt <- "Please, select the 'Results' folder within the workspace"
  if(is.null(path.results)) path.results <- choose.dir(caption = txt)
  suppressWarnings(if(scenarios == "all") 
    scenarios <- list.dirs(path=path.results, full.names=FALSE, recursive=FALSE))
  
  l.iter.folders <- lapply(scenarios, iter.folders, dir.path=path.results)
  nscens <- seq_along(scenarios)
  # A list where each elelment is a scenario with a data.table of the progress 
  # of the front (in the same unit as patch.width) for each iteration
  progress <- lapply(nscens, byscen, scenarios=scenarios, ncensus=ncensus,
                      l.iter.folders=l.iter.folders)
  
  scen.means <- lapply(progress, scen.mean)
  scen.sds <- lapply(progress, scen.sd)
  scen.over <- lapply(progress, scen.overall)
  
  
  scen.means <- rbindlist(scen.means)
  scen.sds <- rbindlist(scen.sds)
  scen.over <- rbindlist(scen.over)
    
  Scenario <- scenarios
  scen.means <- cbind(Scenario, scen.means)
  scen.sds <- cbind(Scenario, scen.sds)
  scen.over <- cbind(Scenario, scen.over)
  
  wb.name <- paste0(path.results, "/", "Invasion.front", ".", "xlsx")
  if(file.exists(wb.name)) file.remove(wb.name)
  wb <- loadWorkbook(wb.name, create=TRUE)
  createSheet(wb, name="means")
  writeWorksheet(wb, scen.means, sheet="means")
  createSheet(wb, name="sd")
  writeWorksheet(wb, scen.sds, sheet="sd")
  createSheet(wb, name="overall")
  writeWorksheet(wb, scen.over, sheet="overall")
  saveWorkbook(wb)
  
  return(list(means=scen.means, sds=scen.sds, overall=scen.over))
}
