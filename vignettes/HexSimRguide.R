## ---- echo=FALSE, out.width=690, fig.cap="__Figure 1.__ The HexSim generated map of the study area. Yellow; inside the fence; Green: outside."----
knitr::include_graphics("Map.tif")

## ------------------------------------------------------------------------
library(HexSimR, quietly = TRUE)
example_dir <- system.file("extdata", "Results", package="HexSimR")

## ------------------------------------------------------------------------
duplicate <- tempdir()
file.copy(from= example_dir, to=duplicate, recursive=TRUE)


## ------------------------------------------------------------------------
# HexSimR is expecting to be pointed to the Results folder
path.results <- paste(duplicate, "Results", sep="/")

# total pop size inside and outside
temp <- census.calc(path.results, ncensus=2, 
                     headers=c("Trait Index  2", "Trait Index  3"), 
                     var.name = "Inside", bin.f = "+", scenarios = "all")
temp <- census.calc(path.results, ncensus=2, headers=c("Trait Index  4", "Trait Index  5"), 
            var.name = "Outside", bin.f = "+", scenarios = "all")

# Calculate the means and SDs
coll.census <- collate.census(path.results, scenarios="all")


