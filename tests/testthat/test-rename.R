library(HexSimR)
library(data.table)
context("rename replicates")

test_that("rename replicates", {
  
  test.file <- system.file("extdata", "Results.zip", package="HexSimR")
  testFolder <- tempdir(check=TRUE)
  file.copy(test.file, testFolder)
  unzip(test.file, exdir=testFolder)
  path.results <- file.path(testFolder, "Results")
  scen <- "DingoBaseSBF_SelDist"
  ren <- rename.replicates(path.results, scenario = scen, suffix=3:4)
  ld <- list.dirs(file.path(path.results, scen))
  r <- regexec(pattern = "\\[[0-9]]$", ld)
  numbs <- regmatches(ld, r)
  
  path.census <- file.path(path.results, "DingoBaseSBF_SelDist", 
                           "DingoBaseSBF_SelDist-[3]")
  ls <- list.files(path.census, "[0-9]+.csv$", full.names = TRUE)
  census <- fread(ls[[1]])
  census2 <- fread(ls[[4]])
  
  expect_equal(length(ld), 3)
  expect_equal(numbs[[2]], "[3]")
  expect_equal(numbs[[3]], "[4]")
  expect_equal(length(ls), 4)
  expect_equal(census[, unique(Run)], 3)
  expect_equal(census2[, unique(Run)], 3)
  
  files <- list.files(file.path(testFolder), full.names=TRUE, recursive=TRUE)
  file.remove(files)
  unlink(testFolder, recursive=TRUE)
})