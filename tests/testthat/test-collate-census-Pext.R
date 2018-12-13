library(HexSimR)
library(xml2)
context("collate.census and Pext")

test_that("collate.census and Pext", {
  
  test.file <- system.file("extdata", "PextTest.zip", package="HexSimR")
  testFolder <- tempdir(check=TRUE)
  file.copy(test.file, testFolder)
  unzip(test.file, exdir=testFolder)
  path.results <- file.path(testFolder, "PextTest", "PextTest")
  collall<-collate.census(path.results, end=30)
  collext<-collate.census(path.results, end=30, keep.zeros=FALSE)
  expect_warning(collmax<-collate.census(path.results))
  
  
  expect_equal(dim(collall[[2]][[2]][[1]])[1], 31)
  expect_equal(dim(collext[[2]][[2]][[1]])[1], 31)
  expect_equal(dim(collmax[[2]][[2]][[1]])[1], 30)
  
  expect_equal(collall[[2]][[2]][[1]][22, Population.Size], 2149)
  expect_equal(collall[[2]][[2]][[1]][23, Population.Size], 2130)
  expect_equal(collext[[2]][[2]][[1]][22, Population.Size], 4298)
  expect_equal(collmax[[2]][[2]][[1]][22, Population.Size], 2149)
  expect_equal(collmax[[2]][[2]][[1]][23, Population.Size], 4260)
  
  peall <- Pext(collall, path.results=path.results, ncensus=0, 
                headers=c("Population.Size", "Inside"), base="Sqkm5_LHS9")
  
  peext <- Pext(collext, path.results=path.results, ncensus=0, headers="Inside")
  
  expect_error(Pext(collmax, path.results=path.results, ncensus=0, headers="Inside"))
  
  peallmm <- Pext(collall, path.results=path.results, ncensus=0, start=5, end=29,
                headers=c("Population.Size", "Inside"), base="Sqkm5_LHS9")
  
  expect_equal(peall$cumul.Pext.means$Population.Size, c(0, 1))
  expect_equal(peall$cumul.Pext.means$Inside, c(1, 1))
  expect_equal(round(peall$means.time.step.Pext$Inside, 2), c(0.35, 0.32))
  
  expect_equal(peext$cumul.Pext.means$Inside, c(1, 1))
  expect_equal(round(peext$means.time.step.Pext$Inside, 2), c(0.35, 0.32))
  
  expect_equal(peallmm$cumul.Pext.means$Inside, c(0.5, 1.0))
  expect_equal(round(peallmm$means.time.step.Pext$Inside, 2), c(0.28, 0.34))
  
  
  files <- list.files(file.path(testFolder), full.names=TRUE, recursive=TRUE)
  file.remove(files)
  unlink(testFolder, recursive=TRUE)
})