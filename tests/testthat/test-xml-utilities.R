library(HexSimR)
library(xml2)
context("xml_utilities")

test_that("scenarios.batch.modifier", {
  new <- system.file("extdata", "MRVC_4BaitYr_ThreeCells.xml", package="HexSimR")
  old <- system.file("extdata", "MRVC_4BaitYr_ThreeCells_old.xml", package="HexSimR")
  csv.in <- system.file("extdata", "test_csv.csv", package="HexSimR")
  testFolder <- tempdir()
  file.copy(c(new, old, csv.in), testFolder)
  
  XMLTest <- scenarios.batch.modifier(
    path.scenarios=testFolder,
    scenarios="MRVC_4BaitYr_ThreeCells_old.xml", 
    xml.template="MRVC_4BaitYr_ThreeCells.xml",
    csv.in="test_csv.csv")
  test_new <- read_xml(file.path(testFolder, "MRVC_4BaitYr_ThreeCells.xml"))
  test_old <- read_xml(file.path(testFolder, "MRVC_4BaitYr_ThreeCells_old.xml"))
  expect_equal(test_new, test_old)
  
  unlink(testFolder, recursive = TRUE)
  
})

test_that("make.Xpath", {
  nodes_paths <- c("/scenario/population/initialSize", 
                   "/scenario/spatialDataSeries/name",                 
                   "/scenario/spatialDataSeries/name", 
                   "/scenario/population/traits/accumulatedTrait/name",
                   "/scenario/spatialDataSeries/name")
  identifiers <- c("FALSE", "Fence_sections_2perm", "Shooting_5", 
                   "BaitRates", "EightCells" )
  attribs <- c(FALSE, FALSE, FALSE,  TRUE, FALSE)
  test_norm <- list()
  for(i in 1:5) {
    test_norm[[i]] <- make.Xpath(nodes_paths[i], identifiers[i], attribs[i])
  }
  
  expect_equal(test_norm[[2]], 
               "/scenario/spatialDataSeries[name='Fence_sections_2perm']")
  expect_equal(test_norm[[4]], 
               "/scenario/population/traits/accumulatedTrait[@name='BaitRates']")
  
  
})