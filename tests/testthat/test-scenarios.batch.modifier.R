library(HexSimR)
library(xml2)
context("scenarios.batch.modifier")

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
  
  unlink(duplicate, recursive = TRUE)
  
})