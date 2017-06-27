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
  
  files <- list.files(testFolder, full.names=TRUE)
  file.remove(files)
  unlink(file.path(dirname(testFolder), '/Scenarios_bkup'), 
           recursive=TRUE)
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



test_that("LHS.scenarios", {
  template <- system.file("extdata", "MRVC_4BaitYr_ThreeCells.xml", package="HexSimR")
  csv.LHS.in <- system.file("extdata", "test_csv_LHS.csv", package="HexSimR")
  testFolder <- tempdir()
  file.copy(c(template, csv.LHS.in), testFolder)
  
  LHS <- LHS.scenarios(
    path.scenarios=testFolder,
    xml.template="MRVC_4BaitYr_ThreeCells.xml",
    samples=9, 
    csv.in ="test_csv_LHS.csv",
    generate=TRUE)
  
  expect_equal(sapply(LHS[[2]], length), rep(1, 4))
  
  xml_files <-list.files(testFolder, patter="LHS[0-9].xml$", full.names=TRUE)
  xml_LHS <- vector(mode="list", length=length(xml_files))
  res <- vector(mode="list", length=length(xml_files))
  for (r in seq_along(xml_files)) {
    xml_LHS[[r]] <- read_xml(xml_files[r])
    Xpaths <- c("/scenario/population/initialSize", 
                paste0("/scenario/spatialDataSeries/name[text()='", 
                       LHS[[1]][r, 2], "']"),
                "/scenario/population/traits/accumulatedTrait[@name='BaitRates']/value[@name='Bait3km']",
                "/scenario/spatialDataSeries[name='EightCells']/cycleLength")
    nlist <- lapply(Xpaths, xml_find_all, x=xml_LHS[[r]])
    res[[r]] <- c(xml_text(nlist[[1]]) == as.character(LHS[[1]][r,1]),
                  xml_text(nlist[[2]]) == as.character(LHS[[1]][r,2]),
                  xml_attr(nlist[[3]], "threshold") == as.character(LHS[[1]][r,3]),
                  xml_text(nlist[[4]]) == as.character(LHS[[1]][r,4])
    )
  }
  
  expect_equal(sapply(res, sum), rep(4, length(xml_files)))
  unlink(testFolder, recursive=TRUE)
})

























