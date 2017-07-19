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
  nodes_paths <- c("/scenario/spatialDataSeries/name", 
                   "/scenario/population/traits/accumulatedTrait/name")
  identifiers <- c("Shooting_5", "BaitRates")
  attribs <- c(FALSE, TRUE)
  test_norm <- list()
  for(i in seq_along(nodes_paths)) {
    test_norm[[i]] <- make.Xpath(nodes_paths[i], identifiers[i], attrib=attribs[i])
  }
  
  expect_equal(test_norm[[1]], 
               "/scenario/spatialDataSeries[name='Shooting_5']")
  expect_equal(test_norm[[2]], 
               "/scenario/population/traits/accumulatedTrait[@name='BaitRates']")
})

test_that("make.Xpaths", {
  nodes_paths <- c("/scenario/simulationParameters/timesteps", 
                   "/scenario/population/name",
                   "/scenario/spatialDataSeries/name", 
                   "/scenario/population/traits/accumulatedTrait/name",
                   "/scenario/globalVariables/globalVariable/Name")
  identifiers <- c("FALSE", "Dingoes", "Shooting_5", "BaitRates", "SurvMeanPups")
  attribs <- c(FALSE, FALSE, FALSE, TRUE, TRUE)
  param_nodes <- c(NA, "/initialSize", NA, "/value/name", NA)
  param_node_identifiers <- c(FALSE, FALSE, FALSE, "Bait3km", FALSE)
  param_node_attributes <- c(FALSE, FALSE, FALSE, TRUE, FALSE)
  param_identifiers <- c(FALSE, FALSE, FALSE, "Threshold", "Value")
  test_norm <- list()
  Xpaths <- vector("list", length(nodes_paths))
  test_norm <- make.Xpaths(nodes_paths, Xpaths, identifiers, attrib=attribs)
  
  expect_equal(test_norm[[1]], "/scenario/simulationParameters/timesteps")
  expect_equal(test_norm[[2]], "/scenario/population[name='Dingoes']")
  expect_equal(test_norm[[3]], "/scenario/spatialDataSeries[name='Shooting_5']")
  expect_equal(test_norm[[4]], 
               "/scenario/population/traits/accumulatedTrait[@name='BaitRates']")
  
  test_LHS <- make.Xpaths(nodes_paths, Xpaths, identifiers, attrib=attribs, 
                          param_nodes = param_nodes,
                          param_node_identifiers = param_node_identifiers, 
                          param_node_attributes = param_node_attributes,
                          param_identifiers = param_identifiers, is.LHS = TRUE)
  
  expect_equal(test_LHS[[1]], "/scenario/simulationParameters/timesteps")
  
  expect_equal(test_LHS[[2]], "/scenario/population[name='Dingoes']/initialSize")
  
  expect_equal(test_LHS[[3]], 
               "/scenario/spatialDataSeries/name[text()='Shooting_5']")
  
  expect_equal(test_LHS[[4]], 
   "/scenario/population/traits/accumulatedTrait[@name='BaitRates']/value[@name='Bait3km']")
  
  expect_equal(test_LHS[[5]], 
               "/scenario/globalVariables/globalVariable[@Name='SurvMeanPups']")
})


test_that("LHS.scenarios", {
  template <- system.file("extdata", "MRVC_4BaitYr_ThreeCells.xml", package="HexSimR")
  csv.LHS.in <- system.file("extdata", "test_csv_LHS.csv", package="HexSimR")
  csv.LHS.condChangesin <- 
    system.file("extdata", "LHS_condChanges.csv", package="HexSimR")
  csv.LHS.condChanges_lookup01in <- 
    system.file("extdata", "Test_LHS_condChangesLookup_01.csv", package="HexSimR")
  
  testFolder <- tempdir()
  file.copy(c(template, csv.LHS.in, 
              csv.LHS.condChangesin, csv.LHS.condChanges_lookup01in), testFolder)
  csv_file <- read.csv(file=file.path(testFolder, "test_csv_LHS.csv"), 
                       stringsAsFactors=FALSE)
  
  LHS <- LHS.scenarios(
    path.scenarios=testFolder,
    xml.template="MRVC_4BaitYr_ThreeCells.xml",
    samples=9, 
    csv.in ="test_csv_LHS.csv",
    generate=TRUE)
  
  expect_equal(sapply(LHS[[2]], length), rep(1, nrow(csv_file)))
  
  xml_files <-list.files(testFolder, pattern="LHS[0-9].xml$", full.names=TRUE)
  xml_LHS <- vector(mode="list", length=length(xml_files))
  res <- vector(mode="list", length=length(xml_files))
  for (r in seq_along(xml_files)) {
    xml_LHS[[r]] <- read_xml(xml_files[r])
    Xpaths <- c("/scenario/simulationParameters/timesteps",                                                      
                "/scenario/population[name='Dingoes']/initialSize",                                      
                "/scenario/population[name='Dingoes']/rangeParameters/rangeSpatialData",                 
                paste0("/scenario/spatialDataSeries/name[text()='", LHS[[1]][r, 4], "']"),                       
                "/scenario/population/traits/accumulatedTrait[@name='BaitRates']/value[@name='Bait3km']",
                "/scenario/spatialDataSeries[name='EightCells']/cycleLength",                            
                "/scenario/globalVariables/globalVariable[@Name='SurvMeanPups']")  
      
    nlist <- lapply(Xpaths, xml_find_all, x=xml_LHS[[r]])
    res[[r]] <- c(xml_text(nlist[[1]]) == as.character(LHS[[1]][r,1]),
                  xml_text(nlist[[2]]) == as.character(LHS[[1]][r,2]),
                  xml_text(nlist[[3]]) == as.character(LHS[[1]][r,3]),
                  xml_text(nlist[[4]]) == as.character(LHS[[1]][r,4]),
                  xml_attr(nlist[[5]], "threshold") == as.character(LHS[[1]][r,5]),
                  xml_text(nlist[[6]]) == as.character(LHS[[1]][r,6]),
                  xml_attr(nlist[[7]], "Value") == as.character(LHS[[1]][r,7])
    )
  }
  
  expect_equal(sapply(res, sum), rep(nrow(csv_file), length(xml_files)))
  
  xml.cond.replacement(path.scenarios = testFolder, 
                       scenarios = basename(xml_files), 
                       csv.in = "LHS_condChanges.csv", 
                       lookup = "Test_LHS_condChangesLookup_01.csv")
  
  res <- vector(mode="list", length=length(xml_files))
  
  for (r in seq_along(xml_files)) {
    xml_LHS[[r]] <- read_xml(xml_files[r])
    Xpaths <- c("/scenario/population[name='Dingoes']/rangeParameters/rangeSpatialData")  
    
    nlist <- lapply(Xpaths, xml_find_all, x=xml_LHS[[r]])
    res[[r]] <- c(xml_text(nlist[[1]]) == "ThisIsATest"
    )
  }
  
  chk <- as.character(LHS[[1]][, 4]) == "Shooting_5"
  expect_equal(as.logical(sapply(res, sum)), chk)
  unlink(testFolder, recursive=TRUE)
  
})
























