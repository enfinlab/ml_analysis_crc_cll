
####################################################
# Helper functions
# imported from is from Enfin kitanalyzer
####################################################
slice <- function(input, by=2) {
  starts <- seq(1,length(input),by)
  tt <- lapply(starts, function(y) input[y:(y+(by-1))])
  llply(tt, function(x) x[!is.na(x)])
}


SortData <- function(rawData) {


  #baseDirectory <- "/Users/tanovsky/wip/Enfin/KitAnalyzer/"

  #inputDataFilepath <- file.path(baseDirectory,"input")
  #outputDataFilepath <- file.path(baseDirectory, "output")

  #setwd(baseDirectory)
  #rawDataFilepath <- file.path(inputDataFilepath, "Beipiel-Rohdaten.xlsx")

  #test_that("Input file is accessible", {
  #  expect_equal(file.exists(rawDataFilepath), TRUE)
  #})

  #rawData <- read.xlsx(rawDataFilepath, colNames = TRUE, rowNames = TRUE)


  # For test
  #  rawData <- myData()

  wellColumns <- ""
  kMaxNumberOfPatients <- 4
  kColumnsPerPatient <- 3
  timeIntervals <- seq(from = 0, to = 30, by = 5)


  timeIntervalLabels <- colnames(rawData)

  # generate a vector with all the wells that should be splitted by correspondent patient
  orderedWells <- c()
  for (patient in 1:kMaxNumberOfPatients) {
    endColumn <- patient * kColumnsPerPatient
    beginColumn <- endColumn - kColumnsPerPatient + 1
    for(patientColumn in c(beginColumn: endColumn)) {
      for (wellRow in LETTERS[1:8]) {
        orderedWells <- c(orderedWells, paste0(wellRow, patientColumn))
      }
    }
  }


  ####################################################
  # Generate tables for Patients and fill with time values
  ####################################################

  patientTables <- list()
  patientIdentifier <- paste0("Patient", 1:kMaxNumberOfPatients)
  #splittedWellsPerPatient <- split(orderedWells, kMaxNumberOfPatients * )
  splittedWellsPerPatient <- slice(orderedWells, 24)
  for (i in 1:kMaxNumberOfPatients) {
    patientTables[[patientIdentifier[i]]] = data.frame(matrix(ncol = length(timeIntervalLabels),
                                                              nrow = length(orderedWells)/kMaxNumberOfPatients))
    rownames(patientTables[[patientIdentifier[i]]]) <- splittedWellsPerPatient[[i]]
    colnames(patientTables[[patientIdentifier[i]]]) <- timeIntervalLabels
    #  patientTables[[patientIdentifier[i]]] <- cbind(patientTables[[patientIdentifier[i]]], Well = rownames(patientTables[[patientIdentifier[i]]]))
    for (wellIdentifer in rownames(patientTables[[patientIdentifier[i]]])) {
      for (timeIdentifier in timeIntervalLabels) {
        patientTables[[patientIdentifier[i]]][wellIdentifer, timeIdentifier] <- rawData[wellIdentifer, timeIdentifier]
      }
    }
  }

  # Clean the empty patients..(the whole matrix has 'NA's)
  for (i in names(patientTables)) {
    if (all(is.na(patientTables[[i]]))) {
      patientTables[i] <- NULL
    }
  }

  return(patientTables)
}
