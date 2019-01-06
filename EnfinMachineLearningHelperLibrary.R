library(gtools)
library(openxlsx)
library(reshape2)
library(caret)

################################################################################
# Generated Template data to be filled
# (only executed once to generate template to input data)
################################################################################

# GenerateTemplateForPatientsStatus <- function(filepaths, entity) {
#   #  filepaths <- crc_filepaths
#   #  entity <- "CRC"
#   myDataFrame <- NULL
#   listOfPatients <- paste0("Patient", 1:4)
#   filepaths <- mixedsort(filepaths)
#   for (i in 1:length(filepaths)) {
#     kitId <- paste0("Kit", i)
#     for (pat in listOfPatients) {
#       myDataFrame <- rbind(myDataFrame, c(entity, kitId, basename(filepaths[i]), pat, ""))
#     }
#   }
#   colnames(myDataFrame) <- c("Entity", "Kit", "Filepath", "Patient", "Class")
#   return(myDataFrame)
# }
#
# templateDataForCRC <- GenerateTemplateForPatientsStatus(crc_filepaths,  "CRC")


# templateForCRC <- data.frame(id = as.matrix(colnames(dataPerPatientCRC)), class = "")
# templateForCLL <- data.frame(id = as.matrix(colnames(dataPerPatientCLL)), class = "")
#
# write.xlsx(templateForCRC, file = "templateDataForCRC.xlsx")
# write.xlsx(templateForCLL, file = "templateDataForCLL.xlsx")


################################################################################
# Helper functions
################################################################################

MeltDataPerPatient <- function(rawDataList) {
  #  rawDataList <- myRawData
  dataPerPatient <- NULL
  dataPerPatientColumnNames <- NULL
  #  patientIdentifers <- paste0("Patient", 1:4)
  for (kitId in names(rawDataList)) {
    #    kitId <- "Kit5"
    listOfPatients <- SortData(rawDataList[[kitId]])
    for (patient in names(listOfPatients)) {
      #      patient <- "Patient1"
      patientIdentifier <- paste(kitId, patient)

      #      patientDataNormalized <- listOfPatients[[patient]]

      meltedPatient <- melt(listOfPatients[[patient]])
      dataPerPatient <- cbind(dataPerPatient, meltedPatient[, 2])
      dataPerPatientColumnNames <- c(dataPerPatientColumnNames, patientIdentifier)
    }
  }
  colnames(dataPerPatient) <- dataPerPatientColumnNames
  colnames(dataPerPatient) <- gsub(" ", "_", dataPerPatientColumnNames, fixed = TRUE)
  return(dataPerPatient)
}


GetOrderedWells <- function() {
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
return(orderedWells)
}



MeltDataPerPatientWithRowNames <- function(rawDataList) {
  #  rawDataList <- myRawData
  dataPerPatient <- NULL
  dataPerPatientColumnNames <- NULL
  #  patientIdentifers <- paste0("Patient", 1:4)
  for (kitId in names(rawDataList)) {
    #    kitId <- "Kit5"
    listOfPatients <- SortData(rawDataList[[kitId]])
    for (patient in names(listOfPatients)) {
      #      patient <- "Patient1"
      patientIdentifier <- paste(kitId, patient)

      #      patientDataNormalized <- listOfPatients[[patient]]

      meltedPatient <- melt(listOfPatients[[patient]])
      dataPerPatient <- cbind(dataPerPatient, meltedPatient[, 2])
      dataPerPatientColumnNames <- c(dataPerPatientColumnNames, patientIdentifier)
    }
  }

  orderedWells <- GetOrderedWells()
  timeIntervals <- paste(seq(from = 0, to = 30, by = 5), ".Min", sep="")
  myRownames <- NULL
  for (i in seq_along(timeIntervals)) {
    for (j in seq_along(orderedWells)) {
      myRownames <- c(myRownames, paste(orderedWells[j], timeIntervals[i], sep="_"))
    }
  }

  rownames(dataPerPatient) <- myRownames
  colnames(dataPerPatient) <- gsub(" ", "_", dataPerPatientColumnNames, fixed = TRUE)
  return(dataPerPatient)
}

library(randomForest)
library(dplyr)
library(ggplot2)

FeatureImportancePlot <- function(myData) {
  rf_out <- randomForest(class ~ ., data = myData)
  # Extracts variable importance (Mean Decrease in Gini Index)
  # Sorts by variable importance and relevels factors to match ordering
  var_importance <- data_frame(variable = setdiff(colnames(myData), "class"),
                               importance=as.vector(importance(rf_out)))
  var_importance <- arrange(var_importance, desc(importance))
  var_importance$variable <- factor(var_importance$variable, levels=var_importance$variable)

  p <- ggplot(var_importance, aes(x=variable, weight=importance, fill=variable))
  p <- p + geom_bar() + ggtitle("Variable Importance from Random Forest Fit")
  p <- p + xlab("Feature") + ylab("Variable Importance")
  p <- p + scale_fill_discrete(name="Variable Name")
  p + theme(axis.text.x=element_blank(),
            axis.text.y=element_text(size=12),
            axis.title=element_text(size=16),
            plot.title=element_text(size=18),
            legend.title=element_text(size=16),
            legend.text=element_text(size=12))
  return(p)
}
