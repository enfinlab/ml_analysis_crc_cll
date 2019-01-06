library(gtools)
library(openxlsx)
library(reshape2)
library(caret)
library(e1071)
library(plyr)
library(DMwR)
library(randomForest)
library(nnet)
library(C50)

source(file.path("SetEnvironment.R"))
source(file.path("SetEnfinKitAnalyzerDependencies.R"))
source(file.path("EnfinMachineLearningHelperLibrary.R"))

################################################################################
################################################################################
################################################################################
#  CLL
################################################################################
################################################################################
################################################################################
LBP_CLL_BaseDirectory <- file.path(dataDirectory, "LBP_CLL")

cll_filepaths <- list.files(LBP_CLL_BaseDirectory,
                            recursive = TRUE,
                            full.names = TRUE,
                            include.dirs = TRUE,
                            pattern = "*.xlsx")

myRawData <- list()
for (i in 1:length(cll_filepaths)) {
  sheetNames <- getSheetNames(cll_filepaths[i])
  for (j in 1:length(sheetNames)) {
    sheetNamesFixed <- gsub(" ", "", sheetNames[j])
    myRawData[[sheetNamesFixed]] <- read.xlsx(cll_filepaths[i], sheetNames[j], rowNames = TRUE)
  }
}

dataPerPatientCLL <- MeltDataPerPatient(myRawData)

################################################################################
# Importing and adding classes
################################################################################

dataPerPatientCLLforClassification <- as.data.frame(t(dataPerPatientCLL))

# Preparing the aditional data from classification and regression
classesForCLL <- read.xlsx(file.path(dataDirectory, "templateDataForCLL_gg.xlsx"))
classesForCLL[classesForCLL == "na"] <- NA
classesForCLL$start <- convertToDate(classesForCLL$start)
classesForCLL$end <- convertToDate(classesForCLL$end)
classesForCLL$TFSdays <- classesForCLL$end - classesForCLL$start

dataPerPatientCLLforClassification <- NA
dataPerPatientCLLforClassification <- rbind(dataPerPatientCLL, classesForCLL$class)
dataPerPatientCLLforClassification <- dataPerPatientCLLforClassification[, !is.na(dataPerPatientCLLforClassification[nrow(dataPerPatientCLLforClassification),])]
dataPerPatientCLLforClassification <- t(dataPerPatientCLLforClassification)
class(dataPerPatientCLLforClassification) <- "numeric"


colnames(dataPerPatientCLLforClassification) <- c(paste0("f", 1:168), "class")
dataPerPatientCLLforClassification <- as.data.frame(dataPerPatientCLLforClassification)
dataPerPatientCLLforClassification$class <- factor(dataPerPatientCLLforClassification$class)


newData <- dataPerPatientCLLforClassification

mySeed <- 3233
#mySeed <- 1
set.seed(mySeed)
proportion <- 0.7

kNUMBER_OF_EXECUTIONS  <- 5
svm_radialConfusionMatrices <- list()
svm_linearConfusionMatrices <- list()
rf_ConfusionMatrices <- list()
pcaNNet_ConfusionMatrices <- list()
avNNet_ConfusionMatrices <- list()
C50_ConfusionMatrices <- list()


for (exec in 1:kNUMBER_OF_EXECUTIONS) {

  intrain <- createDataPartition(y = newData$class, p = proportion, list = FALSE)

  training <- newData[intrain,]
  testing <- newData[-intrain,]
  training[["class"]] = factor(training[["class"]])

  trctrl_repeatedcv <- trainControl(
    method = "repeatedcv",
    number = 10,
    repeats = 3)

  trctrl <- trctrl_repeatedcv

  grid <- expand.grid(C = c(0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 1, 1.25, 1.5, 1.75, 2,5))

  svm_Linear <- train(class ~., data = training,
                      method = "svmLinear",
                      trControl = trctrl,
                      preProcess = c("center", "scale"),
                      tuneGrid = grid,
                      tuneLength = 10)

  svm_svm_radial <- train(class ~., data = training,
                          method = "svmRadial",   # Radial kernel
                          #                       tuneLength = 9, # 9 values of the cost function
                          preProc = c("center","scale"),  # Center and scale data
                          trControl=trctrl)

  rf_random <- train(class ~., data = training,
                     method = "rf",   # Random forest
                     preProc = c("center","scale"),  # Center and scale data
                     trControl = trctrl)

  pcaNNet <- train(class ~., data = training,
                   method = "pcaNNet",   # pcaNNet
                   preProc = c("center","scale"),
                   trControl = trctrl)

  avNNet <- train(class ~., data = training,
                   method = "avNNet",   # pcaNNet
                   preProc = "pca",  # Center and scale data
                   trControl = trctrl)

  C50 <- train(class ~., data = training,
                  method = "C5.0",   #
                  trControl = trctrl)

  test_pred_svm_radial <- predict(svm_svm_radial, newdata = testing)
  test_pred_svm_linear <- predict(svm_Linear, newdata = testing)
  test_pred_rf <- predict(rf_random, newdata = testing)
  test_pred_pcaNNet <- predict(pcaNNet, newdata = testing)
  test_pred_avNNet <- predict(avNNet, newdata = testing)
  test_pred_C50 <- predict(C50, newdata = testing)

  svm_radialConfusionMatrix <- confusionMatrix(test_pred_svm_radial, testing$class, positive = "1")
  svm_linearConfusionMatrix <- confusionMatrix(test_pred_svm_linear, testing$class, positive = "1")
  rf_ConfusionMatrix <- confusionMatrix(test_pred_rf, testing$class, positive = "1")
  pcaNNet_ConfusionMatrix <- confusionMatrix(test_pred_pcaNNet, testing$class, positive = "1")
  avNNet_ConfusionMatrix <- confusionMatrix(test_pred_avNNet, testing$class, positive = "1")
  C50_ConfusionMatrix <- confusionMatrix(test_pred_C50, testing$class, positive = "1")

  svm_radialConfusionMatrices[[exec]] <- svm_radialConfusionMatrix
  svm_linearConfusionMatrices[[exec]] <- svm_linearConfusionMatrix
  rf_ConfusionMatrices[[exec]] <- rf_ConfusionMatrix
  pcaNNet_ConfusionMatrices[[exec]] <- pcaNNet_ConfusionMatrix
  avNNet_ConfusionMatrices[[exec]] <- avNNet_ConfusionMatrix
  C50_ConfusionMatrices[[exec]] <- C50_ConfusionMatrix

}

GetAccuracyFromConfusionMatrixList <- function(confusionMatrixList) {
  helper <- lapply(confusionMatrixList, function(x) {
    c(x$overall['Accuracy'],
      x$byClass)})
  result <- matrix(unlist(helper), ncol = 12, byrow = TRUE)
  colnames(result) <- names(helper[[1]])
  result
}

svm_radial_metrics <- GetAccuracyFromConfusionMatrixList(svm_radialConfusionMatrices)
svm_linear_metrics <- GetAccuracyFromConfusionMatrixList(svm_linearConfusionMatrices)
rf_metrics <- GetAccuracyFromConfusionMatrixList(rf_ConfusionMatrices)
pcaNNet_metrics <- GetAccuracyFromConfusionMatrixList(pcaNNet_ConfusionMatrices)
avNNet_metrics <- GetAccuracyFromConfusionMatrixList(avNNet_ConfusionMatrices)
C50_metrics <- GetAccuracyFromConfusionMatrixList(C50_ConfusionMatrices)

svm_radial_mean_metrics <- colMeans(svm_radial_metrics)
svm_linear_mean_metrics <- colMeans(svm_linear_metrics)
rf_mean_metrics <- colMeans(rf_metrics)
pcaNNet_mean_metrics <- colMeans(pcaNNet_metrics)
avNNet_mean_metrics <- colMeans(avNNet_metrics)
C50_mean_metrics <- colMeans(C50_metrics)

svm_radial_metrics <- rbind(svm_radial_metrics, svm_radial_mean_metrics)
svm_linear_metrics <- rbind(svm_linear_metrics, svm_linear_mean_metrics)
rf_metrics <- rbind(rf_metrics, mean = rf_mean_metrics)
pcaNNet_metrics <- rbind(pcaNNet_metrics, mean = pcaNNet_mean_metrics)
avNNet_metrics <- rbind(avNNet_metrics, mean = avNNet_mean_metrics)
C50_metrics <- rbind(C50_metrics, mean = C50_mean_metrics)

confusionMatrix(test_pred_svm_radial, testing$class, positive = "1")
confusionMatrix(test_pred_svm_linear, testing$class, positive = "1")
confusionMatrix(test_pred_rf, testing$class, positive = "1")
confusionMatrix(test_pred_pcaNNet, testing$class, positive = "1")
confusionMatrix(test_pred_avNNet, testing$class, positive = "1")
confusionMatrix(test_pred_C50, testing$class, positive = "1")



################################################################################
################################################################################
################################################################################
#  CRC
################################################################################
################################################################################
################################################################################
impact_CRC_baseDirectory <- file.path(dataDirectory, "IMPACT_EnFin")

crc_filepaths <- list.files(impact_CRC_baseDirectory,
                            recursive = TRUE,
                            full.names = TRUE,
                            include.dirs = TRUE,
                            pattern = "*.xlsx")


# TODO clarify this file that is not supposed to be here..
crc_filepaths <- crc_filepaths[!grepl("CRC BA IMP", crc_filepaths)]

dataPerPatientCRC <- NULL
dataPerPatientCRCColumnNames <- NULL
listOfPlattesIdentifiers <- paste0("Platte ", 1:(length(crc_filepaths)), ",")
for (platteIdentifier in listOfPlattesIdentifiers) {
  kitFilepath <- grepl(platteIdentifier, crc_filepaths)
  rawData <- read.xlsx(crc_filepaths[kitFilepath], rowNames = TRUE)
  listOfPatients <- SortData(rawData)
  for(patient in names(listOfPatients)) {
    patientIdentifier <- paste(platteIdentifier, patient)
    meltedPatient <- melt(listOfPatients[patient])
    dataPerPatientCRC <- cbind(dataPerPatientCRC, meltedPatient[, 2])
    dataPerPatientCRCColumnNames <- c(dataPerPatientCRCColumnNames, patientIdentifier)
  }
}
colnames(dataPerPatientCRC) <- dataPerPatientCRCColumnNames
colnames(dataPerPatientCRC) <- gsub(",", "_", gsub(" ", "", colnames(dataPerPatientCRC), fixed = TRUE), fixed = TRUE)
colnames(dataPerPatientCRC) <- gsub("Platte", "Kit", colnames(dataPerPatientCRC), fixed = TRUE)


daysPerYear <- 365
threshold <- 2 * daysPerYear

classesForCRC <- read.xlsx(file.path(dataDirectory, "templateDataForCRC_gg.xlsx"))

dataPerPatientCRCforClassification <- as.data.frame(t(dataPerPatientCRC))
# classification is representing recurrenceWithinTwoYears
dataPerPatientCRCforClassification <- cbind(dataPerPatientCRCforClassification, class = as.integer(classesForCRC$DFS < threshold))
dataPerPatientCRCforClassification$class <- factor(dataPerPatientCRCforClassification$class)

newData <- dataPerPatientCRCforClassification


mySeed <- 3233
set.seed(mySeed)
proportion <- 0.7

kNUMBER_OF_EXECUTIONS  <- 5
svm_radialConfusionMatrices <- list()
svm_linearConfusionMatrices <- list()
rf_ConfusionMatrices <- list()
pcaNNet_ConfusionMatrices <- list()
avNNet_ConfusionMatrices <- list()
C50_ConfusionMatrices <- list()


for (exec in 1:kNUMBER_OF_EXECUTIONS) {
  intrain <- createDataPartition(y = newData$class, p = proportion, list = FALSE)

  training <- newData[intrain,]
  testing <- newData[-intrain,]
  training[["class"]] = factor(training[["class"]])

  trctrl_repeatedcv <- trainControl(
    method = "repeatedcv",
    number = 10,
    repeats = 10,
    sampling = "smote")

  trctrl <- trctrl_repeatedcv

  grid <- expand.grid(C = c(0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 1, 1.25, 1.5, 1.75, 2,5))

  svm_Linear <- train(class ~., data = training,
                      method = "svmLinear",
                      trControl = trctrl,
                      preProcess = c("center", "scale"),
                      tuneGrid = grid,
                      tuneLength = 10)

  svm_svm_radial <- train(class ~., data = training,
                          method = "svmRadial",   # Radial kernel
                          #                       tuneLength = 9, # 9 values of the cost function
                          preProc = c("center","scale"),  # Center and scale data
                          trControl=trctrl)

  rf_random <- train(class ~., data = training,
                     method = "rf",   # Random forest
                     preProc = c("center","scale"),  # Center and scale data
                     trControl = trctrl)

  pcaNNet <- train(class ~., data = training,
                   method = "pcaNNet",   # pcaNNet
                   preProc = c("center","scale"),
                   trControl = trctrl)

  avNNet <- train(class ~., data = training,
                  method = "avNNet",   # pcaNNet
                  preProc = "pca",  # Center and scale data
                  trControl = trctrl)

  C50 <- train(class ~., data = training,
               method = "C5.0",   #
               trControl = trctrl)

  test_pred_svm_radial <- predict(svm_svm_radial, newdata = testing)
  test_pred_svm_linear <- predict(svm_Linear, newdata = testing)
  test_pred_rf <- predict(rf_random, newdata = testing)
  test_pred_pcaNNet <- predict(pcaNNet, newdata = testing)
  test_pred_avNNet <- predict(avNNet, newdata = testing)
  test_pred_C50 <- predict(C50, newdata = testing)

  svm_radialConfusionMatrix <- confusionMatrix(test_pred_svm_radial, testing$class, positive = "1")
  svm_linearConfusionMatrix <- confusionMatrix(test_pred_svm_linear, testing$class, positive = "1")
  rf_ConfusionMatrix <- confusionMatrix(test_pred_rf, testing$class, positive = "1")
  pcaNNet_ConfusionMatrix <- confusionMatrix(test_pred_pcaNNet, testing$class, positive = "1")
  avNNet_ConfusionMatrix <- confusionMatrix(test_pred_avNNet, testing$class, positive = "1")
  C50_ConfusionMatrix <- confusionMatrix(test_pred_C50, testing$class, positive = "1")

  svm_radialConfusionMatrices[[exec]] <- svm_radialConfusionMatrix
  svm_linearConfusionMatrices[[exec]] <- svm_linearConfusionMatrix
  rf_ConfusionMatrices[[exec]] <- rf_ConfusionMatrix
  pcaNNet_ConfusionMatrices[[exec]] <- pcaNNet_ConfusionMatrix
  avNNet_ConfusionMatrices[[exec]] <- avNNet_ConfusionMatrix
  C50_ConfusionMatrices[[exec]] <- C50_ConfusionMatrix

}

GetAccuracyFromConfusionMatrixList <- function(confusionMatrixList) {
  helper <- lapply(confusionMatrixList, function(x) {
    c(x$overall['Accuracy'],
      x$byClass)})
  result <- matrix(unlist(helper), ncol = 12, byrow = TRUE)
  colnames(result) <- names(helper[[1]])
  result
}

svm_radial_metrics <- GetAccuracyFromConfusionMatrixList(svm_radialConfusionMatrices)
svm_linear_metrics <- GetAccuracyFromConfusionMatrixList(svm_linearConfusionMatrices)
rf_metrics <- GetAccuracyFromConfusionMatrixList(rf_ConfusionMatrices)
pcaNNet_metrics <- GetAccuracyFromConfusionMatrixList(pcaNNet_ConfusionMatrices)
avNNet_metrics <- GetAccuracyFromConfusionMatrixList(avNNet_ConfusionMatrices)
C50_metrics <- GetAccuracyFromConfusionMatrixList(C50_ConfusionMatrices)

svm_radial_mean_metrics <- colMeans(svm_radial_metrics)
svm_linear_mean_metrics <- colMeans(svm_linear_metrics)
rf_mean_metrics <- colMeans(rf_metrics)
pcaNNet_mean_metrics <- colMeans(pcaNNet_metrics)
avNNet_mean_metrics <- colMeans(avNNet_metrics)
C50_mean_metrics <- colMeans(C50_metrics)

svm_radial_metrics <- rbind(svm_radial_metrics, svm_radial_mean_metrics)
svm_linear_metrics <- rbind(svm_linear_metrics, svm_linear_mean_metrics)
rf_metrics <- rbind(rf_metrics, mean = rf_mean_metrics)
pcaNNet_metrics <- rbind(pcaNNet_metrics, mean = pcaNNet_mean_metrics)
avNNet_metrics <- rbind(avNNet_metrics, mean = avNNet_mean_metrics)
C50_metrics <- rbind(C50_metrics, mean = C50_mean_metrics)

confusionMatrix(test_pred_svm_radial, testing$class, positive = "1")
confusionMatrix(test_pred_svm_linear, testing$class, positive = "1")
confusionMatrix(test_pred_rf, testing$class, positive = "1")
confusionMatrix(test_pred_pcaNNet, testing$class, positive = "1")
confusionMatrix(test_pred_avNNet, testing$class, positive = "1")
confusionMatrix(test_pred_C50, testing$class, positive = "1")
