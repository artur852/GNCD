mergeData <- function() {
  
  featureTable <- read.table("Data/features.txt", col.names = c("ID", "name"))
  featureNames <- as.character(featureTable$name)
  
  testDataSub <- read.table("Data/test/subject_test.txt", col.names = c("subject"))
  trainDataSub <- read.table("Data/train/subject_train.txt", col.names = c("subject"))
  
  testDataX <- read.table("Data/test/X_test.txt", col.names = featureNames)
  testDataY <- read.table("Data/test/y_test.txt", col.names = "activity")
  
  trainDataX <- read.table("Data/train/X_train.txt", col.names = featureNames)
  trainDataY <- read.table("Data/train/y_train.txt", col.names = "activity")
  
  activity <<- read.table("Data/activity_labels.txt", col.names = c("ID", "label"))
  
  fullTestData <- cbind(testDataSub, testDataX, testDataY)
  fullTrainData <- cbind(trainDataSub, trainDataX, trainDataY)

  completeData <<- rbind(fullTrainData, fullTestData)
  
  filteredData <- completeData[, grep('subject|*mean*|*std*|activity', names(completeData))]
  
  for (i in 1:nrow(filteredData)) {
    currVal <- filteredData[i, "activity"]
    filteredData[i, "activity"] <- as.character(activity[currVal, ]$label)
  }
  
  filteredData$activity <- as.factor(filteredData$activity)
  filteredData$subject <- as.factor(filteredData$subject)
  
  return (filteredData)
  
}

buildAveragedDataSet <- function(df, fileName = NULL) {
  ret <- aggregate(. ~ subject + activity, data = df, mean)
  if (!is.null(fileName)) {
    write.table(ret, fileName, row.names = FALSE)
  }
  return (ret)
}
