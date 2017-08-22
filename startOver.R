library(data.table)
library(dplyr)
rawDF <- fread('data/ML_challenge_raw_data.tsv', sep = "\t")

#want output variable in last column
rawDF <- select(rawDF, dim_1, dim_2, cactus:whisper, error)
rawDF <- na.omit(rawDF) #only a handful on NAs

# preProcOb <- preProcess(x = rawDF, method = c("center", "scale"))
# rawDF <- predict(preProcOb, rawDF)

library(caret)
#split training and test data
trainIndex <- createDataPartition(rawDF$error, p = 0.8, list = FALSE)

testDat <- rawDF[-trainIndex,]
trainDat <- rawDF
rmsDF <- data.frame()

#for learning curve
for(i in seq(from = 1, to = 1, by = 0.2)) {
  #trainDat <- rawDF[trainIndex,]
  #subTrainingIndex <- createDataPartition(trainDat$error, p = 1, list = FALSE)
  #trainDat <- trainDat[subTrainingIndex,]
  modelOb <- train(x = select(trainDat, -error), 
                   y = trainDat$error, method = "xgbLinear")
  plot(modelOb)
  trainPreds <- predict(modelOb, trainDat)
  testPreds <- predict(modelOb, testDat)
  print("Test Data:")
  testRMSE <- postResample(pred = testPreds, obs = testDat$error)
  print(testRMSE)
  print("Training Data:")
  trainRMSE <- postResample(pred = trainPreds, obs = trainDat$error)
  print(trainRMSE)
  
  #plot results
  plot(trainDat$error, trainPreds, ylab = "predicted", 
       xlab = "actual", pch = 20)
  points(testDat$error, testPreds, col = "red", pch = 20)
  
  #store in DF
  # rowDF <- data.frame(t(c(trainRMSE, testRMSE, i, nrow(trainDat))))
  # names(rowDF) <- c("trainRMSE", "trainRsq", "testRMSE", "testRsq",
  #                   "fracTrainingDat", "nTrain")
  # rmsDF <- bind_rows(rmsDF, rowDF)
  # print(paste("done", i))
}