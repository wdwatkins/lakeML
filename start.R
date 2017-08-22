library(data.table)
library(dplyr)
rawDF <- fread('data/ML_challenge_single_system.tsv', sep = "\t")

#want output variable in last column
rawDF <- select(rawDF, dim_1, dim_2, cactus:whisper, error)
rawDF <- na.omit(rawDF) #only a handful on NAs

#rawDF <- select(rawDF, -dim_1, -dim_2, -coffee, -villan, -whisper)
library(gradDescent)
split <- splitData(rawDF, dataTrainRate = 0.5)
trainDat <- split$dataTrain
testDat <- split$dataTest

gdOb <- gradDescentR.learn(dataSet = trainDat, learningMethod = "MGD",
                           control = list(maxIter = 200, alpha = 0.01, 
                                          momentum = 0.9))
testPreds <- predict(gdOb, mutate(testDat, error = NA))
trainPreds <- predict(gdOb, mutate(trainDat, error = NA))
test_rms <- sqrt(mean((testPreds$V1 - testDat$error)^2))
train_rms <- sqrt(mean((trainPreds$V1 - trainDat$error)^2))

print(paste("Test RMSE:", test_rms))
print(paste("Train RMSE:", train_rms))
plot(testDat$error, testPreds$V1, main = "Test")
plot(trainDat$error, trainPreds$V1, main = "Training")

corrplot(cor(rawDF, use = "complete.obs", method = "kendall"))

#try nnets
library(nnet)
nn <- nnet(x = select(trainDat, -error), y = trainDat$error, 
            size = 30)
nnRMS <- sqrt(mean(nn$residuals^2))
print(paste("NN RMSE:", nnRMS))

plotVsOutVar <- function(df, var) {
  features <- names(df)[names(df) != var]
  for(f in features) {
    plot(df[[var]], df[[f]], xlab = var, ylab = paste("linear", f))
    plot(df[[var]], log(df[[f]]), xlab = var, ylab = paste("log", f))
  }
}
