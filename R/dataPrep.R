library(data.table)

importData <- function() {
  trainingData <<- fread('data/train.csv')
  testData <<- fread('data/test.csv')
}

setupFactorsInData <- function() {
  trainingData$Sex <<- as.factor(trainingData$Sex)
  trainingData$Embarked <<- as.factor(trainingData$Embarked)
  trainingData$Cabin <<- as.factor(trainingData$Cabin)
  trainingData$Ticket <<- as.factor(trainingData$Ticket)
  trainingData$Survived <<- as.factor(trainingData$Survived)
  trainingData$SibSp <<- as.factor(trainingData$SibSp)
  trainingData$Parch <<- as.factor(trainingData$Parch)
  
  testData$Sex <<- as.factor(testData$Sex)
  testData$Embarked <<- as.factor(testData$Embarked)
  testData$Cabin <<- as.factor(testData$Cabin)
  testData$Ticket <<- as.factor(testData$Ticket)
  testData$SibSp <<- as.factor(testData$SibSp)
  testData$Parch <<- as.factor(testData$Parch)
}

