library(data.table)
library(rattle)
library(rpart)
library(rpart.plot)
library(RColorBrewer)
library(randomForest)
library(party)

downloadLibraries <- function() {
  install.packages('data.table')
  install.packages("https://togaware.com/access/rattle_5.0.14.tar.gz", repos=NULL, type="source")
  install.packages('rpart.plot')
  install.packages('RColorBrewer')
  install.packages('RGtk2')
  install.packages('randomForest')
  install.packages('party')
}

importData <- function() {
  trainingData <<- fread('data/train.csv')
  testData <<- fread('data/test.csv')
}

setupFactorsInData <- function() {
  trainingData$Sex <<- as.factor(trainingData$Sex)
  trainingData$Embarked <<- as.factor(trainingData$Embarked)
  trainingData$Survived <<- as.factor(trainingData$Survived)
  
  testData$Sex <<- as.factor(testData$Sex)
  testData$Embarked <<- as.factor(testData$Embarked)
}

getFit <- function(dataSet) {
  rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked, 
        data=dataSet, 
        method='class',
        control=rpart.control(minsplit = 20))
}

getFitWithMoreFactors <- function(dataSet) {
  rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamilySize + FamilyID, 
        data=dataSet, 
        method='class',
        control=rpart.control(minsplit = 20, cp=0.005))
}

combineDataSets <- function() {
  rbind(trainingData, testData, fill=TRUE)
}

createTitles <- function(dataSet) {
  dataSet$Title <- sapply(dataSet$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][2]})
  dataSet$Title <- sub(' ', '', dataSet$Title)
  dataSet$Title[dataSet$Title %in% c('Mme', 'Mlle')] <- 'Mlle'
  dataSet$Title[dataSet$Title %in% c('Capt', 'Don', 'Major', 'Sir')] <- 'Sir'
  dataSet$Title[dataSet$Title %in% c('Dona', 'Lady', 'the Countess', 'Jonkheer')] <- 'Lady'
  dataSet$Title <- as.factor(dataSet$Title)
  dataSet
}

createSurnames <- function(dataset) {
  dataset$Surname <- sapply(dataset$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][1]})
  dataset
}

createFamilySize <- function(dataSet) {
  dataSet$FamilySize <- dataSet$SibSp + dataSet$Parch + 1
  dataSet
}

createFamilyId <- function(dataSet) {
  dataSet$FamilyID <- paste(as.character(dataSet$FamilySize), dataSet$Surname, sep="")
  dataSet$FamilyID[dataSet$FamilySize <= 2] <- 'Small'
  famIds <- data.frame(table(dataSet$FamilyID))
  famIds <- famIds[famIds$Freq < 3,]
  dataSet$FamilyID[dataSet$FamilyID %in% famIds$Var1] <- 'Small'
  dataSet$FamilyID <- factor(dataSet$FamilyID)
  
  dataSet$FamilyID2 <- as.character(dataSet$FamilyID)
  dataSet$FamilyID2[dataSet$FamilySize <= 3] <- 'Small'
  dataSet$FamilyID2 <- factor(dataSet$FamilyID2)
  
  dataSet
}

cleanData <- function(dataSet) {
  ageFit <- rpart(Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked + Title + FamilySize,
                  data=dataSet[!is.na(dataSet$Age),],
                  method='anova')
  dataSet$Age[is.na(dataSet$Age)] <- predict(ageFit, dataSet[is.na(dataSet$Age),])
  
  dataSet$Embarked[dataSet$Embarked == ''] <- "S"
  
  dataSet$Fare[which(is.na(dataSet$Fare))] <- median(dataSet$Fare, na.rm=TRUE)
  
  dataSet
}

addFeatureColumns <- function(dataSet) {
  cleanData(createFamilyId(createFamilySize(createSurnames(createTitles(dataSet)))))
}
