source('R/dataPrep.R')

importData()
setupFactorsInData()

combinedDataSets <- combineDataSets()
combinedDataSets <- addFeatureColumns(combinedDataSets)
trainingData <- combinedDataSets[1:891,]
testData <- combinedDataSets[892:1309,]

#rpart
fit <- getFit(trainingData)
fancyRpartPlot(fit)
Prediction <- predict(fit, testData, type='class')

#rpart with engineered factors
fitWithMoreFactors <- getFitWithMoreFactors(trainingData)
fancyRpartPlot(fitWithMoreFactors)
Prediction <- predict(fitWithMoreFactors, testData, type='class')

#random forst with engineered factors
fitRandomForest <- randomForest(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamilySize + FamilyID2,
                               data=trainingData,
                               importance=TRUE,
                               ntree=2000)
Prediction <- predict(fitRandomForest, testData)

#conditional inference trees
fitConditional <- cforest(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamilySize + FamilyID,
                          data=trainingData,
                          controls=cforest_unbiased(ntree=2000, mtry=3))
Prediction <- predict(fitConditional, testData, OOB=TRUE, type='response')

#Submission Area For Kaggle. Make sure to use the Prediction of the fit you want
submit <- data.frame(PassengerId = testData$PassengerId, Survived = Prediction)
write.csv(submit, file = "submission.csv", row.names = FALSE)
