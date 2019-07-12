library(randomForest) #library used for modeling
#for file.choose(), select heart.csv
heart <- read.csv(file.choose(), header = TRUE, colClasses = c("target"="factor")) #need to change target to categorical var
summary(heart)
target <- heart$target
#random cample to reduce bias in training & valid sets
train <- sample(nrow(heart), 0.7*nrow(heart), replace = FALSE)             
TrainSet <- heart[train,]
ValidSet <- heart[-train,]
summary(TrainSet)
summary(ValidSet)
#first model based on the training set
model1 <- randomForest(target ~ ., data = TrainSet, importance = TRUE)
model1
#second model, with adjusted number of variables being changed at every split
model2 <- randomForest(target ~ ., data = TrainSet, ntree = 500, mtry = 5, importance = TRUE)
model2
#actual predictions comparing predictions to trainset (should have no errors!)
predTrain <- predict(model2, TrainSet, type = "class")
table(predTrain, TrainSet$target)
#predicting valid set target value based on model2
predValid <- predict(model2, ValidSet, type = "class")
mean(predValid == ValidSet$target) #shows accuracy % btwn prediction and actual target value
table(predValid,ValidSet$target)
varImpPlot(model2)
#checking for optimal mtry for the predicting model to increase accuracy 
a <- c() 
i = 5
for (i in 3:13) {
       model3 <- randomForest(target ~ ., data = TrainSet, ntree = 500, mtry = i, importance = TRUE)
       predValid <- predict(model3, ValidSet, type = "class")
       a[i-2] = mean(predValid == ValidSet$target)
}
a
#visual of the optimal mtry 
plot(3:13, a)