library(gbm)
library(caret)
library(rpart)
library(e1071)
library(kernlab)    

### load data
setwd("~/spr2017-proj3-group13")
feature_sift<-read.csv("data/training_data/sift_features/sift_features.csv")
feature_sift_new<-t(feature_sift)
label_train <- read.csv("data/training_data/labels.csv")
y <- label_train[1:2000,]
data<-as.data.frame(cbind(feature_sift_new,y))

### set apart train & test data
index <- 1:nrow(data)
trainindex <- sample(index, 0.8*nrow(data),replace=F)
testset <- data[-trainindex,]
trainset <-data[trainindex,]

### select parameter: cost = 150 ; gamma = 1
system.time(svm.model <- svm(y ~ ., data = trainset, cost = 150, gamma = 1, scale = FALSE, type = "C-classification"))
svm.pred <- predict(svm.model, testset[,-5001])
error<-mean(svm.pred != testset$y)

### tune parameter
system.time(svm_tune <- tune(svm, train.x=trainset[,-5001], train.y=trainset$y, scale = FALSE,
                             kernel="radial", ranges=list(cost=10^(-1:2), gamma=c(.5,1,2))))

### best parameters: cost = 100 ; gamma = 1
### best performance: 0.1618074
### 85 minutes

system.time(svm.model <- svm(y ~ ., data = trainset, cost = 100, gamma = 1, scale = FALSE, type = "C-classification"))
svm.pred <- predict(svm.model, testset[,-5001])
error<-mean(svm.pred != testset$y)
error

#19.5%-20.25%
