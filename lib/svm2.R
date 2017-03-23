library(gbm)
library(caret)
library(rpart)
library(e1071)
library(kernlab)    

### load data
setwd("~/spr2017-proj3-group13")

### 5000 sifted features
feature_sift<-read.csv("data/training_data/sift_features/sift_features.csv")
feature_new<-t(feature_sift)

### 577 pac features
feature<-read.csv("output/pca_features.csv")
feature_new<-feature

label_train <- read.csv("data/training_data/labels.csv")
y <- label_train[1:2000,]
data<-as.data.frame(cbind(feature_new,y))

### set apart train & test data
index <- 1:nrow(data)
trainindex <- sample(index, 0.8*nrow(data),replace=F)
testset <- data[-trainindex,]
trainset <-data[trainindex,]

### random select parameter: cost = 150 ; gamma = 1
system.time(svm.model <- svm(y ~ ., data = trainset, cost = 150, gamma = 1, scale = FALSE, type = "C-classification"))
svm.pred <- predict(svm.model, testset[,-5001])
error<-mean(svm.pred != testset$y)

### cross validation

mycost<-10^(2:5)
mygamma<-c(.5,1,2)

X<-data
K<-5  
n <- length(label_train)
n.fold <- floor(n/K)
s <- sample(rep(1:K, c(rep(n.fold, K-1), n-(K-1)*n.fold)))  
cv.error <- rep(NA,K)
error<-matrix(ncol=length(mygamma),nrow=length(mycost))


for (k in 1:K)
{
  trainset <- X[s != k,]
  testset <- X[s == k,]
  for (i in 1:length(mycost))
  {
    for (j in 1:length(mygamma))
    {
      system.time(svm.model <- svm(y ~ ., data = trainset, cost = mycost[i], gamma = mygamma[j], scale = FALSE, type = "C-classification"))
      svm.pred <- predict(svm.model, testset[,-5001])
      error[i,j]<-mean(svm.pred != testset$y)
      print(paste0("i=",i,",j=",j,",k=",k," finished"))
    }
  }
  cv.error[k]<-min(error)
}



### tune parameter
system.time(svm_tune <- tune(svm, train.x=trainset[,-5001], train.y=trainset$y, scale = FALSE,
                             kernel="radial", ranges=list(cost=10^(2:5), gamma=c(.5,1,2))))
### best parameters: cost = 100 ; gamma = 1
### best performance: 0.1618074
### 85 minutes
plot(svm_tune)



system.time(svm.model <- svm(y ~ ., data = trainset, cost = 100, gamma = 1, scale = FALSE, type = "C-classification"))
svm.pred <- predict(svm.model, testset[,-5001])
error<-mean(svm.pred != testset$y)
error
#19.5%-20.25%
#48.5% 3600  57s  - 43s