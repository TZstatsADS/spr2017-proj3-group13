library(data.table)
library(dplyr)
library(randomForest)
library(EBImage)

### load data
setwd("~/spr2017-proj3-group13")
dat_train=fread("data/training_data/sift_features/sift_features.csv")
dat_train<-tbl_df(t(dat_train))
label_train<-read.csv("data/training_data/labels.csv")
label_train<-label_train[1:2000,]


### random forest train

rf_train=function(dat_train, label_train,ntree,mtry)
{
  dat_train=data.frame(dat_train)
  data=mutate(dat_train,label=factor(label_train))
  rf_fit <- randomForest(label~ .,
                         data=data,
                         importance=TRUE, 
                         ntree=ntree,
                         mtry=mtry)
  return(rf_fit)
}

### random forest test

rf_test <- function(fit_train, dat_test){
  library(randomForest)
  pred <- predict(fit_train, newdata=dat_test)
  return(pred)
}

rf.cv.function <- function(X.train, y.train,ntree=500,mtry=sqrt(ncol(X.train)),K=5,cv=T){
  
  n <- length(y.train)
  n.fold <- floor(n/K)
  s <- sample(rep(1:K, c(rep(n.fold, K-1), n-(K-1)*n.fold)))  
  cv.error <- rep(NA, K)
  
  
  for (i in 1:K){
    train.data <- X.train[s != i,]
    train.label <- y.train[s != i]
    test.data <- X.train[s == i,]
    test.label <- y.train[s == i]
    
    fit <- rf_train(train.data, train.label, ntree=ntree,mtry=mtry)
    pred <- rf_test(fit, test.data)  
    cv.error[i] <- mean(pred != test.label)  
    if (cv==F) 
    {
      break
    }
  }	
  if (cv==F) 
  {
    return(cv.error)
  }
  else
  {
    return(c(mean(cv.error),sd(cv.error)))
  }
  
}

######## try with cross validation

rf.cv.function(dat_train,label_train)

# error: 0.28500000   
# error sd: 0.02236068

######## try without cross validation

rf.cv.function(dat_train,label_train,cv=F)

######## Tune randomForest for the optimal mtry parameter

x<- dat_train
y<- as.factor(label_train)
mtryStart<-sqrt(ncol(x))
tuneRF(x,y, mtryStart, ntreeTry=500, stepFactor=2, improve=0.05,trace=TRUE, plot=TRUE, doBest=FALSE)


# mtry=141

######## try with cross validation with mtry=141

rf.cv.function(dat_train,label_train,mtry=141)

# error: 0.27300000   
# error sd: 0.01267379 
