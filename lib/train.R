#########################################################
### Train a classification model with training images ###
#########################################################


### baseline model

train <- function(dat_train, label_train, par=NULL){
  
  ### Train a Gradient Boosting Model (GBM) using processed features from training images
  
  ### Input: 
  ###  -  processed features from images 
  ###  -  class labels for training images
  ### Output: training model specification
  
  ### load libraries
  library("gbm")
  
  ### Train with gradient boosting model
  if(is.null(par)){
    depth <- 9
  } else {
    depth <- par$depth
  }
  
  # tune the parameter 
  # fit_gbm <- gbm.fit(x=dat_train, y=label_train,
  #                    n.trees=100,
  #                    distribution="bernoulli",
  #                    interaction.depth=depth, 
  #                    bag.fraction = 0.5,
  #                    verbose=FALSE)
  
  fit_gbm <- gbm.fit(x=dat_train, y=label_train,
                     n.trees=150,
                     distribution="bernoulli",
                     interaction.depth=depth, 
                     bag.fraction = 0.5,
                     verbose=FALSE)
  best_iter <- gbm.perf(fit_gbm, method="OOB", plot.it = FALSE)
  return(list(fit=fit_gbm, iter=best_iter))
} 


### advance model


### tune parameter manually

tune.svm2<-function(trainset)

{
mycost1<-c(100,1000,10000)
mygamma1<-c(0.02,0.1,0.5)
cv_error_rate<-matrix(ncol=length(mygamma1),nrow=length(mycost1))

for (i in 1:length(mycost1))
{
  for (j in 1:length(mygamma1))
  {
    svm.model <- svm(y ~ ., data = trainset, cost = mycost1[i], gamma = mygamma1[j], scale = FALSE, cross= 5,type = "C-classification")
    cv_error_rate[i,j]<-1-(svm.model$tot.accuracy/100)
    print(paste0("i=",i,",j=",j," finished"))
  }
}
cv_error_rate
min_index<-which.min(cv_error_rate)
mygamma_index<-ceiling(min_index/length(mycost1))
mycost_index<-min_index-length(mycost1)*floor(min_index/length(mycost1))
mygamma<-mygamma1[mygamma_index]
mycost<-mycost1[mycost_index]
return(c(mycost,mygamma))
}

### best parameters: cost = 100 ; gamma = 0.02

train.svm.cv <- function(X, mycost=100,mygamma=0.02){
  
  library(gbm)
  library(caret)
  library(rpart)
  library(e1071)
  library(kernlab) 
  
  ### Train with svm
  


  
  K<-5  
  n <- length(label_train)
  n.fold <- floor(n/K)
  s <- sample(rep(1:K, c(rep(n.fold, K-1), n-(K-1)*n.fold)))  
  cv.error <- rep(NA,K)
  m<-dim(X)[2]
  
  for (k in 1:K)
  {
    trainset <- X[s != k,]
    testset <- X[s == k,]
    svm.model <- svm(y ~ ., data = trainset, cost = mycost, gamma = mygamma,kernel="radial",scale = FALSE, type = "C-classification")
    svm.pred <- predict(svm.model, testset[,-m])
    cv.error[k]<-mean(svm.pred != testset$y)
    print(paste0("fold k=",k," finished"))
  }
  print(paste("the mean of training error rate is:",mean(cv.error)))
  return(svm.model) 
  
  
  }
  