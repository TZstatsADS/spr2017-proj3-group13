######################################################
### Fit the classification model with testing data ###
######################################################


test <- function(fit_train, dat_test){
  
  ### Fit the classfication model with testing data
  
  ### Input: 
  ###  - the fitted classification model using training data
  ###  -  processed features from testing images 
  ### Output: training model specification
  
  ### load libraries
  library("gbm")
  
  pred <- predict(fit_train$fit, newdata=dat_test, 
                  n.trees=fit_train$iter, type="response")
  
  return(as.numeric(pred> 0.5))
}


test.svm <- function(model,test_set){
  
  ### load libraries
  library(gbm)
  library(caret)
  library(rpart)
  library(e1071)
  library(kernlab) 
  m<-dim(test_set)[2]
  svm.pred <- predict(model, test_set[,-m])
  return(svm.pred)
}


