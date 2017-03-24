library(xgboost)

# Load data
features<-read.csv("../data/sift_features.csv")
labels<-read.csv("../data/labels.csv")
dat<-cbind(t(features), labels)
train.idx<-sample(1:nrow(dat), 0.8*nrow(dat), replace = F)
train<-dat[train.idx,]
test<-dat[-train.idx,]

# Training function for the xgboost model
xgb_train <- function(dtrain, par){
  xgb_fit = xgb.train(data=dtrain, params=par, nrounds=par$nrounds)
  return(xgb_fit)
}

# Fit the xgboost model with testing data
xgb_test <- function(fit_train, dat_test){
  pred <- predict(fit_train, as.matrix(dat_test[,1:(dim(dat_test)[2]-1)]))
  err <- mean(as.numeric(pred > 0.5) != dat_test[,dim(dat_test)[2]])
  return(list(err, as.numeric(pred> 0.5)))
}

# Find best xgboost model by cross validation
xgb_cv <- function(dtrain) {
  best_param <- list()
  best_err <- Inf
  best_err_index <- 0
  cv.result <- data.frame(shk1=I(list()),shk2=I(list()),shk3=I(list()),shk4=I(list()),shk5=I(list()))
  Shrinkage <- seq(0.1,0.5,0.1)
  for (d in 6:10) {
    for(e in 1:5){
      param <- list(objective = "binary:logistic",
                    max_depth = d,
                    eta = Shrinkage[e]
      )
      cv.nround = 1000
      cv.nfold = 5
      cat("d=",d,"e=",e,'\n')
      cv.bst <- xgb.cv(data=dtrain, params = param, 
                       nfold=cv.nfold, nrounds=cv.nround,
                       verbose = 0, early_stopping_round=5, maximize=FALSE)
      min_err <- min(cv.bst$evaluation_log[, test_error_mean])
      min_err_index <- which.min(cv.bst$evaluation_log[, test_error_mean])
      cv.result[[d-5,e]] <- list(min_err,min_err_index)
      if (min_err < best_err) {
        best_err <- min_err
        best_err_index <- min_err_index
        best_param <- param
      }
    }
  }
  xgb_best<-data.frame(best_err,best_err_index,best_param)
  return(xgb_best)
}

cvf<-function(X, y, K){
  n <- length(y)
  n.fold <- floor(n/K)
  s <- sample(rep(1:K, c(rep(n.fold, K-1), n-(K-1)*n.fold)))  
  cv.error <- rep(NA, K)
  
  for (i in 1:K){
    train.data <- X[s != i,]
    train.label <- y[s != i]
    test.data <- X[s == i,]
    test.label <- y[s == i]
    test<-cbind(test.data,test.label)
    
    dtrain <- xgb.DMatrix(data = as.matrix(train.data), label = train.label)  
    xgb_best<-xgb_cv(dtrain)
    best_par<-list(objective = xgb_best$objective,
                   max_depth = xgb_best$max_depth, 
                   eta = xgb_best$eta, 
                   nrounds = xgb_best$best_err_index 
    )
    xgb<-xgb_train(dtrain, best_par)
    pred<-xgb_test(xgb, test)  
    cv.error[i] <- pred[1] 
  }		
  return(cv.error)
}

#save(cv.result,file="~/Desktop/GR5243/training_data/cv_xgb_result.RData")
#save(xgb_best,file="~/Desktop/GR5243/training_data/xgb_best.RData")

dtrain <- xgb.DMatrix(data = as.matrix(train[,1:5000]), label = train[,5001])  
xgb_best<-xgb_cv(dtrain)
best_par<-list(objective = xgb_best$objective,
               max_depth = xgb_best$max_depth, # 9
               eta = xgb_best$eta, # 0.4
               nrounds = xgb_best$best_err_index # 57
)
xgb<-xgb_train(dtrain, best_par)
pred<-xgb_test(xgb, test)
# Training Error Rate is 0.248125. Test Error Rate is 0.2425.

tm_train <- system.time(xgb<-xgb_train(dtrain, best_par))
cat("Time for training model=", tm_train[1], "s \n")
# Time for training model= 22.994 s 

cv.error<-cvf(train[,1:5000], train[,5001], 5) # 0.2825 0.3025 0.2750 0.3175 0.2750
mean(sapply(cv.error,mean)) # Training Error Rate is 0.2905


# Test the selected feature
selected<-read.csv("../output/feature_selected2.csv")
dat2<-cbind(selected, labels)
dat2<-dat2[,2:(ncol(selected)+1)]
train.idx2<-sample(1:nrow(dat2), 0.8*nrow(dat2), replace = F)
train2<-dat2[train.idx2,]
test2<-dat2[-train.idx2,]
dtrain2 <- xgb.DMatrix(data = as.matrix(train2[,1:(ncol(selected)-1)]), label = train2[,ncol(selected)]) 
xgb_best<-xgb_cv(dtrain2)
best_par<-list(objective = xgb_best$objective,
               max_depth = xgb_best$max_depth, # 10
               eta = xgb_best$eta, # 0.2
               nrounds = xgb_best$best_err_index # 22
)
xgb<-xgb_train(dtrain, best_par)
pred<-xgb_test(xgb, test)
# Training Error Rate is 0.28. Test Error Rate is 0.2625.

# Test the PCA selected feature
selected<-read.csv("../output/pca_features.csv")
dat2<-cbind(selected, labels)
dat2<-dat2[,2:(ncol(selected)+1)]
train.idx2<-sample(1:nrow(dat2), 0.8*nrow(dat2), replace = F)
train2<-dat2[train.idx2,]
test2<-dat2[-train.idx2,]
cv.error<-cvf(dat2[,1:(ncol(dat2)-1)], dat2[,ncol(dat2)], 5)
mean(sapply(cv.error,mean))
# Training Error Rate is 0.2495.

# Final version
# Sift Features
dtrain <- xgb.DMatrix(data = as.matrix(data[,1:(ncol(data)-1)]), label = data[,ncol(data)]) 
tm_train <- system.time(xgb_best<-xgb_cv(dtrain))
cat("Time for training model=", tm_train[1], "s \n")
# Time for training model=  1747.893 s

# PCA Features
dtrain <- xgb.DMatrix(data = as.matrix(dat2[,1:(ncol(dat2)-1)]), label = dat2[,ncol(dat2)]) 
tm_train <- system.time(xgb_best<-xgb_cv(dtrain))
cat("Time for training model=", tm_train[1], "s \n")
# Time for training model= 433.79 s 