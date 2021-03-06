#setwd("../spr2017-proj3-group13-master")
#getwd()
features<-read.csv("data/training_data/sift_features/sift_features.csv")
#setwd("../training_data")
labels<-read.csv("data/training_data/labels.csv")
#dat<-cbind(t(features), labels)
dat<-cbind(t(features))

dat <- data.frame(dat)

train.idx<-sample(1:nrow(dat), 0.8*nrow(dat), replace = F)
#train<-dat[train.idx,]
#test<-dat[-train.idx,]
pca<-prcomp(dat[,1:5000],center = T)
prop_pca<-pca$sdev^2/sum(pca$sdev^2)
plot(cumsum(prop_pca), xlab = "Principal Component",
     ylab = "Cumulative Proportion of Variance Explained",
     type = "b")
N<-length(pca$sdev)-sum(cumsum(prop_pca)>0.8)

#train.data <- data.frame(Label = train$V1, pca$x)[,1:N]

all.data <- data.frame(pca$x)[,1:N]

train.data<-all.data[train.idx,]
test.data<-all.data[-train.idx,]

train.label <- labels[train.idx,]
test.label <- labels[-train.idx,]

train.all <- cbind(train.data, train.label)

pca

write.csv(all.data,file="data/training_data/sift_features/pca_features.csv")

# #run xgboost
# library(xgboost)
# 
# # Load data
# features<-read.csv("/Users/wangzhishan/Desktop/spr2017-proj3-group13-master/data/training_data/sift_features/pca_features.csv")
# #labels<-read.csv("../data/labels.csv")
# dat<-cbind(t(features), labels)
# train.idx<-sample(1:nrow(dat), 0.8*nrow(dat), replace = F)
# train<-dat[train.idx,]
# test<-dat[-train.idx,]
# 
# # Training function for the xgboost model
# xgb_train <- function(dtrain, par){
#   xgb_fit = xgb.train(data=dtrain, params=par, nrounds=par$nrounds)
#   return(xgb_fit)
# }
# 
# # Fit the xgboost model with testing data
# xgb_test <- function(fit_train, dat_test){
#   pred <- predict(fit_train, as.matrix(dat_test[,1:(dim(dat_test)[2]-1)]))
#   err <- mean(as.numeric(pred > 0.5) != dat_test[,dim(dat_test)[2]])
#   return(list(err, as.numeric(pred> 0.5)))
# }
# 
# # Find best xgboost model by cross validation
# xgb_cv <- function(dtrain) {
#   best_param <- list()
#   best_err <- Inf
#   best_err_index <- 0
#   cv.result <- data.frame(shk1=I(list()),shk2=I(list()),shk3=I(list()),shk4=I(list()),shk5=I(list()))
#   Shrinkage <- seq(0.1,0.5,0.1)
#   for (d in 6:10) {
#     for(e in 1:5){
#       param <- list(objective = "binary:logistic",
#                     max_depth = d,
#                     eta = Shrinkage[e]
#       )
#       cv.nround = 1000
#       cv.nfold = 5
#       cat("d=",d,"e=",e,'\n')
#       cv.bst <- xgb.cv(data=dtrain, params = param, 
#                        nfold=cv.nfold, nrounds=cv.nround,
#                        verbose = 0, early_stopping_round=5, maximize=FALSE)
#       min_err <- min(cv.bst$evaluation_log[, test_error_mean])
#       min_err_index <- which.min(cv.bst$evaluation_log[, test_error_mean])
#       cv.result[[d-5,e]] <- list(min_err,min_err_index)
#       if (min_err < best_err) {
#         best_err <- min_err
#         best_err_index <- min_err_index
#         best_param <- param
#       }
#     }
#   }
#   xgb_best<-data.frame(best_err,best_err_index,best_param)
#   return(xgb_best)
# }
# 
# save(cv.result,file="~/Desktop/GR5243/training_data/cv_xgb_result.RData")
# save(xgb_best,file="~/Desktop/GR5243/training_data/xgb_best.RData")
# 
# dtrain <- xgb.DMatrix(data = as.matrix(train[,1:5000]), label = train[,5001])  
# xgb_best<-xgb_cv(dtrain)
# best_par<-list(objective = xgb_best$objective,
#                max_depth = xgb_best$max_depth, # 9
#                eta = xgb_best$eta, # 0.4
#                nrounds = xgb_best$best_err_index # 57
# )
# xgb<-xgb_train(dtrain, best_par)
# pred<-xgb_test(xgb, test)
# # Training Error Rate is 0.248125. Test Error Rate is 0.2425.
# 
# tm_train <- system.time(xgb<-xgb_train(dtrain, best_par))
# cat("Time for training model=", tm_train[1], "s \n")
# # Time for training model= 22.994 s 
# 
# 
# # Test the selected feature
# selected<-read.csv("../output/feature_selected2.csv")
# dat2<-cbind(selected, labels)
# dat2<-dat2[,2:(ncol(selected)+1)]
# train.idx2<-sample(1:nrow(dat2), 0.8*nrow(dat2), replace = F)
# train2<-dat2[train.idx2,]
# test2<-dat2[-train.idx2,]
# dtrain2 <- xgb.DMatrix(data = as.matrix(train2[,1:(ncol(selected)-1)]), label = train2[,ncol(selected)]) 
# xgb_best<-xgb_cv(dtrain2)
# best_par<-list(objective = xgb_best$objective,
#                max_depth = xgb_best$max_depth, # 10
#                eta = xgb_best$eta, # 0.2
#                nrounds = xgb_best$best_err_index # 22
# )
# xgb<-xgb_train(dtrain, best_par)
# pred<-xgb_test(xgb, test)
# # Training Error Rate is 0.28. Test Error Rate is 0.2625.
# 
# #run a decision tree
# library(rpart)
# rpart.model <- rpart(train.label ~ .,data = train.all, method = "class")
# 
# #transform test into PCA
# #test.data <- stats::predict(pca, newdata = test.d)
# #test.data <- as.data.frame(test.data)
# 
# #select the first 30 components
# #test.data <- test.data[,1:N]
# 
# test.all <- cbind(test.data, test.label)
# 
# #make prediction on test data
# rpart.prediction<-predict(rpart.model, test.data)
# rpart.prediction<-ifelse(apply(rpart.prediction, 1, which.max)==1,0,1)
# mean(rpart.prediction!=test.all$test.label)
