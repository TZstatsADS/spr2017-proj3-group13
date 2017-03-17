library(sgd)

#loading data
setwd("~/spr2017-proj3-group13")
feature_sift<-read.csv("data/training_data/sift_features/sift_features.csv")
sift_data<-t(feature_sift)
label_train <- read.csv("data/training_data/labels.csv")


#split data into training and testing

set.seed(2)
index<-sample(1:2000,1600)
train_set <- sift_data[index,]
test_set <- sift_data[-index,]
train_label <- as.matrix(label_train[index,])
test_label <- as.matrix(label_train[-index,])


#stochastic gradient descent logistic regression
log_fit<- sgd(train_set, train_label,model='glm',model.control=binomial(link="logit"))

#test on test set
pred <- predict(log_fit, test_set,type = 'response')  
pred <- ifelse(pred <= 0.5, 0, 1) 
error<-mean(pred!=test.label)
#log_fit <- sgd(train_set, train_label,model='glm',model.control=list(family="binomial",lambda2=0.001))


