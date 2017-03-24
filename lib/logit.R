
#select 5000 features-------
### load data
setwd("~/spr2017-proj3-group13")

### 5000 sifted features
feature_sift<-read.csv("../data/sift_features.csv")
sift_feature<-t(sift_feature)
sift_feature <- data.matrix(sift_feature)
label <- read.csv("labels.csv")

#select bow features------------

#sift_feature_bow <-read.csv("../output/feature_selected2.csv") 
#sift_feature_bow <- data.matrix(sift_feature_bow)[,-1]

#select PCA features---------

#sift_feature_pca <- read.csv("../output/pca_features.csv")
#sift_feature_pca <- sift_feature_pca[,-1]
#sift_feature_pca <- data.matrix(sift_feature_pca)


#split data into training and testing

install.packages('glmnet')
library("glmnet")
install.packages('sgd')
library("sgd")

#cross validation function for lasso logistic regression-----
cv.function <- function(X.train, y.train, K){
  
  n <- length(y.train)
  n.fold <- floor(n/K)
  s <- sample(rep(1:K, c(rep(n.fold, K-1), n-(K-1)*n.fold)))  
  cv.error <- rep(NA, K)
  train.error<- rep(NA, K)
  diff<-rep(NA, K)
  for (i in 1:K){
    train.data <- X.train[s != i,]
    train.label <- y.train[s != i]
    test.data <- X.train[s == i,]
    test.label <- y.train[s == i]
    
    #train model, tune for lambda parameter
    fit <- cv.glmnet(as.matrix(train.data),train.label,alpha = 1,family = "binomial",type.measure='auc',lambda=10^(seq(-1,-5,-0.2)))
    
    #test error
    pred <- predict(fit, test.data)  
    pred <- predict(fit, test.data,type = "response",s='lambda.min')  
    cv.error[i] <- mean(pred != test.label)  
    
    #training error (someone check if this logic is correct)
    pred <- predict(fit, train.data,type = "response",s='lambda.min')  
    pred<-ifelse(pred>0.5,1,0)
    train.error[i] <- mean(pred != train.label)  
    
    diff[i] <- cv.error[i]-train.error[i]
  }			
  return(c(mean(cv.error),sd(cv.error),mean(diff),sd(diff)))
  
}
#---------
cv.function(sift_feature,as.matrix(label),5)

#this is the same thing as the cross validation function, just taken out of the 
#function format to better analyze

install.packages('glmnet')
library("glmnet")

sift_feature <- read.csv("feature_selected2.csv")
label <- read.csv("labels.csv")

X.train =sift_feature
y.train = as.matrix(label)

K=5

n <- length(y.train)
n.fold <- floor(n/K)
s <- sample(rep(1:K, c(rep(n.fold, K-1), n-(K-1)*n.fold)))  
cv.error <- rep(NA, K)
train.error<- rep(NA, K)
diff<-rep(NA, K)
for (i in 1:K){
  print(i)
  train.data <- X.train[s != i,]
  train.label <- y.train[s != i]
  test.data <- X.train[s == i,]
  test.label <- y.train[s == i]
  
  #train the model---------
  fit <- cv.glmnet(train.data,train.label,alpha = 1,family = "binomial",type.measure="class",lambda=10^(seq(-1,-5,-0.2)))
  #fit <- sgd(train.data, train.label,model='glm',model.control=binomial(link="logit"))
  
  #error testing------------
  #testing error
  pred <- predict(fit, test.data,type = "response",s='lambda.min')  
  #pred <- predict(fit, test.data,type = 'response')
  pred<-ifelse(pred>0.5,1,0)
  cv.error[i] <- mean(pred != test.label)  
  
  #training error
  pred <- predict(fit, train.data,type = "response",s='lambda.min')  
  #pred <- predict(fit, train.data,type = 'response')
  pred<-ifelse(pred>0.5,1,0)
  train.error[i] <- mean(pred != train.label)  
  
  diff[i] <- cv.error[i]-train.error[i]
}			

train.error
cv.error
diff
