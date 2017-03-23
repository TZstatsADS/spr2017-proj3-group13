install.packages('glmnet')
library("glmnet")

sift_feature <- read.csv("feature_selected2.csv")
label <- read.csv("labels.csv")

X.train =data.matrix(sift_feature)
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
  #split data
  train.data <- X.train[s != i,]
  train.label <- y.train[s != i]
  test.data <- X.train[s == i,]
  test.label <- y.train[s == i]
  
  
  #train
  fit <- cv.glmnet(train.data,train.label,alpha = 1,family = "binomial",type.measure="class")
  
  #predict test error
  pred <- predict(fit, test.data,type = "response",s='lambda.min')  
  pred<-ifelse(pred>0.5,1,0)
  cv.error[i] <- mean(pred != test.label)  
  
  #go back to predict on training set to get training error
  pred <- predict(fit, train.data,type = "response",s='lambda.min')  
  pred<-ifelse(pred>0.5,1,0)
  train.error[i] <- mean(pred != train.label)  
  
  #difference
  diff[i] <- cv.error[i]-train.error[i]
}			

train.error
cv.error
diff
