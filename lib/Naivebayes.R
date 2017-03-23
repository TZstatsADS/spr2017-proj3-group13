install.packages('e1071', dependencies=TRUE)
library(e1071)
sift_feature<-data.frame(read.csv("sift_features.csv"))
label<-data.frame(read.csv("labels.csv"))[,1]
sift_feature<-t(sift_feature)


# Name each column
colnames(sift_feature)<-paste("feature",1:ncol(sift_feature))
sift_feature<-cbind.data.frame(sift_feature,class=label)


# separate into training and testing
set.seed(5)
index<-sample(1:2000,1600)
train <- sift_feature[index,]
test <- sift_feature[-index,]


# Train the model 

bayes.fit<-naiveBayes(train[,-5001],as.factor(train[,5001]))

# Test the model 
prediction<-predict(bayes.fit,test[,-5001])
error<-mean(prediction!=test[,5001])
error
