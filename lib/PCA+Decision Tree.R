setwd("../training_data/sift_features")
features<-read.csv("sift_features.csv")
setwd("../training_data")
labels<-read.csv("labels.csv")
dat<-cbind(t(features), labels)
train.idx<-sample(1:nrow(dat), 0.8*nrow(dat), replace = F)
train<-dat[train.idx,]
test<-dat[-train.idx,]
pca<-prcomp(train[,1:5000],center = T)
prop_pca<-pca$sdev^2/sum(pca$sdev^2)
plot(cumsum(prop_pca), xlab = "Principal Component",
     ylab = "Cumulative Proportion of Variance Explained",
     type = "b")
N<-length(pca$sdev)-sum(cumsum(prop_pca)>0.8)
train.data <- data.frame(Label = train$V1, pca$x)[,1:N]
#run a decision tree
library(rpart)
rpart.model <- rpart(Label ~ .,data = train.data, method = "class")

#transform test into PCA
test.data <- predict(pca, newdata = test)
test.data <- as.data.frame(test.data)

#select the first 30 components
test.data <- test.data[,1:N]

#make prediction on test data
rpart.prediction<-predict(rpart.model, test.data)
rpart.prediction<-ifelse(apply(rpart.prediction, 1, which.max)==1,0,1)
mean(rpart.prediction!=test$V1)