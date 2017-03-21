
setwd("../training_data/sift_features")
features<-read.csv("sift_features.csv")
setwd("../training_data")
labels<-read.csv("labels.csv")
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


#run a decision tree
library(rpart)
rpart.model <- rpart(train.label ~ .,data = train.all, method = "class")

#transform test into PCA
#test.data <- stats::predict(pca, newdata = test.d)
#test.data <- as.data.frame(test.data)

#select the first 30 components
#test.data <- test.data[,1:N]

test.all <- cbind(test.data, test.label)

#make prediction on test data
rpart.prediction<-predict(rpart.model, test.data)
rpart.prediction<-ifelse(apply(rpart.prediction, 1, which.max)==1,0,1)
mean(rpart.prediction!=test.all$test.label)
