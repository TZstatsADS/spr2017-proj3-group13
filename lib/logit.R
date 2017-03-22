library(sgd)
sift_feature<-read.csv("sift_features.csv")
sift_feature<-t(sift_feature)
label <- read.csv("labels.csv")


#split data into training and testing

set.seed(2)
index<-sample(1:2000,1600)
train_set <- sift_feature[index,]
test_set <- sift_feature[-index,]
train_label <- as.matrix(label[index,])
test_label <- as.matrix(label[-index,])


#stochastic gradient descent logistic regression
log_fit<- sgd(train_set, train_label,model='glm',model.control=binomial(link="logit"))

#test on test set
pred <- predict(log_fit, test_set,type = 'response')  
pred <- ifelse(pred <= 0.5, 0, 1) 
error<-mean(pred!=test_label)
#log_fit <- sgd(train_set, train_label,model='glm',model.control=list(family="binomial",lambda2=0.001))






install.packages('glmnet')
library("glmnet")

lasso_logistic<-cv.glmnet(as.matrix(train_set),train_label,alpha = 1,family = "binomial",type.measure='auc')
pred_lasso_logistic<- predict(lasso_logistic,newx  = as.matrix(test_set),type = "response",s='lambda.min')
pred_lasso_logistic<-ifelse(pred_lasso_logistic>0.5,1,0)
error<-mean(pred_lasso_logistic!=test_label)
error




#pca 


train.idx<-sample(1:nrow(sift_feature), 0.8*nrow(sift_feature), replace = F)
train<-sift_feature[train.idx,]
test<-sift_feature[-train.idx,]
pca<-prcomp(train[,1:5000],center = T)
prop_pca<-pca$sdev^2/sum(pca$sdev^2)
plot(cumsum(prop_pca), xlab = "Principal Component",
     ylab = "Cumulative Proportion of Variance Explained",
     type = "b")
N<-length(pca$sdev)-sum(cumsum(prop_pca)>0.8)
train.data <- data.frame(Label = train[,5001], pca$x)[,1:N]

pca_lasso_logistic <- cv.glmnet(as.matrix(train.data[,-1]),train.data$Label,alpha = 1,family = "binomial",type.measure='auc')


#transform test into PCA
test.data <- predict(pca, newdata = test[,-5001])
test.data <- as.data.frame(test.data)

#select the first 30 components
test.data <- as.matrix(test.data[,1:N-1])
pca_lasso_logistic_pred<-predict(pca_lasso_logistic, test.data)
pca_lasso_logistic_pred<-ifelse(pca_lasso_logistic_pred>0.5,1,0)
error<-mean(pca_lasso_logistic_pred!=test_label)
error
