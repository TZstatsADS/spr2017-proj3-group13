setwd("~/spr2017-proj3-group13")
features<-read.csv("data/sift_features.csv")
features<-t(features)
fea.mean <- apply(features, 1, mean)
fea.cent <- t(scale(t(features), center=T, scale=F))

ppc <- prcomp(t(fea.cent))
# screeplot(ppc, type = "lines")
# as the plot showing, we can choose the first 1000 columns to get a pretty precise result. 
write.csv(ppc$rotation[,1:300],file="output/pca_features3.csv")




