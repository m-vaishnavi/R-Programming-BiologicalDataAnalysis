bioclite("multtest")
library(multtest)
data(golub)

##Principal component analysis
eigen(cor(golub))$values[1:5]
p<-ncol(data);n<-row(data);nboot<-1000
eigenvalues <- array(dim=c(nboot,p))
for (i in 1:nboot){dat.star <-data[sample(1:n,replace=TRUE),] 
eigenvalues[i,] <- eigen(cor(dat.star))$values}
for (j in 1:p) print(quantile(eigenvalues[,j],c(0.025,0.975)))
for (j in 1:5) cat(j,as.numeric(quantile(eigenvalues[,j], + c(0.025,0.975))),"\n" ) 
sum(eigen(cor(golub))$values[1:2])/38*100 
eigen(cor(golub))$vec[,1:2]
pca < princomp(golub, center = TRUE, cor=TRUE, scores=TRUE)
o < order(pca$scores[,2])
golub.gnames[o[1:10],2]
golub.gnames[o[3041:3051],2]

##correlation
corgol<- apply(golub, 1, function(x) cor(x,golub.cl))
o <- order(corgol)
golub.gnames[o[3041:3051],2]

##K-means
data <- data.frame(golub[ccnd3,],golub[zyxin,],golub[829,],golub[808,],golub[2670,])
cl <- kmeans(data, 2,nstart = 10)
cl
fit<-kmeans(data, 2,nstart = 10)
fit$cluster
fit$centers
aggregate(data,by=list(fit$cluster),FUN=mean)
plot(data, col = cl$cluster)
require(cluster)
fit<-pam(x=data,k=8)
fit$clustering
fit$medoids
summary(fit)

##hierarchial clustering
d<-dist(data,method="euclidean")
fit<-hclust(d,method = "ward.D")
plot(fit)
groups<-cutree(fit,k=8)
rect.hclust(fit,k=8,border="red")

##correlation plot visualization
require(corrplot)
corrplot(cor(data[1:2]))

## biplot
biplot(princomp(data,cor=TRUE),pc.biplot=TRUE,cex=0.5,expand=0.8)

##Linear Discriminant Analysis
> data(golub)
> golubY<-golub[,1]
> golubX<-as.matrix(golub[,2:11])
> ratio<-2/3
> set.seed(111)
> learnind<-sample(length(golubY), size=floor(ratio*length(golubY)))
> ldaresult<-ldaCMA(X=golubX, y=golubY, learnind=learnind)
> show(ldaresult)
binary Classification with linear discriminant analysis
number of predictions: 13
> ftable(ldaresult)
number of missclassifications:  5 
missclassification rate:  0.385 
sensitivity: 0.167 
specificity: 1 
    predicted
true 0 1
   0 7 0
   1 5 1
> plot(ldaresult)

##Quadratic Discriminant Analysis:
> data(golub)
> golubY<-golub[,1]
> golubX<-as.matrix(golub[,2:11])
> ratio<-2/3
> set.seed(112)
> learnind<-sample(length(golubY), size=floor(ratio*length(golubY)))
> qdaresult<-qdaCMA(X=golubX,y=golubY,learnind=learnind)
> show(qdaresult)
binary Classification with quadratic discriminant analysis
number of predictions: 13
> ftable(qdaresult)
number of missclassifications:  3 
missclassification rate:  0.231 
sensitivity: 0 
specificity: 0.833 
    predicted
true  0  1
   0 10  2
   1  1  0
> plot(qdaresult)

##Diagonal Discriminant Analysis:
> data(golub)
> golubY<-golub[,1]
> golubX<-as.matrix(golub[,-1])
> ratio<-2/3
> set.seed(111)
> learnind<-sample(length(golubY), size=floor(ratio*length(golubY)))
> dldaresult<-dldaCMA(X=golubX,y=golubY,learnind=learnind)
> show(dldaresult)
binary Classification with diagonal discriminant analysis
number of predictions: 13
> ftable(dldaresult)
number of missclassifications:  0 
missclassification rate:  0 
sensitivity: 1 
specificity: 1 
    predicted
true 0 1
   0 7 0
   1 0 6
> plot(dldaresult)

##Shrunken Centroids Discriminant Analysis  
> data(golub)
> golubY<-golub[,1]
> golubX<-as.matrix(golub[,-1])
> set.seed(111)
> learnind<-sample(length(golubY), size=floor(2/3*length(golubY)))
> scdaresult<-scdaCMA(X=golubX,y=golubY,learnind=learnind)
> show(scdaresult)
binary Classification with shrunken centroids discriminant analysis
number of predictions: 13
> ftable(scdaresult)
number of missclassifications:  0 
missclassification rate:  0 
sensitivity: 1 
specificity: 1 
    predicted
true 0 1
   0 7 0
   1 0 6
> plot(scdaresult)

##Shrinkage Linear Discriminant Analysis:
> library(sda)
> data(golub)
> golubY<-golub[,1]
> golubX<-as.matrix(golub[,-1])
> ratio<-2/3
> set.seed(111)
> learnind<-sample(length(golubY), size=floor(ratio*length(golubY)))
> result<-shrinkldaCMA(X=golubX,y=golubY,learnind=learnind)
> show(result)
binary Classification with shrinkage linear discriminant analysis
number of predictions: 13
> ftable(result)
number of missclassifications:  4 
missclassification rate:  0.308 
sensitivity: 0.333 
specificity: 1 
    predicted
true 0 1
   0 7 0
   1 4 2
> plot(result)

##Fisher’s Linear Discriminant Analysis
> data(golub)
> golubY<-golub[,1]
> golubX<-as.matrix(golub[,2:11])
> ratio<-2/3
> set.seed(111)
> learnind<-sample(length(golubY), size=floor(ratio*length(golubY)))
> fdaresult<-fdaCMA(X=golubX,y=golubY,learnind=learnind,comp=1,plot=TRUE)
> show(fdaresult)
binary Classification with Fisher's linear discriminant
number of predictions: 13
> ftable(fdaresult)
number of missclassifications:  5 
missclassification rate:  0.385 
sensitivity: 0.5 
specificity: 0.714 
    predicted
true 0 1
   0 5 2
   1 3 3
> plot(fdaresult)

## Flexible Discriminant Analysis 
> data(golub)
> golubY<-golub[,1]
> golubX<-as.matrix(golub[,2:11])
> ratio<-2/3
> set.seed(111)
> learnind<-sample(length(golubY), size=floor(ratio*length(golubY)))
> result<-flexdaCMA(X=golubX,y=golubY,learnind=learnind,comp=1)
> show(result)
binary Classification with flexible discriminant analysis
number of predictions: 13
> ftable(result)
number of missclassifications:  5 
missclassification rate:  0.385 
sensitivity: 0.5 
specificity: 0.714 
    predicted
true 0 1
   0 5 2
   1 3 3
> plot(result)
                