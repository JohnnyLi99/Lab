#3.31 IN-CLASS

#install library
library(e1071)
set.seed (1)
#generating the observations
x=matrix(rnorm(20*2), ncol=2)
y=c(rep(-1,10), rep(1,10))
x[y==1,]=x[y==1,] + 1
x
y
# checking if the classes are linearly separable
plot(x,col=(3-y))
#new data frame
dat <- data.frame(x = x,y = as.factor(y))
svmfit <- svm(y ~., data=dat, kernel="linear", cost=10,scale=FALSE)
#Plot the support vector classifier
plot(svmfit , dat)
#determine the identities 
svmfit$index
#description of the support vector classifier fit
summary(svmfit)
#if cost = 0.1
svmfit <- svm(y ~., data=dat, kernel="linear", cost = 0.1, scale=FALSE)
plot(svmfit , dat)
svmfit$index

#access the cross-validation errors for each model
set.seed (1)
tune.out <- tune(svm, y ~.,data=dat,kernel="linear", ranges=list(cost=c(0.001, 0.01, 0.1, 1,5,10,100)))
summary(tune.out)
#store the best model
bestmod=tune.out$best.model
summary(bestmod)
#generate a test data set
xtest=matrix(rnorm(20*2), ncol=2)
ytest=sample(c(-1,1), 20, rep=TRUE)
xtest[ytest==1,]=xtest[ytest==1,] + 1
testdat=data.frame(x=xtest, y=as.factor(ytest))
#predictions from using the best model obtained through cross-validation
ypred <-predict(bestmod ,testdat)
table(predict=ypred, truth=testdat$y)
#if cost = 0.01
svmfit <- svm(y~., data=dat, kernel="linear", cost=.01, scale=FALSE)
ypred=predict(svmfit ,testdat)
table(predict=ypred, truth=testdat$y)
# separate the two classes
x[y==1,]=x[y==1,]+0.5
plot(x, col=(y+5)/2, pch=19)
# fit the support vector classifier and plot the resulting hyperplane
dat=data.frame(x=x,y=as.factor(y))
svmfit <-svm(y~., data=dat, kernel="linear", cost=1e5)
summary(svmfit)
plot(svmfit,dat)
#if cost = 1
svmfit <- svm(y~., data=dat, kernel="linear", cost=1)
summary(svmfit)
plot(svmfit ,dat)
### when cost =1e5, model performs the best

#SVM Application to Gene Expression Dataset
#install library
library(ISLR)
names(Khan)
#examine the dimension
dim(Khan$xtrain )
dim(Khan$xtest )
length(Khan$ytrain )
length(Khan$ytest )
table(Khan$ytrain )
table(Khan$ytest )

dat <- data.frame(x=Khan$xtrain , y = as.factor(Khan$ytrain ))
out <- svm(y ~., data=dat, kernel="linear",cost=10)
summary(out)

dat.te=data.frame(x=Khan$xtest , y = as.factor(Khan$ytest ))
pred.te=predict(out, newdata=dat.te)
table(pred.te, dat.te$y)
