install.packages("rpart.plot")
library(rpart)
library(rpart.plot)

#the iris dataset
iris
dim(iris)

#create a sample
sa_iris <- sample(150,100)
sa_iris
#test and train
iris_train <- iris[sa_iris,]
iris_test <- iris[-sa_iris,]
dim(iris_test)
dim(iris_train)
#generate decision tree
decisionTreeModel <- rpart(Species~., iris_train, method = "class")
decisionTreeModel
#plot
rpart.plot(decisionTreeModel)
