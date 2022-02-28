#Validation
library(ISLR)
library(MASS)
library(boot)

train = sample(392,196)
lm.fit <- lm(mpg~horsepower, data = Auto, subset = train)
attach(Auto)
mean((mpg-predict(lm.fit,Auto))[-train]^2)
lm.fit2 <- lm(mpg~poly(horsepower,2), data = Auto, subset = train) 
mean((mpg-predict(lm.fit2,Auto))[-train]^2) 
lm.fit3 <- lm(mpg~poly(horsepower,3), data = Auto, subset = train)
mean((mpg-predict(lm.fit3,Auto))[-train]^2)

train = sample(392,196)
lm.fit <- lm(mpg~horsepower, data = Auto, subset = train)
mean((mpg-predict(lm.fit,Auto))[-train]^2)

lm.fit2 <- lm(mpg~poly(horsepower,2), data = Auto, subset = train)
mean((mpg-predict(lm.fit2,Auto))[-train]^2)

lm.fit3 <- lm(mpg~poly(horsepower,3), data = Auto, subset = train)
mean((mpg-predict(lm.fit3,Auto))[-train]^2)


#Random Forest Example

require(devtools)
install_version("randomForest", version = "4.6-14", repos = "http://cran.us.r-project.org")

install.packages("randomForest")

library(randomForest)
rf <- read.csv(file.choose(),header=TRUE)
head(rf)

colnames(rf) <- c("BuyingPrice","Maintenance","NumDoors","NumPersons","BootSpace","Safety","Condition")
head(rf)
str(rf)

levels(rf$Condition)
summary(rf)

set.seed(100)
train <- sample(nrow(rf),0.7*nrow(rf),replace=FALSE)
TrainSet <- rf[train,]
ValidSet <- rf[-train,]
summary(TrainSet)
summary(ValidSet)

model <- randomForest(Condition ~ ., data = TrainSet, importance = TRUE)
model

model2 <- randomForest(Condition ~ ., data = TrainSet, ntree = 500, mtry = 6, importance = TRUE)
model2

predTrain <- predict(model2,TrainSet,type="class")
table(predTrain,TrainSet$Condition)
predValid <- predict(model2,ValidSet,type="class")
table(predValid,ValidSet$Condition)

importance(model2)
varImpPlot(model2)

a =c()
i = 5
for (i in 3:8) {
  model3 <- randomForest(Condition ~., data = TrainSet, ntree = 500, mtry = i, importance = TRUE)
  predValid <- predict(model3,ValidSet,type="class")
  a[i-2] = mean(predValid == ValidSet$Condition)
}
a
plot(3:8,a)

library(rpart)
library(caret)
library(e1071)

modeldt <- train(Condition ~ ., data=TrainSet, method = "rpart")
modeldt1 = predict(model_dt,data=TrainSet)
table(modeldt1,TrainSet$Condition)
mean(modeldt1 == TrainSet$Condition)

modeldt_vs = predict(modeldt,newdata = ValidSet)
table(modeldt_vs,ValidSet$Condition)
mean(modeldt_vs == ValidSet$Condition)