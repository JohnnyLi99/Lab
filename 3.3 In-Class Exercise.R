#March 3rd

#Cook's distance
mtcars
head(mtcars)
str(mtcars)
modelM <- lm(mpg~cyl+wt,data=mtcars)
modelM
#create the plot
plot(modelM,pch=18,col="red",which = c(4))
#identify distance
cooks.distance(modelM)
CookDistance <- cooks.distance(modelM)
#round(CookDistance,5)
sort(round(CookDistance,5))

#In-Class work
#install packages for later analysis
install.packages('ISLR')
library(ISLR)
library(dplyr)
#import the dataset
Hitters
head(Hitters)
dim(Hitters)
#check for missing values
is.na(Hitters)
#remove the N/A values
HData <- na.omit(Hitters)
#check the dimensions after removed the N/A values
dim(HData)
glimpse(HData)
head(HData)
#create model to predict the salary
SalaryPredictM1 <- lm(Salary~.,data=HData)
summary(SalaryPredictM1)
#cook's distance
CookDis <- cooks.distance(SalaryPredictM1)
influential <- CookDis[(CookDis > (3*mean(CookDis,na.rm = TRUE)))]
influential
#check names of the outliers
NameInf <- names(influential)
NameInf
outliers <- HData[NameInf,]
WithoutOutliers <- HData %>% anti_join(outliers)
#create second model that predicts salary without outliers
SalaryPredictM2 <- lm(Salary~., data = WithoutOutliers)
summary(SalaryPredictM2)