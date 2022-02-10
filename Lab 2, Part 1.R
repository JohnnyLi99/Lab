##Lab2 Part1a

EPI_data <- read.csv("EPI_data.csv")
attach(EPI_data)
install.packages("psych")
install.packages("DescTools")

#Measures of Central Tendency
summary(EPI)
library(psych)
mean(EPI,na.rm=TRUE)  #Arithmetic Mean
geometric.mean(EPI)  #Geometric Mean
harmonic.mean(EPI)  #Harmonic Mean
median(EPI,na.rm=TRUE)  #Median

summary(DALY)
mean(DALY,na.rm=TRUE)  #Arithmetic Mean
geometric.mean(DALY)  #Geometric Mean
harmonic.mean(DALY)  #Harmonic Mean
median(DALY,na.rm=TRUE)  #Median

#Generate the Histogram
hist(EPI)
hist(DALY)

boxplot(ENVHEALTH,ECOSYSTEM) #Generate the Boxplot
qqplot(ENVHEALTH,ECOSYSTEM) #Generate the Q-Q plot

##Lab2 Part1b Regression Exercise

#EPI Regression
#Linear & Least-Squares
boxplot(EPI,DALY,AIR_H,WATER_H)
lmEPI <- lm(EPI~DALY+AIR_H+WATER_H)
lmEPI
summary(lmEPI) 
cEPI <- coef(lmEPI)
#Predict
DALYNEW <- c(seq(5,95,5))
AIR_HNEW <- c(seq(5,95,5))
WATER_HNEW <- c(seq(5,95,5))
NEW <- data.frame(DALYNEW,AIR_HNEW,WATER_HNEW)
pEPI <- predict(lmEPI,NEW,interval = "prediction")
cEPI <- predict(lmEPI,NEW,interval = "confidence")

#AIR_E Regression
boxplot(AIR_E,DALY,AIR_H,WATER_H)
lmAIRE <- lm(AIR_E~DALY+AIR_H+WATER_H)
lmAIRE
summary(lmAIRE)
cAIRE <- coef(lmAIRE)
#Predict
DALYNEW <- c(seq(5,95,5))
AIR_HNEW <- c(seq(5,95,5))
WATER_HNEW <- c(seq(5,95,5))
NEW <- data.frame(DALYNEW,AIR_HNEW,WATER_HNEW)
pAIRE <- predict(lmAIRE,NEW,interval = "prediction")
cAIRE <- predict(lmAIRE,NEW,interval = "confidence")

#CLIMATE Regression
boxplot(CLIMATE,DALY,AIR_H,WATER_H)
lmCLIM <- lm(CLIMATE~DALY+AIR_H+WATER_H)
lmCLIM
summary(lmCLIM)
cAIRE <- coef(lmCLIM)
#Predict
DALYNEW <- c(seq(5,95,5))
AIR_HNEW <- c(seq(5,95,5))
WATER_HNEW <- c(seq(5,95,5))
NEW <- data.frame(DALYNEW,AIR_HNEW,WATER_HNEW)
pCLIM <- predict(lmCLIM,NEW,interval = "prediction")
cCLIM <- predict(lmCLIM,NEW,interval = "confidence")
