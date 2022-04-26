#Data Analytics Assignment 6 - Term Assignment - Level 6000
#Peilun Li
##662019332

                                    ##################
                                    #   FILE MERGE   #
                                    ##################
#install.packages("dplyr")
#install.packages("plyr")
#install.packages("readr")
#install.packages("tidyverse")

#import libraries
library("dplyr")
library("plyr")
library("readr")
library("tidyr")
#import files
air <- read.csv("/Users/johnnyli1/Desktop/ad_viz_plotval_data.csv")
car <- read.csv("/Users/johnnyli1/Desktop/Vehicle Count.csv")
#read the headers and first few rows
head(air)
head(car)

#change the date format for 'car' & 'air'
car$newDate <- strptime(as.character(car$Reg.Valid.Date),"%m/%d/%y")
car$txtdate <- format(car$newDate, "%Y-%m-%d")
air$newDate <- strptime(as.character(air$Date),"%m/%d/%Y")
air$txtdate <- format(air$newDate, "%Y-%m-%d")

#check the new columns
head(air)

#create subsets for data frame 'air'
albanyA <- subset(air, COUNTY =="Albany")
bronxA <- subset(air, COUNTY =="Bronx")
chautauquaA <- subset(air, COUNTY =="Chautauqua")
erieA <- subset(air, COUNTY =="Erie")
essexA <- subset(air, COUNTY =="Essex")
kingsA <- subset(air, COUNTY =="Kings")
monroeA <- subset(air, COUNTY =="Monroe")
nassauA <- subset(air, COUNTY =="Nassau")
nyA <- subset(air, COUNTY =="New York")
oneidaA <- subset(air, COUNTY =="Oneida")
onondagaA <- subset(air, COUNTY =="Onondaga")
orangeA <- subset(air, COUNTY =="Orange")
queensA <- subset(air, COUNTY =="Queens")
richmondA <- subset(air, COUNTY =="Richmond")
rocklandA <- subset(air, COUNTY =="Rockland")
steubenA <- subset(air, COUNTY =="Steuben")
suffolkA <- subset(air, COUNTY =="Suffolk")
westchesterA <- subset(air, COUNTY =="Westchester")

#create subsets for data frame 'car'
albanyC <- subset(car, County =="ALBANY")
bronxC <- subset(car, County =="BRONX")
chautauquaC <- subset(car, County =="CHAUTAUQUA")
erieC <- subset(car, County =="ERIE")
essexC <- subset(car, County =="ESSEX")
kingsC <- subset(car, County =="KINGS")
monroeC <- subset(car, County =="MONROE")
nassauC <- subset(car, County =="NASSAU")
nyC <- subset(car, County =="NEW YORK")
oneidaC <- subset(car, County =="ONEIDA")
onondagaC <- subset(car, County =="ONONDAGA")
orangeC <- subset(car, County =="ORANGE")
queensC <- subset(car, County =="QUEENS")
richmondC <- subset(car, County =="RICHMOND")
rocklandC <- subset(car, County =="ROCKLAND")
steubenC <- subset(car, County =="STEUBEN")
suffolkC <- subset(car, County =="SUFFOLK")
westchesterC <- subset(car, County =="WESTCHESTER")

#head(nyC)
#summary(albanyA)

#remove duplicate date in 'air'
albanyA <- albanyA %>% distinct(Date, .keep_all = TRUE)
bronxA <- bronxA %>% distinct(Date, .keep_all = TRUE)
chautauquaA <- chautauquaA %>% distinct(Date, .keep_all = TRUE)
erieA <- erieA %>% distinct(Date, .keep_all = TRUE)
essexA <- essexA %>% distinct(Date, .keep_all = TRUE)
kingsA <- kingsA %>% distinct(Date, .keep_all = TRUE)
monroeA <- monroeA %>% distinct(Date, .keep_all = TRUE)
nassauA <- nassauA %>% distinct(Date, .keep_all = TRUE)
nyA <- nyA %>% distinct(Date, .keep_all = TRUE)
oneidaA <- oneidaA %>% distinct(Date, .keep_all = TRUE)
onondagaA <- onondagaA %>% distinct(Date, .keep_all = TRUE)
orangeA <- orangeA %>% distinct(Date, .keep_all = TRUE)
queensA <- queensA %>% distinct(Date, .keep_all = TRUE)
richmondA <- richmondA %>% distinct(Date, .keep_all = TRUE)
rocklandA <- rocklandA %>% distinct(Date, .keep_all = TRUE)
steubenA <- steubenA %>% distinct(Date, .keep_all = TRUE)
suffolkA <- suffolkA %>% distinct(Date, .keep_all = TRUE)
westchesterA <- westchesterA %>% distinct(Date, .keep_all = TRUE)

#add PM2.5 values to 'car' subsets
albanyC$PM2.5 <- albanyA$Daily.Mean.PM2.5.Concentration[match(albanyC$txtdate,albanyA$txtdate)]
bronxC$PM2.5 <- bronxA$Daily.Mean.PM2.5.Concentration[match(bronxC$txtdate,bronxA$txtdate)]
chautauquaC$PM2.5 <- chautauquaA$Daily.Mean.PM2.5.Concentration[match(chautauquaC$txtdate,chautauquaA$txtdate)]
erieC$PM2.5 <- erieA$Daily.Mean.PM2.5.Concentration[match(erieC$txtdate,erieA$txtdate)]
essexC$PM2.5 <- essexA$Daily.Mean.PM2.5.Concentration[match(essexC$txtdate,essexA$txtdate)]
kingsC$PM2.5 <- kingsA$Daily.Mean.PM2.5.Concentration[match(kingsC$txtdate,kingsA$txtdate)]
monroeC$PM2.5 <- monroeA$Daily.Mean.PM2.5.Concentration[match(monroeC$txtdate,monroeA$txtdate)]
nassauC$PM2.5 <- nassauA$Daily.Mean.PM2.5.Concentration[match(nassauC$txtdate,nassauA$txtdate)]
nyC$PM2.5 <- nyA$Daily.Mean.PM2.5.Concentration[match(nyC$txtdate,nyA$txtdate)]
oneidaC$PM2.5 <- oneidaA$Daily.Mean.PM2.5.Concentration[match(oneidaC$txtdate,oneidaA$txtdate)]
onondagaC$PM2.5 <- onondagaA$Daily.Mean.PM2.5.Concentration[match(onondagaC$txtdate,onondagaA$txtdate)]
orangeC$PM2.5 <- orangeA$Daily.Mean.PM2.5.Concentration[match(orangeC$txtdate,orangeA$txtdate)]
queensC$PM2.5 <- queensA$Daily.Mean.PM2.5.Concentration[match(queensC$txtdate,queensA$txtdate)]
richmondC$PM2.5 <- richmondA$Daily.Mean.PM2.5.Concentration[match(richmondC$txtdate,richmondA$txtdate)]
steubenC$PM2.5 <- steubenA$Daily.Mean.PM2.5.Concentration[match(steubenC$txtdate,steubenA$txtdate)]
suffolkC$PM2.5 <- suffolkA$Daily.Mean.PM2.5.Concentration[match(suffolkC$txtdate,suffolkA$txtdate)]
westchesterC$PM2.5 <- westchesterA$Daily.Mean.PM2.5.Concentration[match(westchesterC$txtdate,westchesterA$txtdate)]

#merge the subsets
merge <- bind_rows(albanyC,bronxC,chautauquaC,erieC,essexC,kingsC,monroeC,nassauC,nyC,oneidaC,onondagaC,orangeC,queensC,richmondC,steubenC,suffolkC,westchesterC)
#summary(merge)

#remove NA values from PM2.5
merge<-merge[!is.na(merge$PM2.5),]
#summary(merge)

#export data frame to csv file
write.csv(merge,"/Users/johnnyli1/Desktop/Vehicle with PM2.5.csv", row.names = FALSE)


                                            ##########
                                            #  EDA   #
                                            ##########

#import library
#install.packages("skimr")
#install.packages("plotly")
#install.packages("hrbrthemes")
#install.packages("caret")
#install.packages("TTR")
library(tidyverse)
library(skimr)
library(ggplot2)
library(dplyr)
library(plotly)
library(hrbrthemes)
library(lubridate)
library(plyr)
library(caret)
library(repr)
library(data.table)
library(TTR)
library(stats)
#read the file
df <- read.csv("/Users/johnnyli1/Desktop/Vehicle with PM2.5.csv")

summary(df)
str(df)
#check the column observations that contain NA values
df %>% count(Maximum.Gross.Weight)
df %>% count(Passengers)
df %>% count(Scofflaw.Indicator)
df %>% count(Suspension.Indicator)
df %>% count(Revocation.Indicator)

#drop the columns
md = subset(df, select = -c(Maximum.Gross.Weight,Passengers,Scofflaw.Indicator,Suspension.Indicator,Revocation.Indicator) )

#check the new data frame
head(md)
summary(md)
str(md)
skim(md)
summary(md$PM2.5)
#Display observations under County
md %>% count(County)

#PM2.5 Column Histogram ###
PM2.5 <- md$PM2.5
hist(PM2.5,
     main = "PM2.5 Concentration Distribution in NY State",
     xlab = "Daily Mean PM2.5 Concentration",
     xlim = c(-10,50),
     col = "ivory",
     freq = FALSE)
#histogram return values
h <- hist(PM2.5)
h

#boxplot
boxplot(md$PM2.5)

#ggplot(md$PM2.5) + 
#geom_histogram(mapping = aes(x = y), binwidth = 0.5)
###Display column width for PM2.5
#md %>% count(cut_width(PM2.5, 0.5))

### Time series plot of PM2.5 in NY state
md$Date <- as.Date(md$Date)
p <- md %>%
  ggplot( aes(x=Date, y=PM2.5)) +
  geom_area(fill="darkorchid4", alpha=0.5) +
  geom_line(color="darkorchid4") +
  ggtitle("PM2.5 Concentration in NY State") +
  ylab("Vehicle Count") +
  theme_ipsum() +
  scale_x_date(date_labels = "%b") +
  theme(axis.ticks = element_blank(),
        panel.background = element_blank())
p <- ggplotly(p)
p

###create new column for month
md[, "month"] <- format(md[,"Date"], "%m")
albanyPM <- md %>% filter(County == 'ALBANY')
min(albanyPM$Date)
max(albanyPM$Date)

ggplot(data = albanyPM, aes(Date,PM2.5)) +
  geom_bar(stat = "identity", fill = "darkorchid4") +
  xlab("Date") +
  ggtitle("PM2.5 Concentration in Albany") +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank())

summary(albanyPM$PM2.5)


#a copy of cleaned data frame for additional date columns
df2 <- data.frame(md)
#add date related features
df2$year = lubridate::year(df2$Date)
df2$yday = yday(df2$Date)
df2$quarter = quarter(df2$Date)
df2$month = lubridate::month(df2$Date)
df2$day = lubridate::day(df2$Date)
df2$weekdays = weekdays(df2$Date)
glimpse(df2)
#convert the data type
df2 = as.data.table(df2)
df2$month = as.factor(df2$month)
df2$weekdays = factor(df2$weekdays,levels = c("Monday", "Tuesday", "Wednesday","Thursday","Friday","Saturday",'Sunday'))
df2[weekdays %in% c("Saturday",'Sunday'),weekend:=1]
df2[!(weekdays %in% c("Saturday",'Sunday')),weekend:=0]
df2$weekend = as.factor(df2$weekend)
df2$year = as.factor(df2$year)
df2$quarter = as.factor(df2$quarter)
df2$week = format(df2$Date, "%V")
df2 = as.data.frame(df2)
df2$week = as.integer(df2$week)
glimpse(df2)

#plot for months Concentration
ggplot(df2, aes(x = month, y = PM2.5,group=month)) +  
  ggtitle("Monthly PM2.5 Concentration") +
  stat_summary(fun.data = mean_se, fun.args = list(mult = 1.96)) +
  geom_line(aes(x = as.numeric(month)))

                                            ##########
                                            # MODELS #
                                            ##########

#install.packages("partimat")
library(dplyr)
library(class)
library(car)
library(leaps)
library(rpart)
library(rpart.plot)
library(RColorBrewer)
library(rattle)
library(caTools)
library(party)
library(magrittr)
library(rpart)
library(tree)
library(caret)
#
str(df2)
pm <- data.frame(df2)
str(pm)
pm$Color <- as.factor(pm$Color)
pm = subset(pm, select = -c(Record.Type,VIN,Registration.Class,City,State,Zip,County,Model.Year,Make,
                            Body.Type,Fuel.Type,Reg.Valid.Date,Reg.Expiration.Date,Date,txtdate,weekdays) )

#check na
col1<- mapply(anyNA,pm) 
col1
#unclass
pmn <- sapply(pm, unclass)
head(pmn)
#normalize
normalize<- function(x){
  return((x-min(x))/(max(x)-min(x)))
}
pmn[,-c(4)]<- normalize(pmn[,-c(4)])
head(pmn)
#get class
class<- data.frame("month"= pmn$month)
names(class)= "Month"
pmn %>% select(-month)
head(pmn)
#train and test
rnum<- sample(rep(1:397690))
pm2<- pm[rnum,] 
class<- as.data.frame(class[rnum,])

pm.train<- pm2[1:318152,]
pm.train.target<- class[1:318152,]
pm.test<- pm2[318153:397690,]
pm.test.target<- class[318153:397690,]

str(pm2)

colSums(df4==0)
df4$Date <- NULL
df5<-df4[ , -which(names(df4) %in% c("Model.Year"))]


                              ### Multiple linear regression ###
lmPM2.5 = lm(PM2.5 ~ yday + day + Unladen.Weight, data = df4)
summary(lmPM2.5)
summary(lmPM2.5)$coefficient
avPlots(lmPM2.5)
confint(lmPM2.5)
sigma(lmPM2.5)/mean(df4$PM2.5)
#result not ideal
Subset <-
  regsubsets(PM2.5~., data =df5,
             nbest = 1,      
             nvmax = NULL,    
             force.in = NULL, force.out = NULL,
             method = "exhaustive")
#decide the better predictor features
summary_subset <- summary(Subset)
as.data.frame(summary_subset$outmat)
which.max(summary_subset$adjr2)
summary_subset$which[6,]
lmPM5 = lm(PM2.5 ~ ., data = df5)
summary(lmPM5)


#a little preprocess for models
df4 <- df3[df3$PM2.5 !=0,]
str(df4)
df4$MAKE <- as.factor(df4$MAKE)
df4c <- df4c[,2:length(df4c)]
df4$month <- df2$month


                                            ### knn ###
#dataset for KNN
dfk = subset(pm, select = -c(year,Unladen.Weight,month,quarter,Color,weekend,weekdays))
head(dfk)
sapply(dfk, class)
#split the data
vindex <- createDataPartition(dfk$PM2.5, p=0.80, list=FALSE)
validation <- dfk[-vindex,]
dataset <- dfk[vindex,]
#create x and y
x <- dataset[,1:4]
y <- dataset[,5]
#the boxplot for features
par(mfrow=c(1,4))
for(i in 1:4) {
  boxplot(x[,i], main=names(iris)[i])
}
#using the repeatedcv method to do 10 fold cross validation
control <- trainControl(method="repeatedcv",number=10)
metric <- "Accuracy"
set.seed(110)
fit.knn <- train(PM2.5~., data=dataset, method="knn", metric=metric, trControl=control)

#knn for the train dataset
mod <- train(PM2.5 ~ ., data = dataset, method = "knn")
mod


                                          ### Decision tree ###

dfd = subset(df2, select = -c(Record.Type,VIN,Registration.Class,City,State,Zip,County,Model.Year,Make,
                              Body.Type,Fuel.Type,Reg.Valid.Date,Reg.Expiration.Date,txtdate,Unladen.Weight,weekdays,
                              year,Color,Date,quarter,yday))
PMcat <- ifelse(dfd$PM2.5<=7.11,"Low","High")
dfd <- data.frame(dfd,PMcat)
dfd1 =subset(dfd,select=-c(PM2.5))


set.seed(110)
#dfd1$PMcat = factor(dfd1$PMcat, levels = c(0, 1))
split <- sample.split(Y = dfd1$PMcat, SplitRatio = 0.75)
train_set <- subset(x = dfd1, split == TRUE)
test_set <- subset(x = dfd1, split == FALSE)

tree <- rpart(
  PMcat ~ ., 
  data = train_set, method ="class")
  #control = rpart.control(minsplit = 2))
rpart.plot(tree, box.palette="RdBu",extra=101,type=1,nn=TRUE)
#rpart.plot(tree)
fancyRpartPlot(tree)
importance <- varImp(tree)
importance %>%
  arrange(desc(Overall))

pred <- predict(tree, test_set, type = "class")
confusionMatrix(pred,as.factor(test_set$PMcat))


                                              ### LDA ###
library(MASS)
library(ISLR)
head(df4)
head(df3)
df3copy <- data.frame(df3)

#Data partition
split <- sample.split(Y = df3copy$PM2.5, SplitRatio = 0.75)
train_set <- subset(x = df3copy, split == TRUE)
test_set <- subset(x = df3copy, split == FALSE)
pairs(df3copy)

#LDA for whole dataset
r <- lda(PM2.5 ~ ., data = df3copy)
r
r$prior
r$counts
r$means
r$scaling
r$svd
prop = r$svd^2/sum(r$svd^2)
prop

#build LDA model for prediction
lda.fit <- lda(PM2.5 ~ ., data = df3copy, subset = 1:318152)
lda.fit
summary(lda.fit)
#predict
lda.pred <- predict(lda.fit, df3copy[318153:397690,])
summary(lda.pred)
names(lda.pred)
lda.pred
# Predicted classes
head(lda.pred$class, 6)
# Predicted probabilities
head(lda.pred$posterior, 6) 
# Linear discriminant 
head(lda.pred$x, 3) 

lda.data <- cbind(df3copy[318153:397690,], lda.pred$x)
ggplot(lda.data, aes(LD1, LD2)) +
  geom_point(aes(color = PM2.5))
#plot
ldahist(data = lda.pred$x[,2], g = df3copy[1:318152,]$PM2.5)
#Accuracy of LDA
mean(lda.pred$class==df3copy[318153:397690,]$PM2.5)

