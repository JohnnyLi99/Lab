#read the file
multivariate <- read.csv("/Users/johnnyli1/Desktop/6600/multivariate.csv")
attach(multivariate)
names(multivariate)
multivariate

#create the scatterplots
plot(Income,Immigrant,main="Scatterplot")
plot(Immigrant,Homeowners)

#fitting using lm function
#help(lm)
nn <- lm(Homeowners ~ Immigrant)
plot(Immigrant,Homeowners)
abline(nn)
abline(nn,col=2,lwd=3)

summary(nn)
attributes(nn)
nn$coefficients

#Bar Graphs/Visualization
#Chapter 3, Bar-Graphs
install.packages(c("ggplot2", "gcookbook"))
library(ggplot2)
library(gcookbook)
ggplot(pg_mean,aes(x=group,y=weight))+geom_bar(stat="identity")
BOD
str(BOD)
ggplot(BOD,aes(x=Time,y=demand))+geom_bar(stat="identity")
ggplot(BOD,aes(x=factor(Time),y=demand))+geom_bar(stat = "identity")
ggplot(pg_mean,aes(x=group,y=weight))+geom_bar(stat = "identity",fill = "lightblue",colour="red")
ggplot(BOD,aes(x=factor(Time),y=demand))+geom_bar(stat = "identity",fill = "orange",colour="red")

#cabbage
cabbage_exp
ggplot(cabbage_exp,aes(x=Date,fill=Cultivar))+geom_bar(position = "dodge")
ggplot(cabbage_exp,aes(x=Date,y=Weight,fill = Cultivar))+geom_bar(stat = "identity")
ggplot(diamonds, aes(x=cut)) +geom_bar() # this is equvalent to using geom_bar(stat="bin)

data("diamonds")
diamonds
ggplot(diamonds,aes(x=carat)) + geom_bar()

#
ggplot(diamonds, aes(x=carat)) + geom_histogram()

ups <- subset(uspopchange, rank(Change)>40)
ups
ggplot(ups, aes(x=Abb, y= Change, fill=Region)) + geom_bar(stat = "identity")
ggplot(ups, aes(x=Abb, y=Change, fill=Region)) +geom_bin2d()
ggplot(ups, aes(x=Abb, y=Change, fill=Region)) + geom_col()
ggplot(ups, aes(x=reorder(Abb,Change), y=Change, fill=Region)) + geom_bar(stat = "identity", colour= "red") + scale_fill_manual(values=c("#669933", "#FFCC66")) + xlab("US-States")
ggplot(ups, aes(x=reorder(Abb,Change), y=Change, fill=Region)) + geom_bar(stat = "identity", color = "purple") + scale_fill_manual(values=c("#224455","#DDCC33"))

#
csub <- subset(climate, source="Berkeley" & Year >= 1900)
csub
csub$pos <- csub$Anomaly10y >=0
csub
ggplot(csub, aes(x=Year, y=Anomaly10y, fill= pos)) + geom_bar(stat = "identity", position = "identity")
ggplot(csub, aes(x=Year, y=Anomaly10y, fill=pos)) + geom_bar(stat="identity", colour="black", size=0.25) + scale_fill_manual(values=c("#CCEEFF", "#FFDDDD"), guide=FALSE)

ggplot(pg_mean, aes(x=group, y=weight)) +geom_bar(stat="identity")
ggplot(pg_mean, aes(x=group, y=weight)) +geom_bar(stat="identity", width = 0.5)
ggplot(pg_mean, aes(x=group, y=weight)) +geom_bar(stat = "identity", width = 0.95)
ggplot(cabbage_exp, aes(x=Date, y= Weight, fill=Cultivar)) + geom_bar(stat = "identity", width = 0.5, position = "dodge")
ggplot(cabbage_exp, aes(x=Date, y=Weight, fill=Cultivar)) + geom_bar(stat = "identity", width = 0.5, position = position_dodge(0.7))

#
ggplot(cabbage_exp, aes(x=Date, y=Weight, fill=Cultivar)) + geom_bar(stat = "identity")
cabbage_exp
ggplot(cabbage_exp, aes(x= Date, y= Weight, fill=Cultivar)) + geom_bar(stat = "identity") + guides(fill=guide_legend(reverse = TRUE))

ggplot(cabbage_exp, aes(x=interaction(Date,Cultivar), y=Weight)) +geom_bar(stat = "identity") + geom_text(aes(label=Weight),vjust=1.5,colour="white")

ggplot(cabbage_exp, aes(x=interaction(Date, Cultivar), y=Weight)) +
  geom_bar(stat="identity") +
  geom_text(aes(label=Weight), vjust=-0.2) +
  ylim(0, max(cabbage_exp$Weight) * 1.05)

ggplot(cabbage_exp, aes(x=interaction(Date, Cultivar), y=Weight)) +
  geom_bar(stat="identity") +
  geom_text(aes(y=Weight+0.1, label=Weight))

ggplot(cabbage_exp, aes(x=Date, y=Weight, fill=Cultivar)) +
  geom_bar(stat="identity", position="dodge") +
  geom_text(aes(label=Weight), vjust=1.5, colour="white",
            position=position_dodge(.9), size=3)
