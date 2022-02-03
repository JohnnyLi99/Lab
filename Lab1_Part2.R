#Feb 3
plot(mtcars$wt,mtcars$mpg)
library(ggplot2)
qplot(mtcars$wt,mtcars$mpg)
qplot(wt,mpg,data = mtcars)
ggplot(mtcars,aes(x=wt,y=mpg)) + geom_point()
plot(pressure$temperature,pressure$pressure,type="l")
points(pressure$temperature,pressure$pressure)

lines(pressure$temperature,pressure$pressure/2,col="red")
points(pressure$temperature,pressure$pressure/2,col="blue")
library(ggplot2)
qplot(pressure$temperature,pressure$pressure,geom="line")
qplot(temperature,pressure,data = pressure, geom = "line")
ggplot(pressure,aes(x=temperature,y=pressure)) + geom_line() + geom_point()

#creating bar graphs
barplot(BOD$demand,names.arg = BOD$Time)
table(mtcars$cyl) #generate content's table
barplot(table(mtcars$cyl)) 
qplot(mtcars$cyl)
qplot(factor(mtcars$cyl))
#bar graph of contents
qplot(factor(cyl),data=mtcars)
ggplot(mtcars,aes(x=factor(cyl))) + geom_bar()