# creating a matrix data with random numbers
# plotting the matrix using the image function
# there is no real pattern in the plot.
set.seed(12345)
#help(par)

par(mar = rep(0.2,4))
data_Matrix <-matrix(rnorm(400), nrow = 40)
image(1:10, 1:40, t(data_Matrix)[,nrow(data_Matrix):1])

par(mar=rep(0.2,4))
heatmap(data_Matrix)

set.seed(678910)
for(i in 1:40){
  # flipping a coin and getting the data
  coin_Flip <- rbinom(1, size = 1, prob = 0.5)
  # if the coin is "Heads", add a common pattern to that row,
  if(coin_Flip){
    data_Matrix[i, ] <- data_Matrix[i, ] + rep(c(0,3), each =5)
  }
}

#plot the data
par(mar= rep(0.2, 4))
image(1:10, 1:40, t(data_Matrix)[, nrow(data_Matrix):1])

#heatmap
par(mar=rep(0.2, 4))
heatmap(data_Matrix)


hh <- hclust(dist(data_Matrix))
data_Matrix_Ordered <- data_Matrix[hh$order,]
par(mfrow=c(1,3))
image(t(data_Matrix_Ordered)[, nrow(data_Matrix_Ordered):1])
plot(rowMeans(data_Matrix_Ordered),40:1, ,xlab="The Row Mean",ylab="Row",pch=19)
plot(colMeans(data_Matrix_Ordered),xlab="Column",ylab = "Column Mean",pch=19)


#Outliers in Data
cars1 <- cars[1:30,] # first 30 rows of the original cars dataset.
head(cars1)
#introduce some additional data points
cars_outliers <- data.frame(speed=c(19,19,20,20,20), dist=c(190,186,210,220,218)) 
head(cars_outliers)
cars2 <- rbind(cars1, cars_outliers)

par(mfrow=c(1, 2))
plot(cars2$speed, cars2$dist, xlim=c(0, 28), ylim=c(0, 230), main="With Outliers", xlab="speed", ylab="dist",
     pch="*", col="red", cex=2)
abline(lm(dist ~ speed, data=cars2), col="blue", lwd=3, lty=2)
#plot the data
plot(cars1$speed, cars1$dist, xlim=c(0, 28), ylim=c(0, 230), main="Without Outliers",
     xlab="speed", ylab="dist", pch="*", col="red", cex=2)
abline(lm(dist ~ speed, data=cars1), col="blue", lwd=3, lty=2)