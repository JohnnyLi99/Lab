#Exercise 1
plot(ecdf(EPI_data$EPI),do.points=FALSE.verticals = TRUE)
plot(ecdf(EPI_data$EPI),do.points=TRUE, verticals = TRUE)
par(pty="s")
ggnorm(EPI_data$EPI)
ggline(EPI_data$EPI)
x <- seg(30,95,1)
x
x2 <- seg(30,95,2)
x2
x2 <- seg(30,96,2)
x2
ggplot(qt(ppoints(250),df=5),x,xlab = "Q-Q plot")
ggline(x)