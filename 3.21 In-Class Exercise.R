#PCA on USArrests
#import the dataset
data("USArrests")

states=row.names(USArrests)
states
#variables in columns
names(USArrests)

apply(USArrests, 2, mean)
apply(USArrests, 2, var)

prout=prcomp(USArrests, scale=TRUE)
names(prout)
prout$center
prout$scale
prout$rotation

dim(prout$x)
#plot
biplot(prout, scale=0)
prout$sdev

prvar = prout$sdev^2
prvar
pve = prvar/sum(prvar)
pve

#PCA on iris
data("iris")
head(iris)
irisdata1 <- iris[,1:4]
irisdata1
#summary table
principal_components <- princomp(irisdata1, cor = TRUE, score = TRUE)
summary(principal_components)
#plots
plot(principal_components)
plot(principal_components, type = "l")
biplot(principal_components)

#PCA on Boston
install.packages('MASS')
data(Boston, package="MASS")
#computes the principal components
pca.out <- prcomp(Boston,scale. = T)
pca.out
plot(pca.out)
#create the plot and show the data
biplot(pca.out, scale = 0)
boston.pc <- pca.out$x
boston.pc
#summary table
summary(boston.pc)