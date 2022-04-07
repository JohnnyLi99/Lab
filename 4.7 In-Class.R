#April 7 In Class Lab

#install packages
install.packages('MASS')
data(Boston, package="MASS")
#shows the loadings
pca_out <- prcomp(Boston,scale. = T)
pca_out
plot(pca_out)

biplot(pca_out, scale = 0)
boston_pc <- pca_out$x
boston_pc
#desciriptive summary
head(boston_pc)
summary(boston_pc)