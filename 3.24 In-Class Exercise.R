#March 24 In Class Exercise

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

#PCA on wine data
wine_data <- read.table("/Users/johnnyli1/Desktop/wine.data", sep = ",")
head(wine_data)
nrow(wine_data)
#adding names
colnames(wine_data) <- c("Cvs", "Alcohol",
                         "Malic_Acid", "Ash", "Alkalinity_of_Ash",
                         "Magnesium", "Total_Phenols", "Flavanoids", "NonFlavanoid_Phenols",
                         "Proanthocyanins", "Color_Intensity", "Hue", "OD280/OD315_of_Diluted_Wine",
                         "Proline")
head(wine_data)
#create the heatmap
heatmap(cor(wine_data),Rowv = NA, Colv = NA) 
cultivar_classes <- factor(wine_data$Cvs)
cultivar_classes
wine_data_PCA <- prcomp(scale(wine_data[,-1]))
summary(wine_data_PCA)
