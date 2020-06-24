#Read Data 
Label_Data <- read.csv("https://raw.githubusercontent.com/Slytherin1112/USDA_Project1/master/Food_Label_Data.csv", header = TRUE)
Data1 = Label_Data[c(2:18),c(1:9)]
Data2 = Label_Data[c(2:18),c(1:3,11:20)]

#Normalization 

#Caculate the Euclidean distance 
distance1 <- dist(Data1)
distance2 <- dist(Data2)
print(distance1, digits = 2)
print(distance2, digits = 2)

#Clustering Dendrogram (Complete)
hc1 <- hclust(distance1) 
hc2 <- hclust(distance2)
plot(hc1, labels = Data1$Product, hang = -1, xlab = "Category", ylab = "Distance", main = "Clustering Dendrogram (Label)")
plot(hc2, labels = Data2$Product, hang = -1, xlab = "Category", ylab = "Distance", main = "Clustering Dendrogram (Property)")

#Clustering Dendrogram (Average)
hc1Avg <- hclust(distance1, method = "average")
hc2Avg <- hclust(distance2, method = "average")
plot(hc1Avg, labels = Data1$Product, hang = -1, xlab = "Category", ylab = "Distance", main = "Clustering Dendrogram (Label Average)")
plot(hc2Avg, labels = Data1$Product, hang = -1, xlab = "Category", ylab = "Distance", main = "Clustering Dendrogram (Property Average)")

