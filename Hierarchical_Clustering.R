#Import Data
Mydata <- read.csv("https://raw.githubusercontent.com/Slytherin1112/USDA_Project1/master/Label_Properties_Teracore.csv", header = TRUE, stringsAsFactors=FALSE)
Label_Data <- Mydata[,-c(8:18)]
Property_Data <- Mydata[,c(1,8:17)]

#Define function from formular (Max-Min Norm)
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

#Normalization: Min-Max Scaling
Mydata_Norm <- apply(Mydata[,-c(1)],2,normalize)
New_Data <- cbind(Mydata[1], Mydata_Norm)
Label_Data_Norm <- New_Data[,-c(8:18)]
Property_Data_Norm <- New_Data[,c(1,8:17)]
Label_Prop_Norm <- New_Data[,-c(18)]

#Caculate the Euclidean distance 
Distance <- dist(New_Data)
Label_distance <- dist(Label_Data_Norm, method = "euclidean")
Property_distance <- dist(Property_Data_Norm, method = "euclidean")
Lab_prop_distance <- dist(Label_Prop_Norm, method = "euclidean")

#Clustering Dendrogram (Complete)
hc <- hclust(Distance)
label_hc <- hclust(Label_distance)
property_hc <- hclust(Property_distance)
label_prop_hc <- hclust(Lab_prop_distance)

#Hierarchical Clustering Plot 
plot(label_hc, labels = New_Data$Product, main = "Clustering Dendrogram (Label)")
plot(property_hc, labels = New_Data$Product, main = "Clustering Dendrogram (Property)")
plot(label_prop_hc, labels = New_Data$Product, main = "Clustering Dendrogram (Label & Property)")
plot(hc, labels = New_Data$Product, main = "Clustering Dendrogram (Label, Property & Teracore)")



