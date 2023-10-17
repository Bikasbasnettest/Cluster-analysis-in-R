setwd("E:/MNCH 74-75W Report to press basnet/PCA analyis Folder")
Biku<-read.csv("E:/MNCH 74-75W Report to press basnet/PCA analyis Folder/PCA anaylis of the unused data set of over Location with wrong value of RLOD and SlOD.csv")
attach(Biku)
print(Biku)
View(Biku)
Elimin<-Biku[,-c(1,1)]
Elimin
options(max.print = 1000)
options(scipen = 1000)
library(NbClust)
distance<-dist(Elimin)
distance
print(distance,digits=3)
hc<-hclust(distance)
plot(hc,labels=Biku$Gen)
rect.hclust(hc, k=4, border = "purple")
rect.hclust(hc, k=4, border = 2:6)
#for creation of the Circular Diagram of the Cluster
#FOR Creation of Circulr Dendrogram
library(circlize)
circlize_dendrogram(hc)
library(dendextend)
dend <- as.dendrogram(hc)
dend <- color_branches(dend, k = 4)  
dend <- set(dend, "branches_lwd", 2)
par(mar = c(1, 1, 1, 1))  
circlize_dendrogram(dend, labels_track_height = 0.1) 
genotype_names <- as.character(Bik$Var)  
labels(dend) <- genotype_names
par(mar = c(1, 1, 1, 1))  
P2<-circlize_dendrogram(dend, labels_track_height = 0.1)
