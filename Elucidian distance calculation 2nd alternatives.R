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
