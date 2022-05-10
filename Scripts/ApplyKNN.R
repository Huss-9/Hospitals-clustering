rm(list=ls())
setwd("C:/Users/odnam/Documents/Universitat/Informatica/4_1/MD/Dataset")
dd <- read.csv("knn_friendly.csv",sep = ',')

attach(dd)

names(dd)

library(class)

fullVariables<-c(8,17);
aux<-dd[,fullVariables]
dim(aux)
names(aux)

uncompleteVars<-c(9,10,11,12,13,14,15,16,18,19,20,21,22,23,25,26)

for (k in uncompleteVars){
  aux1 <- aux[!is.na(dd[,k]),]
  dim(aux1) 
  aux2 <- aux[is.na(dd[,k]),]
  dim(aux2)
  
  RefValues<- dd[!is.na(dd[,k]),k]
  
  knn.values = knn(aux1,aux2,RefValues)   
  
  dd[is.na(dd[,k]),k] = as.numeric(as.character(knn.values))
  fullVariables<-c(fullVariables, k)
  aux<-dd[,fullVariables]
}

write.table(dd, file = "knn_friendly_modified.csv", sep = ',', row.names = FALSE, col.names = TRUE)