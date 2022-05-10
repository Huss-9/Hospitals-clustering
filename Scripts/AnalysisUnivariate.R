rm(list=ls())
setwd("C:/Users/odnam/Documents/Universitat/Informatica/4_1/MD/Dataset")
dd <- read.csv("Definitiu_07_08_2020.csv",sep = ';')

class(dd)
dim(dd)
n<-dim(dd)[1]
n
K<-dim(dd)[2]
K

names(dd)

listOfColors<-rainbow(K)

names(dd)
class(dd)
class(dd[,1])
sapply(dd, class)


dd$state <- factor(dd$state)
dd$city <- factor(dd$city)
dd$fips_code <- factor(dd$fips_code)
dd$hospital_subtype <- factor(dd$hospital_subtype)
dd$zip <- factor(dd$zip)
dd$hospital_name <- factor(dd$hospital_name)
dd$is_metro_micro <- factor(dd$is_metro_micro)
dd$there_are_pediatric_patients_hospitalized_confirmed_and_suspected_covid_7_day <- factor(dd$there_are_pediatric_patients_hospitalized_confirmed_and_suspected_covid_7_day)

names(dd)
class(dd)
class(dd[,1])
sapply(dd, class)

par(ask=FALSE)

setwd("C:/Users/odnam/Documents/Universitat/Informatica/4_1/MD/Dataset/Graphics")

for(k in 1:K){
  if (is.factor(dd[,k])){ 
    frecs<-table(dd[,k], useNA="ifany")
    proportions<-frecs/n
    png(paste0("Pie of ", names(dd)[k],".png"))
    pie(frecs, cex=0.6, main=paste("Pie of", names(dd)[k]))
    dev.off()
    png(paste0("Barplot of ", names(dd)[k],".png"))
    barplot(frecs, las=3, cex.names=0.7, main=paste("Barplot of", names(dd)[k]), col=listOfColors)
    dev.off()
    print(frecs)
    print(proportions)
   }else{
    png(paste0("Histogram of ",names(dd)[k],".png"))
    hist(dd[,k], main=paste("Histogram of", names(dd)[k]))
    dev.off()
    png(paste0("Boxplot of ",names(dd)[k],".png"))
    boxplot(dd[,k], horizontal=TRUE, main=paste("Boxplot of", names(dd)[k]))
    dev.off()
    print(summary(dd[,k]))
    print(paste("sd: ", sd(dd[,k])))
    print(paste("vc: ", sd(dd[,k])/mean(dd[,k])))
   }
}