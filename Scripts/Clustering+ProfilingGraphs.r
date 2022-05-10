setwd("C:/Users/odnam/Downloads")
dd <- read.csv("DATASET_ENTREGA_3_DEFINITIU.csv", sep=",");
names(dd)

attach(dd)

for(i in 1:ncol(dd)){
   if(is.character(dd[,i])){
      dd[,i]<-as.factor(dd[,i])
   }
   if(is.logical(dd[,i])){
      dd[,i]<-as.factor(dd[,i])
   }
}


library(cluster)

actives<-c(1,3:29)
dissimMatrix <- daisy(dd[,actives], metric = "gower", stand=TRUE)

distMatrix<-dissimMatrix^2

h1 <- hclust(distMatrix,method="ward.D2")
plot(h1)

k<-3

c2 <- cutree(h1,k)

ValorTestXnum <- function(Xnum,P){
  nk <- as.vector(table(P)); 
  n <- sum(nk); 
  xk <- tapply(Xnum,P,mean);
  txk <- (xk-mean(Xnum))/(sd(Xnum)*sqrt((n-nk)/(n*nk))); 
  pxk <- pt(txk,n-1,lower.tail=F);
  for(c in 1:length(levels(as.factor(P)))){if (pxk[c]>0.5){pxk[c]<-1-pxk[c]}}
  return (pxk)
}


ValorTestXquali <- function(P,Xquali){
  taula <- table(P,Xquali);
  n <- sum(taula); 
  pk <- apply(taula,1,sum)/n;
  pj <- apply(taula,2,sum)/n;
  pf <- taula/(n*pk);
  pjm <- matrix(data=pj,nrow=dim(pf)[1],ncol=dim(pf)[2]);      
  dpf <- pf - pjm; 
  dvt <- sqrt(((1-pk)/(n*pk))%*%t(pj*(1-pj))); 
  zkj <- dpf
  zkj[dpf!=0]<-dpf[dpf!=0]/dvt[dpf!=0]; 
  pzkj <- pnorm(zkj,lower.tail=F);
  for(c in 1:length(levels(as.factor(P)))){for (s in 1:length(levels(Xquali))){if (pzkj[c,s]> 0.5){pzkj[c,s]<-1- pzkj[c,s]}}}
  return (list(rowpf=pf,vtest=zkj,pval=pzkj))
}


dades<-dd
K<-dim(dades)[2]
par(ask=TRUE)

P<-c2

nc<-length(levels(factor(P)))
pvalk <- matrix(data=0,nrow=nc,ncol=K, dimnames=list(levels(P),names(dades)))
nameP<-"Class"
n<-dim(dades)[1]

for(k in 1:K){
  if (is.numeric(dades[,k])){ 
      print(paste("An?lisi per classes de la Variable:", names(dades)[k]))

      boxplot(dades[,k]~P, main=paste("Boxplot of", names(dades)[k], "vs", nameP ), horizontal=TRUE)
      
      barplot(tapply(dades[[k]], P, mean),main=paste("Means of", names(dades)[k], "by", nameP ))
      abline(h=mean(dades[[k]]))
      legend(0,mean(dades[[k]]),"global mean",bty="n")
      print("Estad?stics per groups:")
      for(s in levels(as.factor(P))) {print(summary(dades[P==s,k]))}
      o<-oneway.test(dades[,k]~P)
      print(paste("p-valueANOVA:", o$p.value))
      kw<-kruskal.test(dades[,k]~P)
      print(paste("p-value Kruskal-Wallis:", kw$p.value))
      pvalk[,k]<-ValorTestXnum(dades[,k], P)
      print("p-values ValorsTest: ")
      print(pvalk[,k])      
      }else{
        if(class(dd[,k])=="Date"){
          print(summary(dd[,k]))
          print(sd(dd[,k]))
          hist(dd[,k],breaks="weeks")
        }else{
      print(paste("Variable", names(dades)[k]))
      table<-table(P,dades[,k])
      rowperc<-prop.table(table,1)

   colperc<-prop.table(table,2)

   dades[,k]<-as.factor(dades[,k])
   
   marg <- table(as.factor(P))/n
   print(append("Categories=",levels(as.factor(dades[,k]))))

   plot(marg,type="l",ylim=c(0,1),main=paste("Prop. of pos & neg by",names(dades)[k]))
   paleta<-rainbow(length(levels(dades[,k])))
   for(c in 1:length(levels(dades[,k]))){lines(colperc[,c],col=paleta[c]) }

   plot(marg,type="l",ylim=c(0,1),main=paste("Prop. of pos & neg by",names(dades)[k]))
   paleta<-rainbow(length(levels(dades[,k])))
   for(c in 1:length(levels(dades[,k]))){lines(colperc[,c],col=paleta[c]) }
   legend("topright", levels(dades[,k]), col=paleta, lty=2, cex=0.6)
   
   print(append("Categories=",levels(dades[,k])))
   plot(marg,type="n",ylim=c(0,1),main=paste("Prop. of pos & neg by",names(dades)[k]))
   paleta<-rainbow(length(levels(dades[,k])))
   for(c in 1:length(levels(dades[,k]))){lines(rowperc[,c],col=paleta[c]) }
   
   plot(marg,type="n",ylim=c(0,1),main=paste("Prop. of pos & neg by",names(dades)[k]))
   paleta<-rainbow(length(levels(dades[,k])))
   for(c in 1:length(levels(dades[,k]))){lines(rowperc[,c],col=paleta[c]) }
   legend("topright", levels(dades[,k]), col=paleta, lty=2, cex=0.6)
   

   marg <-table(dades[,k])/n
      print(append("Categories=",levels(dades[,k])))
      plot(marg,type="l",ylim=c(0,1),main=paste("Prop. of pos & neg by",names(dades)[k]), las=3)
   paleta<-rainbow(length(levels(as.factor(P))))
      for(c in 1:length(levels(as.factor(P)))){lines(rowperc[c,],col=paleta[c]) }

      plot(marg,type="l",ylim=c(0,1),main=paste("Prop. of pos & neg by",names(dades)[k]), las=3)
      for(c in 1:length(levels(as.factor(P)))){lines(rowperc[c,],col=paleta[c])}
      legend("topright", levels(as.factor(P)), col=paleta, lty=2, cex=0.6)
   
   plot(marg,type="n",ylim=c(0,1),main=paste("Prop. of pos & neg by",names(dades)[k]), las=3)
   paleta<-rainbow(length(levels(as.factor(P))))
   for(c in 1:length(levels(as.factor(P)))){lines(colperc[c,],col=paleta[c]) }
   
   plot(marg,type="n",ylim=c(0,1),main=paste("Prop. of pos & neg by",names(dades)[k]), las=3)
   for(c in 1:length(levels(as.factor(P)))){lines(colperc[c,],col=paleta[c])}
   legend("topright", levels(as.factor(P)), col=paleta, lty=2, cex=0.6)
   
      table<-table(dades[,k],P)
      print("Cross Table:")
      print(table)
      print("Distribucions condicionades a columnes:")
      print(colperc)
      
      paleta<-rainbow(length(levels(dades[,k])))
      barplot(table(dades[,k], as.factor(P)), beside=FALSE,col=paleta )

      barplot(table(dades[,k], as.factor(P)), beside=FALSE,col=paleta )
      legend("topright",levels(as.factor(dades[,k])),pch=1,cex=0.5, col=paleta)
      
      barplot(table(dades[,k], as.factor(P)), beside=TRUE,col=paleta )

      barplot(table(dades[,k], as.factor(P)), beside=TRUE,col=paleta)
      legend("topright",levels(as.factor(dades[,k])),pch=1,cex=0.5, col=paleta)
   
      print("Test Chi quadrat: ")
      print(chisq.test(dades[,k], as.factor(P)))
   
      print("valorsTest:")
      print( ValorTestXquali(P,dades[,k]))
   }
      }
}

for (c in 1:length(levels(as.factor(P)))) {
  if(!is.na(levels(as.factor(P))[c])){
    print(paste("P.values per class:",levels(as.factor(P))[c]));
    print(sort(pvalk[c,]), digits=3) 
  }
}