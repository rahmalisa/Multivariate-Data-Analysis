#DATA CONTOH KASUS
data1	<- read.csv(file.choose(),header=T,sep=";",dec=",")
data1
head(data1)

##MISAL:
y1=data1$Kapasitas.Terpasang
y2=data1$Tenaga.Listrik.Dibangkitkan
y3=data1$Listrik.Didistribusi

data<-as.matrix(cbind(y1,y2,y3))
head(data)

##UJI MULTIKOLINEARITAS
diag(solve(cor(data)))

##JARAK EUCLIDEAN
listrik = scale(data)
d = dist(listrik, method ="euclidean")
d

##Single Linkage
SL=hclust(d,method="single")
SL
plot(SL)
rect.hclust(SL,k=5,border="darkorange")

##Complete Linkage
CL=hclust(d,method="complete")
CL
plot(CL)
rect.hclust(CL,k=5,border="darkolivegreen")

##Average Linkage
AL=hclust(d,method="average")
AL
plot(AL)
rect.hclust(AL,k=5,border="purple")

##Wards
W=hclust(d,method="ward.D")
W
plot(W)
rect.hclust(W,k=5,border="deeppink2")

##Memotong Dendogram
#Misal, Wards.
cutree(W, k=2)
rect.hclust(W, k=2, border="red")

##Analisis Klaster Didukung dengan P-value##
library(pvclust)
fit=pvclust(data,method.hclust="ward.D",method.dist="euclidean")
plot(fit)
pvrect(fit,alpha=0.95)

##Non Hierarki
#Menentukan Banyak Klaster
wss <- (nrow(data))*sum(apply(data,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(data, centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Banyak Klaster",ylab="Jumlah Kuadrat dalam
Klaster")

#Analisis Klaster K Means
fit <- kmeans(data, 4)

#Menentukan Rata-Rata Klaster
aggregate(data,by=list(fit$cluster),FUN=mean)

#Keanggotaan Klaster
agt_klaster<- data.frame(data, fit$cluster)

#Plot Hasil
library(cluster)
clusplot(data, fit$cluster, color=TRUE, shade=TRUE, labels=2, lines=0)

library(fpc)
plotcluster(data, fit$cluster)

#Membandingkan 2 hasil klaster ??
library(fpc)
cluster.stats(d, W$cluster, AL$cluster)