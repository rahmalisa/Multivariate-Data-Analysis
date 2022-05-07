##INPUT DATA
data=read.csv(file.choose(),header=TRUE,sep=";",dec=",")
kota=data[,1]
data=data[,2:8]
rownames(data)=kota
data

#METRIK
d <- dist(data) # jarak euclidean antar baris
fit <- cmdscale(d,eig=TRUE, k=2) # dalam 2 dimensi
fit

x <- fit$points[,1]
y <- fit$points[,2]

plot(x, y, xlab="x", ylab="y",
main="Classical MDS", type="n")
abline(v=0)
abline(h=0)
text(x, y, labels = row.names(data), cex=.7)
	
#NON METRIK
library(MASS)
d <- dist(data[-1]) # jarak euclidean antar baris
fit <- isoMDS(d, k=2) 
fit

fit$stress

x <- fit$points[,1]
y <- fit$points[,2]

plot(x, y, main="Nonmetric MDS", type="n", pch=20)
abline(v=0)
abline(h=0)
text(x, y, labels = row.names(data), cex=.7)