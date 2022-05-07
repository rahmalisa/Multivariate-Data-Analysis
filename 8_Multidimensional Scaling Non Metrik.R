##INPUT DATA
data=read.csv(file.choose(),header=TRUE,sep=";")
kota=data[,1]
data=data[,2:6]
rownames(data)=kota
data

((MULTIDIMENSIONAL SCALLING))
##CARA I
library(MASS)
d <- dist(data) # jarak euclidean antar baris
fit <- isoMDS(d, k=2) 
fit$points
fit$stress
x <- fit$points[,1] 
y <- fit$points[,2]

plot(x, y, main="Nonmetric MDS", type="n", pch=20)
abline(v=0)
abline(h=0)
text(x, y, labels = row.names(data), cex=.7)

##CARA II
mds=function(data,k){
	n=nrow(data)
	D=dist(data)
	Dm=as.matrix(D)
	dij2=Dm^2
	I=diag(n)
	J=matrix(1,nrow=n,ncol=n)
	V=I-(1/n)*J
	B=(-1/2)*V%*%dij2%*%V
	ne=eigen(B)$value[1:k]
	ve=eigen(B)$vectors[,1:k]
	F=ve%*%(sqrt(diag(ne)))
	Dhat=dist(F)
	S=(sqrt(sum((dist(data)-Dhat)^2))/(sum(dist(data)^2)))*100
	a=list(F,S)
	names(a)=c("Koordinat Terbentuk","Nilai Stress (%)")
	x=ve[,1]
	y=ve[,2]
	plot(x,y,main="Nonmetric MDS",type="p",pch=19,col=rainbow(8))
	abline(v=0)
	abline(h=0)
	text(x,y,labels=row.names(data),cex=0.7)
	return(a)
}
hasil=mds(data,2)
hasil