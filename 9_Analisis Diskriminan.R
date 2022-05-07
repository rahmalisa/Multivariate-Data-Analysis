###INPUT DATA
data<-read.csv(file.choose(),header=TRUE,sep=";",dec=",")
head(data)
str(data)
summary(data)

install.packages("MASS")
library(MASS)

###DESKRIPSI DATA
grup1.v<-c(mean(data$Vanadium[1:11]),sd(data$Vanadium[1:11]))
grup2.v<-c(mean(data$Vanadium[12:22]),sd(data$Vanadium[12:22]))
sdes.v<-data.frame(grup1.v,grup2.v)
row.names(sdes.v)<-c("Rata-rata","Simpangan Baku")
sdes.v

grup1.i<-c(mean(data$Iron[1:11]),sd(data$Iron[1:11]))
grup2.i<-c(mean(data$Iron[12:22]),sd(data$Iron[12:22]))
sdes.i<-data.frame(grup1.i,grup2.i)
row.names(sdes.i)<-c("Rata-rata","Simpangan Baku")
sdes.i

grup1.b<-c(mean(data$Beryllium[1:11]),sd(data$Beryllium[1:11]))
grup2.b<-c(mean(data$Beryllium[12:22]),sd(data$Beryllium[12:22]))
sdes.b<-data.frame(grup1.b,grup2.b)
row.names(sdes.b)<-c("Rata-rata","Simpangan Baku")
sdes.b

grup1.s<-c(mean(data$Saturate.Hydrocarbon[1:11]),sd(data$Saturate.Hydrocarbon[1:11]))
grup2.s<-c(mean(data$Saturate.Hydrocarbon[12:22]),sd(data$Saturate.Hydrocarbon[12:22]))
sdes.s<-data.frame(grup1.s,grup2.s)
row.names(sdes.s)<-c("Rata-rata","Simpangan Baku")
sdes.s

grup1.a<-c(mean(data$Aromatic.Hydrocarbon[1:11]),sd(data$Aromatic.Hydrocarbon[1:11]))
grup2.a<-c(mean(data$Aromatic.Hydrocarbon[12:22]),sd(data$Aromatic.Hydrocarbon[12:22]))
sdes.a<-data.frame(grup1.a,grup2.a)
row.names(sdes.a)<-c("Rata-rata","Simpangan Baku")
sdes.a

###PENGUJIAN ASUMSI
##UJI NORMALITAS##
#QQ PLOT MAHALANOBIS
data2<-data.frame(data$Vanadium,data$Iron,data$Beryllium,data$Saturate.Hydrocarbon,data$Aromatic.Hydrocarbon)
y<-as.matrix(data2)
z<-t(y)
mu<-colMeans(y)
n<-nrow(y)
p<-ncol(y)
cov<-cov(y)
d<-mahalanobis(y,mu,cov)
j<-qchisq(ppoints(n),df=p)
qqplot(j,d,main="QQ-Plot Data Populasi Batu Pasir",ylab="Jarak Mahalanobis")
abline(0,1)

#MARDIA
library(mvnormtest)
mshapiro.test(z)

##UJI INDEPENDENSI MATRIKS VARKOV DARI X
library(biotools)
ujihomogenitas<-boxM(data=data[,1:5], group=data[,6])
ujihomogenitas

##NON-MULTIKOLINIERITAS
r=cor(y)
diag(solve(r))

###ANALISIS DISKRIMINAN
##MATRIKS VARKOV
a=data.frame(data$Vanadium[1:11],data$Iron[1:11],data$Beryllium[1:11],data$Saturate.Hydrocarbon[1:11],data$Aromatic.Hydrocarbon[1:11])
b=data.frame(data$Vanadium[12:22],data$Iron[12:22],data$Beryllium[12:22],data$Saturate.Hydrocarbon[12:22],data$Aromatic.Hydrocarbon[12:22])
cov(a)
cov(b)
sgab=((10*cov(a))+(10*cov(b)))/20
sgab
solve(sgab)

##DISKRIMINAN FISHER
#PEMBAGIAN DATA SESUAI SET KATEGORI
q1=data$Vanadium[1:11]
q2=data$Iron[1:11]
q3=data$Beryllium[1:11]
q4=data$Saturate.Hydrocarbon[1:11]
q5=data$Aromatic.Hydrocarbon[1:11]
x11=matrix(q1)
x12=matrix(q2)
x13=matrix(q3)
x14=matrix(q4)
x15=matrix(q5)
xa=matrix(cbind(x11,x12,x13,x14,x15),nrow=11,ncol=5)

q21=data$Vanadium[12:22]
q22=data$Iron[12:22]
q23=data$Beryllium[12:22]
q24=data$Saturate.Hydrocarbon[12:22]
q25=data$Aromatic.Hydrocarbon[12:22]
x21=matrix(q21)
x22=matrix(q22)
x23=matrix(q23)
x24=matrix(q24)
x25=matrix(q25)
xb=as.matrix(cbind(x21,x22,x23,x24,x25),nrow=11,ncol=5)

#RATA-RATA SETIAP KATEGORI DALAM MATRIKS
x1_=data.frame(mean(x11),mean(x12),mean(x13),mean(x14),mean(x15))
x1=as.matrix(t(x1_))
x1

x2_=data.frame(mean(x21),mean(x22),mean(x23),mean(x24),mean(x25))
x2=as.matrix(t(x2_))
x2

#KOEFISIEN FUNGSI DISKRIMINAN
p=t(x1-x2)%*%solve(sgab)
p

#MENENTUKAN MID POINT
mp=0.5*t(x1-x2)%*%solve(sgab)%*%(x1+x2)
mp