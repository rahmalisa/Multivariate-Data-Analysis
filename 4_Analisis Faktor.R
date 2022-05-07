#Membaca Data
data	<- read.csv(file.choose(),header=T,sep=";",dec=",")
head(data)
str(data)

#MISAL
X1=data$X1
X2=data$X2
X3=data$X3
X4=data$X4
X5=data$X5
X6=data$X6
X7=data$X7
X8=data$X8
X9=data$X9
X10=data$X10

#Uji Normalitas Multivariat
library(MVN)
hasil	<- mvn(data=data,mvnTest="mardia")
hasil

#Uji Multikolinieritas
R	<- cor(data)
detR	<- det(R)
VIF	<- diag(solve(R))

#Uji Homogenitas Varians
variabel<-as.factor(rep(c("X1", "X2", "X3", "X3", "X5", 
		"X6", "X7", "X8", "X9", "X10"),each=33))
AspekKehidupan<-c(X1,X2,X3,X4,X5,X6,X7,X8,X9,X10)
data1<-data.frame(variabel,AspekKehidupan)
bartlett.test(AspekKehidupan~variabel,data1)

#ANALISIS FAKTOR
##Manual
R	<- cor(data)
eigen	<- eigen(R)
eigen

#Analisis paralel dan scree plot
library(nFactors)
parallel	<- parallel(subject=33, var=10, cent=.05)
parallel
ns		<- nScree(e$values,parallel$eigen$qevpea)
ns
screeplot	<- plotnScree(ns)

#Menentukan Banyak Faktor
fit	<-factanal(data, factors=2, rotation="varimax")
fit
print(fit, digits=2, cutoff=.3, sort=TRUE)
load	<-fit$loadings
load
