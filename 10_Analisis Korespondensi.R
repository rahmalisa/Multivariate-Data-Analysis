###INPUT DATA
data	<-read.csv(file.choose(),header=TRUE,sep=";",dec=",")
data
attach(data)

##TABEL KONTINGENSI
tabel <-xtabs(Jumlah~Jenjang+Akreditasi,data=data)
tabel 

##PROFIL KOLOM
prop.table(tabel,1)

##PROFIL BARIS
prop.table(tabel,2)

##ANALISIS KORESPONDENSI
library(ca)
library(FactoMineR)
library(factoextra)
chisq		<-chisq.test(data)
chisq
res.ca	<-CA(data, graph = FALSE)
summary(res.ca)
chi2		<-4629.974
df		<-(nrow(data) - 1) * (ncol(data) - 1)
pval		<-pchisq(chi2, df = df, lower.tail = FALSE)
pval
eig.val	<-get_eigenvalue(res.ca)
eig.val
sqrt(eig.val)

#PLOT
fit		<-ca(tabel)
plot(fit)



