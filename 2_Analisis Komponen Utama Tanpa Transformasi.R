#Membaca Data
data	<- read.csv(file.choose(),header=T,sep=";",dec=",")
data
str(data)

#Misal:
A <- data$Rumah.Tangga
B <- data$Komersial
C <- data$Industri
D <- data$Pembangkit.Listrik
E <- data$SPBE.SPBG

#Uji Normalitas Multivariat
library(MVN)
hasil=mvn(data=data,mvnTest="mardia")
hasil

#Uji Homogenitas Varians
variabel=as.factor(rep(c("Rumah.Tangga", "Komersial", "Industri", "Pembangkit.Listrik", "SPBE.SPBG"),each=8))
JenisPelanggan=c(A,B,C,D,E)
data2=data.frame(variabel,JenisPelanggan)
bartlett.test(JenisPelanggan~variabel,data2)

#Matriks Korelasi dan Varkov
a=cor(data)
a
b=cov(data)
b

#Nilai Eigen
eigen(a)
eigen(b)

#ANALISIS KOMPONEN UTAMA
fit_pca=princomp(data,cor=FALSE)
summary(fit_pca)
loadings(fit_pca)

ATAU

PCA=function(x, standardize=FALSE){
  data=as.matrix(x)
  S = cov(data)
  if(standardize == TRUE){
    S = cor(data)
  }
  eigen_val = eigen(S)$values
  eigen_vec = eigen(S)$vector
  
  n = length(eigen_val)
  prop = c()
  for (i in (1:n)){
    prop[i] = (eigen_val[i])/sum(eigen_val)
  }
  
  q = length(prop)
  propcum = c()
  for (i in 1:q){
    propcum[i]=sum(prop[1:i])
  }
  
  p = nrow(eigen_vec)
  corr = matrix(0,p,p)
  for (i in (1:p)){
    for (j in (1:p)){
      corr[i,j] = (eigen_vec[i,j]*sqrt(eigen_val[i]))/S[j,j]
    }
  }
  
  plot(eigen_val, main="Scree Plot", type="o")
  hasil = list("Matriks Varkov/Korelasi"=S, "Eigen Value"=eigen_val, 
               "Eigen Vector"=eigen_vec, "Proporsi Komponen"=prop, 
               "Proporsi Kumulatif Komponen"=propcum, "Matriks Korelasi Y dan X"=corr)
  print(hasil)
}
PCA(data)

((PCA SCORE))
baku=function(X){
  x=as.matrix(X)
  n=nrow(x)
  p=ncol(x)
  a=matrix(0,n,p)
  for (j in (1:p)) {
    for (i in (1:n)) {
      a[i,j]=(x[i,j]-mean(x[,j]))/sd(x[,j])
    }
  }
  print(a)
}
baku(data)