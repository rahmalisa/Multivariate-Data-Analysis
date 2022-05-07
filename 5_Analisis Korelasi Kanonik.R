library(mvnormtest)
library(MVN)

#DATA CONTOH KASUS
data	<- read.csv(file.choose(),header=T,sep=";",dec=",")
data

##MISAL:
y1=data$Uang.Beredar.Sempit
y2=data$Uang.Kuasi
y3=data$Surat.Berharga.selain.Saham
x1=data$Aktiva.Luar.Negeri.Bersih
x2=data$Aktiva.Dalam.Negeri.Bersih
x3=data$Tagihan.Bersih.kepada.Pemerintah.Pusat

data1=data.frame(cbind(y1,y2,y3,x1,x2,x3))

#UJI ASUMSI
##UJI LINEARITAS
model=lm((y1+y2+y3)~(x1+x2+x3),data=data)
summary(model)

##UJI NORMALITAS MULTIVARIAT
mvn(data=data, mvnTest="mardia")

##UJI HOMOGENITAS VARIANS
variabel=as.factor(rep(c("y1","y2","y3","x1","x2","x3"),each=))
index=c(y1,y2,y3,x1,x2,x3)
data2=data.frame(variabel,index)
bartlett.test(index~variabel,data2)

##UJI MULTIKOLINEARITAS
diag(solve(cor(data)))
##Dari hasil di atas, semua variabel menghasilkan nilai VIF yang lebih dari 10
##sehingga dapat disimpulkan bahwa data tersebut teridentifikasi multikolinearitas

install.packages("CCA")
library(CCA)
X	<- as.matrix(cbind(x1,x2,x3)) # set variabel pertama
head(X)
Y	<- as.matrix(cbind(y1,y2,y3)) # set variabel kedua
head(Y)

res.cc=cc(X,Y) # korelasi kanonik
plot(res.cc$cor,type="b") # plot korelasi
plt.cc(res.cc)

kor.kanonik=function(data,p,q,type=c("kovarians","korelasi"),alpha){
if (type=="kovarians"){
##MATRIX KOVARIANS
	cat("KORELASI KANONIK DENGAN MATRIX KOVARIANS\n")
	cat("\n")
	n=nrow(data)
	matkov=cov(data)
	cat("Matrix Kovarians\n")
	print(matkov)
	cat("\n")

	yy=matkov[1:q,1:q]
	yx=matkov[1:q,(q+1):(q+p)]
	xx=matkov[(q+1):(q+p),(q+1):(q+p)]
	xy=matkov[(q+1):(q+p),1:q]
	k=min(p,q)


	ve.yy.u=eigen(yy)$vectors[1:q,1:k]
	cat("Variabel Kanonik U (kolom)\n")
	print(ve.yy.u)
	cat("\n")

	ve.xx.v=eigen(xx)$vectors[1:p,1:k]
	cat("Variabel Kanonik V (kolom)\n")
	print(ve.xx.v)
	cat("\n")


	ro2=solve(yy)%*%yx%*%solve(xx)%*%xy
	eigen(ro2)
	ne.ro2=eigen(ro2)$values

	kor.uv=rep(k,1)
	for (i in 1:k){
		kor.uv[i]=sqrt(ne.ro2[i])
	}
	cat("Korelasi antar Variabel Kanonik\n")
	print(data.frame(Korelasi.UV=kor.uv))
	cat("\n")


	cat("Korelasi antar Variabel Y (baris) dengan Variabel U (kolom)\n")
	ne.yy=eigen(yy)$values
	ve.yy=eigen(yy)$vectors
	output.yu=matrix(nrow=q,ncol=q)
	for (i in 1:q){
	for (j in 1:q){
		output.yu[i,j]=(ve.yy[j,i]*(sqrt(ne.yy[i])))/(sqrt(yy[j,j]))
	}
	}
	print(t(output.yu[1:k,1:q]))
	cat("\n")

	cat("Korelasi antar Variabel X (baris) dengan Variabel V (kolom)\n")
	ne.xx=eigen(xx)$values
	ve.xx=eigen(xx)$vectors
	output.xv=matrix(nrow=p,ncol=p)
	for (i in 1:p){
	for (j in 1:p){
		output.xv[i,j]=(ve.xx[j,i]*(sqrt(ne.xx[i])))/(sqrt(xx[j,j]))
	}
	}
	print(t(output.xv[1:k,1:p]))
	cat("\n")


	cat("UJI SIGNIFIKANSI\n")
	cat("Hipotesis\n")
	cat("H0 : Korelasi Kanonik yang dihasilkan TIDAK Signifikan\n")
	cat("H1 : Korelasi Kanonik yang dihasilkan Signifikan\n")
	cat("Taraf Signifikan (alpha) = ")
	print(alpha)
	cat("\n")
	A=(det(matkov))/(det(yy)*det(xx))
	chi.hitung=-(n-(0.5*(p+q+2)))*log(A)
	chi.tabel=qchisq((1-alpha),p+q)

	cat("UJI Wilks' Lambda\n")
	cat("Hasil Uji:")
	if (chi.hitung>=chi.tabel)"H0 DITOLAK" else"H0 DITERIMA"
	}
else {
##MATRIX KORELASI
	cat("KORELASI KANONIK DENGAN MATRIX KOVARIANS\n")
	cat("\n")
	n=nrow(data)
	matkor=cor(data)
	cat("Matrix Korelasi\n")
	print(matkor)
	cat("\n")

	yy=matkor[1:q,1:q]
	yx=matkor[1:q,(q+1):(q+p)]
	xx=matkor[(q+1):(q+p),(q+1):(q+p)]
	xy=matkor[(q+1):(q+p),1:q]
	k=min(p,q)


	ve.yy.u=eigen(yy)$vectors[1:q,1:k]
	cat("Variabel Kanonik U (kolom)\n")
	print(ve.yy.u)
	cat("\n")

	ve.xx.v=eigen(xx)$vectors[1:p,1:k]
	cat("Variabel Kanonik V (kolom)\n")
	print(ve.xx.v)
	cat("\n")


	ro2=solve(yy)%*%yx%*%solve(xx)%*%xy
	eigen(ro2)
	ne.ro2=eigen(ro2)$values

	kor.uv=rep(k,1)
	for (i in 1:k){
		kor.uv[i]=sqrt(ne.ro2[i])
	}
	cat("Korelasi antar Variabel Kanonik\n")
	print(data.frame(Korelasi.UV=kor.uv))
	cat("\n")


	cat("Korelasi antar Variabel Zy (baris) dengan Variabel U (kolom)\n")
	ne.yy=eigen(yy)$values
	ve.yy=eigen(yy)$vectors
	output.yu=matrix(nrow=q,ncol=q)
	for (i in 1:q){
	for (j in 1:q){
		output.yu[i,j]=(ve.yy[j,i]*(sqrt(ne.yy[i])))/(sqrt(yy[j,j]))
	}
	}
	print(t(output.yu[1:k,1:q]))
	cat("\n")

	cat("Korelasi antar Variabel Zx (baris) dengan Variabel V (kolom)\n")
	ne.xx=eigen(xx)$values
	ve.xx=eigen(xx)$vectors
	output.xv=matrix(nrow=p,ncol=p)
	for (i in 1:p){
	for (j in 1:p){
		output.xv[i,j]=(ve.xx[j,i]*(sqrt(ne.xx[i])))/(sqrt(xx[j,j]))
	}
	}
	print(t(output.xv[1:k,1:p]))
	cat("\n")


	cat("UJI SIGNIFIKANSI\n")
	cat("Hipotesis\n")
	cat("H0 : Korelasi Kanonik yang dihasilkan TIDAK Signifikan\n")
	cat("H1 : Korelasi Kanonik yang dihasilkan Signifikan\n")
	cat("Taraf Signifikan (alpha) = ")
	print(alpha)
	cat("\n")
	A=(det(matkor))/(det(yy)*det(xx))
	chi.hitung=-(n-(0.5*(p+q+2)))*log(A)
	chi.tabel=qchisq((1-alpha),p+q)
	k <- if (chi.hitung>=chi.tabel)"H0 DITOLAK" else"H0 DITERIMA" 
	hasil=data.frame(chi.hitung,chi.tabel,Kesimpulan=k)
	print(hasil)
	}
}

kor.kanonik(data1,3,3,type="kovarians",0.05)

p=3
q=3
yy=matkor[1:q,1:q]
xx=matkor[(q+1):(q+p),(q+1):(q+p)]

matkor=cor(data)
A=(det(matkor))/(det(yy)*det(xx))
chi.hitung=-(n-(0.5*(p+q+2)))*log(A)
chi.tabel=qchisq((1-0.05),p+q)
k <- if (chi.hitung>=chi.tabel)"H0 DITOLAK" else"H0 DITERIMA" 
hasil=data.frame(chi.hitung,chi.tabel,Kesimpulan=k)
print(hasil)
