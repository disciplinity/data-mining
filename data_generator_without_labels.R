rm(list=ls())

# let us generate data
labelsNr<-3
labelsJ<-1:1:labelsNr
labelsI<-1:1:500

sigma1<-matrix(c(4,2,2,3),ncol=2)
sigma2<-matrix(c(2,-2,-2,6),ncol=2)
sigma3<-matrix(c(4,-4,-4,5),ncol=2)
mean1=c(2,0)
mean2=c(1,10)
mean3=c(10,7)
#Sigmas=data.frame(sigma1,sigma2,sigma3)
Sigmas=list(sigma1,sigma2,sigma3)
Means=list(mean1,mean2,mean3)
x<-matrix(,1500,3)
library(mvtnorm)



for (j in seq(along=labelsJ)){
  sigma=Sigmas[[j]]
  mean=Means[[j]]
  xx <- rmvnorm(n=500, mean=mean, sigma=sigma) #multivariate normal distribution
  a<-switch(j,"red","green","blue");
  plot(xx,col=a,type="p",xlim=c(-10,20),ylim=c(-10,20))
  par(new=TRUE)
  for (i in seq(along=labelsI)){
    x[(j-1)*500+i,1:2]=xx[i,]
    x[(j-1)*500+i,3]=j
  }
}

x <- x[,1:2] # the data we used was initially prepared for the classification example please remove third column 

save(x,file="C:/Users/Daiy/Desktop/iti8730-data_mining/kdata.RData")


