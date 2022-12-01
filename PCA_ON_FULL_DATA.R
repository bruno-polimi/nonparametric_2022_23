#to use this code use the dataset on whatsapp!
data=data[,-c(1)]

vec=1:72
for(i in 1:72){
  if(is.factor(data[,i])){
    vec[i]=0
  }
}

num_data=data[,c(which(vec!=0))]
library(car)
library(mvtnorm)
#PCA
x11()
barplot(rapply(num_data,var)) #we must use standardized variables
# We compute the standardized variables
num_data.sd <- scale(num_data)
num_data.sd <- data.frame(num_data.sd)

x11()
barplot(rapply(num_data.sd,var)) #ok all equal to 1

#compute the PCA
pc.num_data.sd <- princomp(num_data.sd, scores=T)
pc.num_data.sd
summary(pc.num_data.sd)


x11()
layout(matrix(c(2,3,1,3),2,byrow=T))
plot(pc.num_data.sd, las=2, main='Principal components', ylim=c(0,4))
barplot(sapply(num_data.sd,sd)^2, las=2, main='Original Variables', ylim=c(0,1), ylab='Variances')
plot(cumsum(pc.num_data.sd$sd^2)/sum(pc.num_data.sd$sd^2), type='b', axes=F, xlab='number of components', 
     ylab='contribution to the total variance', ylim=c(0,1))
abline(h=1, col='blue')
abline(h=0.8, lty=2, col='blue')
box()
axis(2,at=0:10/10,labels=0:10/10)
axis(1,at=1:ncol(num_data.sd),labels=1:ncol(num_data.sd),las=2)

#we should consider at least 12 covariates-> better not to use it

# loadings (recall: coefficients of the linear combination of the original 
#           variables that defines each principal component)

load.num_data.sd <- pc.num_data.sd$loadings
load.num_data.sd
# graphical representation of the loadings of the first 12 principal components
x11()
par(mfcol = c(3,2))
for(i in 1:6) barplot(load.num_data.sd[,i], ylim = c(-1, 1), main=paste("PC",i))

x11()
par(mfcol = c(3,2))
for(i in 7:12) barplot(load.num_data.sd[,i], ylim = c(-1, 1), main=paste("PC",i))














