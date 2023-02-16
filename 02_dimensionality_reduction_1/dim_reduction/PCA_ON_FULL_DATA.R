#use the training dataset
#PCA
data=data[,-c(1)]
data$REGION_RATING_CLIENT=as.factor(data$REGION_RATING_CLIENT)
data$REGION_RATING_CLIENT_W_CITY=as.factor(data$REGION_RATING_CLIENT_W_CITY)
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

#pre-processing
#create only one variables for the last 5
AMT_REQ_CREDIT_BUREAU_TOTAL=0
for(i in 19:24){
  AMT_REQ_CREDIT_BUREAU_TOTAL=AMT_REQ_CREDIT_BUREAU_TOTAL+num_data[,i]
}

num_data=num_data[,-c(19:24)]
num_data$AMT_REQ_CREDIT_BUREAU_TOTAL=AMT_REQ_CREDIT_BUREAU_TOTAL

#days in years/10 and positive
num_data$DAYS_BIRTH=-num_data$DAYS_BIRTH/360
num_data$DAYS_EMPLOYED=-num_data$DAYS_EMPLOYED/360
num_data$DAYS_ID_PUBLISH=-num_data$DAYS_ID_PUBLISH/360
num_data$DAYS_LAST_PHONE_CHANGE=-num_data$DAYS_LAST_PHONE_CHANGE/360
num_data$DAYS_REGISTRATION=-num_data$DAYS_REGISTRATION/360

#scale the amount of money
num_data$AMT_INCOME_TOTAL=log(1+num_data$AMT_INCOME_TOTAL)
num_data$AMT_CREDIT=log(1+num_data$AMT_CREDIT)
num_data$AMT_ANNUITY=log(1+num_data$AMT_ANNUITY)
num_data$AMT_GOODS_PRICE=log(1+num_data$AMT_GOODS_PRICE)


x11()
barplot(rapply(num_data,var))

n_data=num_data[,-c(7:10)]

# We compute the standardized variables
num_data.sd <- scale(n_data)
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


#component1: mean of wealth figures contrasting with region_client,region_clint W city
#component2: mean of OBS and CNT
#component3: region and wealth figures contrasting with region population relative
#component4: mean of number of children and number of family members
#component5: constrast between OBS and DEF
#component6: antimean of the last phone change and credit bureau
#component7: antimean external source and phone change against the credit bureau
#component8: HOUR_APPR_PROCESS_START 
#component9: external source and income total against last phone change

new_data=pc.num_data.sd$scores[,c(1:9)]
new_data=data.frame(new_data)

new_data$YEAR_BIRTH=-num_data$DAYS_BIRTH
new_data$YEAR_EMPLOYED=-num_data$DAYS_EMPLOYED
new_data$YEAR_ID_PUBLISH=-num_data$DAYS_ID_PUBLISH
new_data$YEAR_LAST_PHONE_CHANGE=-num_data$DAYS_LAST_PHONE_CHANGE
new_data$YEAR_REGISTRATION=-num_data$DAYS_REGISTRATION

#save(new_data,file="numeric_new.Rdata")






