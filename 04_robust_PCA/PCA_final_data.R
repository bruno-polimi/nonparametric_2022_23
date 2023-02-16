new_data=data_new[,c("AMT_INCOME_TOTAL","AMT_REQ_CREDIT_BUREAU_TOTAL","DAYS_REGISTRATION","DAYS_BIRTH","DAYS_ID_PUBLISH","REGION_POPULATION_RELATIVE","EXT_SOURCE_2",
                     "DAYS_LAST_PHONE_CHANGE","TARGET","OWN_CAR_AGE","OCCUPATION_TYPE")]
library(rospca)
#save(new_data,file="dim_red_FORESTS.Rdata") load this

# We compute the standardized variables
num_data.sd <- scale(new_data[,c(1:8)])
num_data.sd <- data.frame(num_data.sd)

x11()
barplot(rapply(num_data.sd,var)) #ok all equal to 1

#compute the PCA
num_data.sd=as.matrix(num_data.sd)
pc.num_data.sd <- robpca(num_data.sd,ndir=10000)
pc.num_data.sd
summary(pc.num_data.sd)

pc.num_data.sd$eigenvalues
x11()
plot(cumsum(pc.num_data.sd$eigenvalues)/8, type='b', axes=F, xlab='number of components', 
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
par(mfcol = c(5,1))
for(i in 1:5) barplot(load.num_data.sd[,i], ylim = c(-1, 1), main=paste("PC",i))

data_pca=pc.num_data.sd$scores[,c(1:3)]
facts=new_data[,c(9,10,11)]
##############################################################################
#some plots
#first of all target
data_pca=data.frame(data_pca)

f1=which(new_data$TARGET==1)
f0=which(new_data$TARGET==0)


colors=1:19467
colors[which(new_data$TARGET==1)]="red"
colors[which(new_data$TARGET==0)]="blue"

#x11()
#pairs(data_pca[,1:3],col=colors,ptc=19)

x11()
pairs(data_pca[f1,1:3],col="red",xlim=c(-4.5,4.5),ylim=c(-4.5,4.5),main="defaulted")

x11()
pairs(data_pca[f0[1:2000],1:3],col="blue",xlim=c(-4.5,4.5),ylim=c(-4.5,4.5),main="non defaulted")

#second occupation type

coldrivers=1:19467
collaborers=1:19467

coldrivers[which(facts$OCCUPATION_TYPE=="Drivers")]="red"
coldrivers[which(facts$OCCUPATION_TYPE!="Drivers")]="blue"

collaborers[which(facts$OCCUPATION_TYPE=="Laborers")]="purple"
collaborers[which(facts$OCCUPATION_TYPE!="Laborers")]="black"

x11()
pairs(data_pca[f1,1:3],col=coldrivers)

x11()
pairs(data_pca[f1,1:3],col=collaborers)

#third own car age

col30=1:19467
colnan=1:19467

col30[which(facts$OWN_CAR_AGE==">=30")]="red"
col30[which(facts$OWN_CAR_AGE!=">=30")]="blue"

colnan[which(facts$OWN_CAR_AGE=="NaN")]="purple"
colnan[which(facts$OWN_CAR_AGE!="NaN")]="black"

x11()
pairs(data_pca[f1,1:3],col=colnan)

###################################################################
#bagplot for all data
data_pca=data.frame(data_pca)
x11()
bagplot_matrix <- aplpack::bagplot.pairs(data_pca[1:1000,],main="Bagplots")

x11()
bagplot_matrix1 <- aplpack::bagplot.pairs(data_pca[f1[1:1000],],main="Bagplots-dafaulted")

x11()
bagplot_matrix0 <- aplpack::bagplot.pairs(data_pca[f0[1:1000],],main="Bagplots-regular")
###################################################################
#equality in distribution?

data_perm=data_new[,c("AMT_INCOME_TOTAL","AMT_REQ_CREDIT_BUREAU_TOTAL","DAYS_REGISTRATION","DAYS_BIRTH","DAYS_ID_PUBLISH","REGION_POPULATION_RELATIVE","EXT_SOURCE_2",
                      "DAYS_LAST_PHONE_CHANGE")]
# Computing a proper test statistic 
# (i.e., squared distance between the two sample mean vectors)
X=data_perm[f0,]
Y=data_perm[f1,]
x.mean <- colMeans(X)
y.mean <- colMeans(Y)

n1 <- dim(X)[1]
n2 <- dim(Y)[1]
n  <- n1 + n2

T20 <- as.numeric((x.mean-y.mean) %*% (x.mean-y.mean))
T20

# Estimating the permutational distribution under H0
B <- 1000000
T2 <- numeric(B)

for(perm in 1:B){
  # Random permutation of indexes
  # When we apply permutations in a multivariate case, we keep the units together
  # i.e., we only permute the rows of the data matrix
  permutation <- sample(n)
  t_perm <- data_perm[permutation,]
  X_perm <- t_perm[1:n1,]
  Y_perm <- t_perm[(n1+1):n,]
  
  # Evaluation of the test statistic on permuted data
  X.mean_perm <- colMeans(X_perm)
  Y.mean_perm <- colMeans(Y_perm)
  T2[perm]  <- (X.mean_perm-Y.mean_perm) %*% (X.mean_perm-Y.mean_perm) 
}
#save(T2,file="permutation_test_statistics.Rdata")
# plotting the permutational distribution under H0
x11()
hist(T2,xlim=range(c(T2,T20)),breaks=1000)
abline(v=T20,col=3,lwd=4)

x11()
plot(ecdf(T2))
abline(v=T20,col=3,lwd=4)

# p-value
p_val <- sum(T2>=T20)/B
p_val


#################################################################################################
#test NaN-OWN_CAR_AGE
no_car=facts[,c(1,2)]
no_car=as.matrix((no_car))
no_car[which(no_car[,2]!="NaN"),2]="non NaN"

no_car=data.frame(no_car)
no_car$TARGET=as.factor(no_car$TARGET)
no_car$OWN_CAR_AGE=as.factor(no_car$OWN_CAR_AGE)

T0=chisq.test(no_car$TARGET,no_car$OWN_CAR_AGE)$statistic

B=100000

T2=numeric(B)

for(i in 1:B){
  perm=sample(1:19467)
  T2[i]=chisq.test(no_car$TARGET[perm],no_car$OWN_CAR_AGE)$statistic
}

p_value=sum(T2>=T0)/B
p_value

x11()
hist(T2,xlim=range(c(T2,T0)),breaks=100)
abline(v=T0,col=3,lwd=4)

#save(T2,file="test_stat_NA_car.Rdata")
##################################################################################################
#test for NaN-Occupation
no_occ=facts[,c(1,3)]
no_occ=as.matrix(no_occ)
no_occ[which(no_occ[,2]!="NaN"),2]="non NaN"

no_occ=data.frame(no_occ)
no_occ$TARGET=as.factor(no_occ$TARGET)
no_occ$OCCUPATION_TYPE=as.factor(no_occ$OCCUPATION_TYPE)

T0=chisq.test(no_occ$TARGET,no_occ$OCCUPATION_TYPE)$statistic
B=100000

T2=numeric(B)

for(i in 1:B){
  perm=sample(1:19467)
  T2[i]=chisq.test(no_occ$TARGET[perm],no_occ$OCCUPATION_TYPE)$statistic
}

p_value=sum(T2>=T0)/B
p_value

x11()
hist(T2,xlim=range(c(T2,T0)),breaks=100)
abline(v=T0,col=3,lwd=4)

#save(T2,file="test_stat_NA_occ.Rdata")




















