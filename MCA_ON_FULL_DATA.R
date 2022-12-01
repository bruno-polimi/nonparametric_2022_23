library(FactoMineR)
#to use this code use the dataset on whatsapp!
data=data[,-c(1)]

vec_fact=1:72
vec_num=1:72
for(i in 1:72){
  if(!is.factor(data[,i])){
    vec_fact[i]=0
  }
  if(is.factor(data[,i])){
    vec_num[i]=0
  }
}

#trying to do MCA in our dataset, I now consider target as a covariate for our dimensionality reduction
#numerical variables are considered quantitative supplementary variables
mc_data=MCA(data,quanti.sup=c(which(vec_num!=0)))
summary(mc_data)
#the dimensions explains very little variance, moreover qualitative variables are very badly represented
#in the two dimensions

plot(mc_data,autoLab = "y",invisible="ind",cex=0.5)




#I don't consider now target for the construction of our dimensions and put it as a qualitative supplementary variable

mc_data_notar=MCA(data,quanti.sup=c(which(vec_num!=0)),quali.sup=c(1))
summary(mc_data_notar)
#there is not much difference but now target is visible, unfortunately variables connected to target
#aren't well represented in the first two principal components

plot(mc_data_notar,autolab="y",invisible="ind",cex=0.5,selectMod = "cos2 30")
