library(FactoMineR)
#to use this code use the dataset on whatsapp!
#MCA
#use the "training.Rdata" dataset
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
mc_data=MCA(data[,c(1,28)])
summary(mc_data)

x11()
plot(mc_data,autoLab = "y",invisible="ind",cex=0.5)

#cooking staff,low_skill laborers,security staff,drivers,laborers,sales staff

t1=table(data$OCCUPATION_TYPE)
t2=table(data$OCCUPATION_TYPE[which(data$TARGET==1)])

t2/t1

mc_data=MCA(data[,c(1,40)])
summary(mc_data)

x11()
plot(mc_data,autoLab = "y",invisible="ind",cex=0.5)

help(plot)
#cooking staff,low_skill laborers,security staff,drivers,laborers,sales staff

t1=table(data$ORGANIZATION_TYPE)
t2=table(data$ORGANIZATION_TYPE[which(data$TARGET==1)])

t2/t1



#--------------------------------------------------------------------------------------------------------------

#trying to do MCA in our dataset, I now consider target as a covariate for our dimensionality reduction
#numerical variables are considered quantitative supplementary variables
mc_data=MCA(data,quanti.sup=c(which(vec_num!=0)))
summary(mc_data)

#the dimensions explains very little variance, moreover qualitative variables are very badly represented
#in the two dimensions
x11()
plot(mc_data,autoLab = "y",invisible="ind",cex=0.5)




#I don't consider now target for the construction of our dimensions and put it as a qualitative supplementary variable

mc_data_notar=MCA(data,quanti.sup=c(which(vec_num!=0)),quali.sup=c(1))
summary(mc_data_notar)
#there is not much difference but now target is visible, unfortunately variables connected to target
#aren't well represented in the first two principal components

plot(mc_data_notar,autolab="y",invisible="ind",cex=0.5,selectMod = "cos2 30")

d=data[,c(1,28)]
which(d[,1]==1)
t=table(d[which(d[,1]==1),2])
t/sum(t)

which(t/sum(t)>0.05)
facts[,18]=="Laborers"
OCCUPATION=1:213170

facts=data[,which(vec_num==0)]
for(i in 1:213170){
  if(facts[i,18]=="Core staff"||facts[i,18]=="Drivers"||facts[i,18]=="Laborers"||facts[i,18]=="Managers"||facts[i,18]=="NaN"||facts[i,18]=="Sales staff"){
    OCCUPATION[i]=facts[i,18]
  }
  else{
    OCCUPATION[i]="safe"
  }
}

#4=core staff, 5=drivers, 9=laborers, 11=managers, 13=NaN, 16=sales staff

facts$OCCUPATION_TYPE=OCCUPATION
facts$OCCUPATION_TYPE=as.factor(facts$OCCUPATION_TYPE)

d=data[,c(1,40)]
which(d[,1]==1)
t=table(d[which(d[,1]==1),2])
t/sum(t)


which(t/sum(t)>0.05)
which(facts[,26]=="Business Entity Type 3")
for(i in 1:213170){
  if(facts[i,26]=="Business Entity Type 3"||facts[i,26]=="Other"||facts[i,26]=="Self-employed"||facts[i,26]=="XNA"){
    OCCUPATION[i]=facts[i,26]
  }
  else{
    OCCUPATION[i]="safe"
  }
}

# 6=occupation 3, 34=other, self-employed=43, 58=XNA


facts$ORGANIZATION_TYPE=OCCUPATION
facts$ORGANIZATION_TYPE=as.factor(facts$ORGANIZATION_TYPE)

t1=table(data$OCCUPATION_TYPE)
t2=table(data$OCCUPATION_TYPE[which(data$TARGET==1)])

t2/t1

t1=table(data$ORGANIZATION_TYPE)
t2=table(data$ORGANIZATION_TYPE[which(data$TARGET==1)])

t2/t1