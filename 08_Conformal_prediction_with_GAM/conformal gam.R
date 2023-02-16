library(ISLR2)
library(car)
library(mgcv)
library(rgl)
library(splines)
library(pbapply)

help(gam)
#use data_red
#build order 3 smoothing splines
set.seed(2346578)

samp=sample(1:17800)
split_training=data_red[samp[1:13350],]
val=data_red[samp[13351:17800],]

data_red$TARGET=as.factor(data_red$TARGET)
model_gam=gam(TARGET ~ s(EXT_SOURCE_2,bs="cr")+s(DAYS_ID_PUBLISH,bs="cr")+s(DAYS_BIRTH,bs="cr")+s(DAYS_LAST_PHONE_CHANGE,bs="cr")+NAME_FAMILY_STATUS+OCCUPATION_TYPE+OWN_CAR_AGE,data=split_training,family = binomial("logit"))
summary(model_gam)

n=dim(val)[1]
non_conformity=1:n
alpha=0.05

for (i in 1:n){
  if(val$TARGET[i]=="0"){
    non_conformity[i]=predict(model_gam,newdata=val[i,],"response")
  }
  if(val$TARGET[i]=="1"){
    non_conformity[i]=1-predict(model_gam,newdata=val[i,],"response")
  }
}

nc=0
p_vals=matrix(0,n,2)
for (i in 1:n){
    for(j in 1:2){
      if(j==1){ #assign label 0->1-P(0)=P(1)
        p_vals[i,j]=predict(model_gam,newdata=val[i,],"response")[1]
      }
      else{ #assign label 1
        p_vals[i,j]=1-predict(model_gam,newdata=val[i,],"response")
      }
      s=(sum(non_conformity[-i]>p_vals[i,j])+1)/n
      p_vals[i,j]=s
  }
} 

lab=replicate(n,0)
for(i in 1:n){
  if(p_vals[i,1]>=0.05){
    lab[i]="0"
  }
  if(p_vals[i,2]>=0.05){
    lab[i]="1"
  }
  if(p_vals[i,1]>=0.05 && p_vals[i,2]>=0.05){
    lab[i]="0 and 1"
  }
}
#nb this notation is not the best since we have 3 groups and not two

accuracy=(length(which(lab[which(val$TARGET=="1")]=="1"))+length(which(lab[which(val$TARGET=="0")]=="0"))+length(which(lab=="0 and 1")))/4450
precision=(length(which(lab[which(val$TARGET=="1")]=="1"))+(length(which(lab[which(val$TARGET=="1")]=="0 and 1"))))/
  (length(which(lab[which(val$TARGET=="0")]=="1"))+length(which(lab[which(val$TARGET=="1")]=="1"))+length(which(lab[which(val$TARGET=="1")]=="0 and 1")))

recall=(length(which(lab[which(val$TARGET=="1")]=="1"))+(length(which(lab[which(val$TARGET=="1")]=="0 and 1"))))/
  (length(which(lab[which(val$TARGET=="1")]=="0"))+length(which(lab[which(val$TARGET=="1")]=="1"))+length(which(lab[which(val$TARGET=="1")]=="0 and 1")))

length(which(lab[which(val$TARGET=="1")]=="0 and 1"))/length(which(lab=="0 and 1"))
length(which(lab=="1"))
(length(which(lab[which(val$TARGET=="0")]=="0 and 1"))+length(which(lab[which(val$TARGET=="0")]=="0")))/length(which(val$TARGET=="0"))

(length(which(lab[which(val$TARGET=="1")]=="0"))+length(which(lab[which(val$TARGET=="1")]=="0 and 1")))/length(which(val$TARGET=="1"))

#f1=2*precision*recall/(precision+recall)
#accuracy
#precision
#recall
#f1








