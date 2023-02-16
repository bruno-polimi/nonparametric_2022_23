#use data_red
library(ISLR2)
library(car)
library(mgcv)
library(rgl)
library(splines)
library(pbapply)

help(gam)

#build order 3 smoothing splines
out=data.frame(out)
data_red$TARGET=as.factor(data_red$TARGET)
model_gam=gam(TARGET ~ s(EXT_SOURCE_2,bs="cr")+s(DAYS_ID_PUBLISH,bs="cr")+s(DAYS_BIRTH,bs="cr")+s(DAYS_LAST_PHONE_CHANGE,bs="cr")+s(AMT_REQ_CREDIT_BUREAU_TOTAL,bs="cr")+NAME_FAMILY_STATUS+OCCUPATION_TYPE+OWN_CAR_AGE,data=data_red,family = binomial("logit"))
summary(model_gam)

mean_num=colMeans((data_red[,-c(1,7,8,9)]))
grid_source=with(data=data_red,seq(range(EXT_SOURCE_2)[1],range(EXT_SOURCE_2)[2],length.out=1000))
base_factor=c("BASE_OCC","NaN","Single / not married")


probability=1:1000
for (i in 1:1000){
  new_data=data.frame(EXT_SOURCE_2=grid_source[i],DAYS_ID_PUBLISH=mean_num[2],
                       DAYS_BIRTH=mean_num[3],DAYS_LAST_PHONE_CHANGE=mean_num[4],AMT_REQ_CREDIT_BUREAU_TOTAL=mean_num[5],
                       OCCUPATION_TYPE=base_factor[1],OWN_CAR_AGE=base_factor[2],NAME_FAMILY_STATUS=base_factor[3])
  probability[i]=predict(model_gam,new_data,"response")
}
x11()
plot(grid_source,probability,main="external score")

grid_ID=with(data=data_red,seq(range(DAYS_ID_PUBLISH)[1],range(DAYS_ID_PUBLISH)[2],length.out=1000))
probability=1:1000
for (i in 1:1000){
  new_data=data.frame(EXT_SOURCE_2=grid_source[1],DAYS_ID_PUBLISH=grid_ID[i],
                      DAYS_BIRTH=mean_num[3],DAYS_LAST_PHONE_CHANGE=mean_num[4],AMT_REQ_CREDIT_BUREAU_TOTAL=mean_num[5],
                      OCCUPATION_TYPE=base_factor[1],OWN_CAR_AGE=base_factor[2],NAME_FAMILY_STATUS=base_factor[3])
  probability[i]=predict(model_gam,new_data,"response")
}
x11()
plot(grid_source,probability,main="ID")

grid_AGE=with(data=data_red,seq(range(DAYS_BIRTH)[1],range(DAYS_BIRTH)[2],length.out=1000))
probability=1:1000
for (i in 1:1000){
  new_data=data.frame(EXT_SOURCE_2=grid_source[1],DAYS_ID_PUBLISH=grid_ID[2],
                      DAYS_BIRTH=grid_AGE[i],DAYS_LAST_PHONE_CHANGE=mean_num[4],AMT_REQ_CREDIT_BUREAU_TOTAL=mean_num[5],
                      OCCUPATION_TYPE=base_factor[1],OWN_CAR_AGE=base_factor[2],NAME_FAMILY_STATUS=base_factor[3])
  probability[i]=predict(model_gam,new_data,"response")
}
x11()
plot(grid_AGE,probability,main="AGE")

grid_PHONE=with(data=data_red,seq(range(DAYS_LAST_PHONE_CHANGE)[1],range(DAYS_LAST_PHONE_CHANGE)[2],length.out=1000))
probability=1:1000
for (i in 1:1000){
  new_data=data.frame(EXT_SOURCE_2=grid_source[1],DAYS_ID_PUBLISH=grid_ID[2],
                      DAYS_BIRTH=grid_AGE[3],DAYS_LAST_PHONE_CHANGE=grid_PHONE[i],AMT_REQ_CREDIT_BUREAU_TOTAL=mean_num[5],
                      OCCUPATION_TYPE=base_factor[1],OWN_CAR_AGE=base_factor[2],NAME_FAMILY_STATUS=base_factor[3])
  probability[i]=predict(model_gam,new_data,"response")
}
x11()
plot(grid_PHONE,probability,main="PHONE_CHANGE")

grid_BUREAU=with(data=data_red,seq(range(AMT_REQ_CREDIT_BUREAU_TOTAL)[1],range(AMT_REQ_CREDIT_BUREAU_TOTAL)[2],length.out=1000))
probability=1:1000
for (i in 1:1000){
  new_data=data.frame(EXT_SOURCE_2=grid_source[1],DAYS_ID_PUBLISH=grid_ID[2],
                      DAYS_BIRTH=grid_AGE[3],DAYS_LAST_PHONE_CHANGE=grid_PHONE[4],AMT_REQ_CREDIT_BUREAU_TOTAL=grid_BUREAU[i],
                      OCCUPATION_TYPE=base_factor[1],OWN_CAR_AGE=base_factor[2],NAME_FAMILY_STATUS=base_factor[3])
  probability[i]=predict(model_gam,new_data,"response")
}
x11()
plot(grid_BUREAU,probability,main="BUREAU")