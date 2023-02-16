library(glmnet)
library(caret)


#use training.Rdata
#too many data it could crash
data=data[,-c(1)]
data=na.omit(data)

AMT_REQ_CREDIT_BUREAU_TOTAL=0
for(i in 67:72){
  AMT_REQ_CREDIT_BUREAU_TOTAL=AMT_REQ_CREDIT_BUREAU_TOTAL+data[,i]
}

data=data[,-c(67:72)]
data$AMT_REQ_CREDIT_BUREAU_TOTAL=AMT_REQ_CREDIT_BUREAU_TOTAL
data=data[,-c(22)]
#days in years/10 and positive
data$DAYS_BIRTH=-data$DAYS_BIRTH/360
data$DAYS_EMPLOYED=-data$DAYS_EMPLOYED/360
data$DAYS_ID_PUBLISH=-data$DAYS_ID_PUBLISH/360
data$DAYS_LAST_PHONE_CHANGE=-data$DAYS_LAST_PHONE_CHANGE/360
data$DAYS_REGISTRATION=-data$DAYS_REGISTRATION/360

#scale the amount of money
data$AMT_INCOME_TOTAL=log(1+data$AMT_INCOME_TOTAL)
data$AMT_CREDIT=log(1+data$AMT_CREDIT)
data$AMT_ANNUITY=log(1+data$AMT_ANNUITY)
data$AMT_GOODS_PRICE=log(1+data$AMT_GOODS_PRICE)

myTuneGrid <- expand.grid(alpha = c((0:10)/10), lambda = c((0:20)/2))

fitControl <- trainControl(## 10-fold CV
  method = "CV",
  number = 10,
  allowParallel=TRUE,
  savePredictions="final",
  metric="Kappa")

def_elnet = train(
  TARGET ~ ., data = data,
  method = "glmnet",
  trControl = fitControl,
  tuneGrid = myTuneGrid
)

def_elnet$bestTune

x11()
plot(def_elnet)

help(train)
