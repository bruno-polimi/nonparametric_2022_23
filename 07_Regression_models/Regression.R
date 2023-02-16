# stepwise selection
library( car )
library( ellipse )
library( faraway )
library( leaps )
library(MASS)
library( GGally)
library(rgl)
library(corrplot)
library(ISLR2)
library(mgcv)
library(splines)
library(pbapply)
library(npreg)
library(cvms)
library(ggplot2)
library(caret) 
library(dplyr)
library(tidyr)
library(robustbase)
library(psych)
library(here)
library(DescTools)
library(knitr)
library(RobStatTM)


####preparazione dataset####
load("C:/Users/franc/OneDrive - Politecnico di Milano/Politecnico/Nonparametric statistics/Progetto/revolving_data.Rdata")
load("C:/Users/franc/OneDrive - Politecnico di Milano/Politecnico/Nonparametric statistics/Progetto/outliers.Rdata")

outliers=data.frame(data_new,out)
outliers=outliers[outliers$out == 'TRUE', ]
outliers=outliers[,c("TARGET","EXT_SOURCE_2","DAYS_ID_PUBLISH","DAYS_BIRTH","DAYS_REGISTRATION","DAYS_LAST_PHONE_CHANGE","REGION_POPULATION_RELATIVE","AMT_INCOME_TOTAL","AMT_REQ_CREDIT_BUREAU_TOTAL","OCCUPATION_TYPE","OWN_CAR_AGE","AMT_GOODS_PRICE","AMT_ANNUITY","AMT_CREDIT","OBS_30_CNT_SOCIAL_CIRCLE","OBS_60_CNT_SOCIAL_CIRCLE","NAME_FAMILY_STATUS","CNT_FAM_MEMBERS")]
outliers$TARGET <- as.numeric(as.character(outliers$TARGET))

table(outliers$TARGET)/length(outliers$TARGET)
n=ceiling(length(outliers$TARGET)*0.7)
trainingset=outliers[1:n,]
testset=outliers[n:length(outliers$TARGET),]
table(trainingset$TARGET)/length(trainingset$TARGET)
table(testset$TARGET)/length(testset$TARGET)

#######gsm model#########

model_gsm=gsm(TARGET ~ .,data=trainingset,family="binomial")
#step-wise selection
model_gsm=gsm(TARGET ~ EXT_SOURCE_2 +DAYS_ID_PUBLISH+DAYS_REGISTRATION+DAYS_LAST_PHONE_CHANGE+AMT_REQ_CREDIT_BUREAU_TOTAL+AMT_CREDIT+NAME_FAMILY_STATUS+OCCUPATION_TYPE+OWN_CAR_AGE,data=trainingset,family="binomial")
summary( model_gsm )

predictedval <- predict(model_gsm,newdata=testset,type='response')

Prediction <- ifelse(predictedval > 0.1,1,0)
Target <- testset$TARGET
cm<-table(Prediction,Target)
plt <- as.data.frame(cm)
ggplot(data=plt, mapping = aes(x = Prediction,
                               y = Target)) +
  geom_tile(aes(fill = Freq)) +
  geom_text(aes(label = sprintf("%1.0f", Freq)), vjust = 1) +
  scale_fill_gradient(low = "white",
                      high = "deepskyblue3",
                      trans = "log")+
  
  accuracy <- sum(cm[1], cm[4]) / sum(cm[1:4])
#0.85040
precision <- cm[4] / sum(cm[4], cm[2])
#0.13384
sensitivity <- cm[4] / sum(cm[4], cm[3])
#0.26935
fscore <- (2 * (sensitivity * precision))/(sensitivity + precision)
#0.17883
specificity <- cm[1] / sum(cm[1], cm[2])
#0.88780


library(pROC)
roc_score = roc(Target, Prediction)  # AUC score
plot(roc_score, main="ROC curve -- Logistic Regression ")


######gam model########


model_gam=gam(TARGET ~ s(EXT_SOURCE_2,bs="cr")+s(DAYS_ID_PUBLISH,bs="cr")+s(DAYS_BIRTH,bs="cr")+s(DAYS_REGISTRATION,bs="cr")+s(DAYS_LAST_PHONE_CHANGE,bs="cr")+s(REGION_POPULATION_RELATIVE,bs="cr")+s(AMT_INCOME_TOTAL,bs="cr")+s(AMT_REQ_CREDIT_BUREAU_TOTAL,bs="cr")+s(AMT_GOODS_PRICE,bs="cr")+s(AMT_ANNUITY,bs="cr")+s(AMT_CREDIT,bs="cr")+s(OBS_30_CNT_SOCIAL_CIRCLE,bs="cr")+s(OBS_60_CNT_SOCIAL_CIRCLE,bs="cr")+s(CNT_FAM_MEMBERS,bs="cr",k=5)+NAME_FAMILY_STATUS+OCCUPATION_TYPE+OWN_CAR_AGE,data=trainingset,family="binomial")
summary(model_gam)
#stepwise selection
model_gam=gam(TARGET ~ s(EXT_SOURCE_2,bs="cr")+s(DAYS_ID_PUBLISH,bs="cr")+s(DAYS_REGISTRATION,bs="cr")+s(DAYS_LAST_PHONE_CHANGE,bs="cr")+s(AMT_ANNUITY,bs="cr")+s(CNT_FAM_MEMBERS,bs="cr",k=5)+NAME_FAMILY_STATUS+OCCUPATION_TYPE+OWN_CAR_AGE,data=trainingset,family="binomial")
summary(model_gam)
#final model
model_gamale=gam(TARGET ~ s(EXT_SOURCE_2,bs="cr")+s(DAYS_ID_PUBLISH,bs="cr")+s(DAYS_BIRTH,bs="cr")+s(DAYS_LAST_PHONE_CHANGE,bs="cr")+NAME_FAMILY_STATUS+OCCUPATION_TYPE+OWN_CAR_AGE,data=trainingset,family="binomial")

predictedval <- predict(model_gamale,newdata=testset,type='response')

Prediction <- ifelse(predictedval > 0.1,1,0)
Target <- testset$TARGET


cm<-table(Prediction,Target)
plt <- as.data.frame(cm)
ggplot(data=plt, mapping = aes(x = Prediction,
                               y = Target)) +
  geom_tile(aes(fill = Freq)) +
  geom_text(aes(label = sprintf("%1.0f", Freq)), vjust = 1) +
  scale_fill_gradient(low = "white",
                      high = "deepskyblue3",
                      trans = "log")
accuracy <- sum(cm[1], cm[4]) / sum(cm[1:4])
#0.8560
precision <- cm[4] / sum(cm[4], cm[2])
#0.1426
sensitivity <- cm[4] / sum(cm[4], cm[3])
#0.27554
fscore <- (2 * (sensitivity * precision))/(sensitivity + precision)
#0.18796
specificity <- cm[1] / sum(cm[1], cm[2])
#0.89338

summary(model_gam)
hist(model_gam$residuals)
qqnorm(model_gam$residuals)
plot(model_gam)

roc_score = roc(Target, Prediction)  # AUC score
plot(roc_score, main="ROC curve -- Logistic Regression ")

model_gam$aic

######glm model#########
model_glm <- glm(TARGET ~ .,data=trainingset,family=binomial)  # build the model

summary(model_glm)
predictedval <- predict(model_glm,newdata=testset,type='response')

Prediction <- ifelse(predictedval > 0.1,1,0)
Target <- testset$TARGET

#confusionMatrix(Prediction,Target,dnn=c("Prediction,Reference"))

cm<-table(Prediction,Target)
plt <- as.data.frame(cm)
ggplot(data=plt, mapping = aes(x = Prediction,
                               y = Target)) +
  geom_tile(aes(fill = Freq)) +
  geom_text(aes(label = sprintf("%1.0f", Freq)), vjust = 1) +
  scale_fill_gradient(low = "white",
                      high = "chartreuse3",
                      trans = "log")
accuracy <- sum(cm[1], cm[4]) / sum(cm[1:4])
#0.8463
precision <- cm[4] / sum(cm[4], cm[2])
#0.1349
sensitivity <- cm[4] / sum(cm[4], cm[3])
#0.0.28483
fscore <- (2 * (sensitivity * precision))/(sensitivity + precision)
#0.0.18308
specificity <- cm[1] / sum(cm[1], cm[2])
#0.882423
summary(model_glm)

AIC(model_glm)
BIC(model_glm)


##############bestthreshold####

currMod<-model_gam
predictedval <- predict(currMod,newdata=testset,type='response')

Threshold=c()
Accuracy=c()
Precision=c()
Sensitivity=c()
Fscore=c()
Specificity=c()
counter=1

for(tresh in seq(0.05,0.5,by = 0.05)){
  Prediction <- ifelse(predictedval > tresh,1,0)
  Target <- testset$TARGET
  
  #confusionMatrix(Prediction,Target,dnn=c("Prediction,Reference"))
  
  cm<-table(Prediction,Target)
  
  Threshold[counter]=tresh
  Accuracy[counter] <- sum(cm[1], cm[4]) / sum(cm[1:4])
  Precision[counter] <- cm[4] / sum(cm[4], cm[2])
  Sensitivity[counter] <- cm[4] / sum(cm[4], cm[3])
  Fscore[counter] <- (2 * (Sensitivity[counter] * Precision[counter]))/(Sensitivity[counter] + Precision[counter])
  Specificity[counter] <- cm[1] / sum(cm[1], cm[2])
  counter=counter+1
}

df= data.frame()
df=data.frame(Threshold,Accuracy,Precision,Sensitivity,Fscore,Specificity)

library(data.table)
df.m= data.frame()
df.m <- melt(df, id.vars="Threshold") # melt the dataframe using topics as id

colnames(df.m)[2]  <- "Variables"
colnames(df.m)[3]  <- "Value"
# plot the lines using ggplot and geom_line
x11()
ggplot(data = df.m,
       aes(x = Threshold, y = Value, group = Variables, color = Variables)) +
  geom_line(size = 0.5)+
  geom_point(size=2)+
  theme_minimal()+
  scale_x_continuous(breaks = seq(0, 0.5, by = 0.1))
