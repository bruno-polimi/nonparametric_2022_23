#use the fact dataset
#use the numeric_new
#run after association_rules.R
fact1=fact[,c("TARGET","OWN_CAR_AGE","OCCUPATION_TYPE","NAME_FAMILY_STATUS","NAME_TYPE_SUITE",
             "NAME_HOUSING_TYPE","ORGANIZATION_TYPE","NAME_EDUCATION_TYPE",
             "NAME_INCOME_TYPE","FLAG_OWN_REALTY","FLAG_WORK_PHONE",
             "FLAG_PHONE","CODE_GENDER","REGION_RATING_CLIENT_W_CITY","REGION_RATING_CLIENT",
             "FLAG_EMAIL","FLAG_DOCUMENT_3")]

data_new=data_new[,-c(30)] #I no longer remember which dataset it was
data=data.frame(num_data,fact1)
data=data[,-c(12)]
data=data[,-c(8)]
rf <- randomForest(TARGET~., data=data_new,replace=FALSE)

x11()
varImpPlot(rf,
           sort = T,
           n.var = 30,
           main = "Top 30 Variable Importance")