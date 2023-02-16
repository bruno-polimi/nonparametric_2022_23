#to use with training.Rdata
#association rule mining
#preparation of the dataset
library(randomForest)
library(arules)
library(arulesViz)
library(ggplot2)
data$REGION_RATING_CLIENT=as.factor(data$REGION_RATING_CLIENT)
data$REGION_RATING_CLIENT_W_CITY=as.factor(data$REGION_RATING_CLIENT_W_CITY)
data=data[,-c(18)]
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

num=data[,vec_num]
save(num,file="numeric_data.Rdata")

fact=data[,c(which(vec_fact!=0))]
fact=fact[,-c(4,21)]

fT2517=fact[,c(1,26,17)]
#association rules
associa_rules = apriori(data = fT2517, 
                        parameter = list(support=0.05,confidence = 0.05))


# Visualising the results
inspect(sort(associa_rules, by = 'lift'))
plot(associa_rules, method = "graph", 
     measure = "confidence", shading = "lift")


# Visualising the results
inspect(sort(associa_rules, by = 'lift',rhs="TARGET"))

#keep for organization: xna,business entity type 3, self-employed
#keep for occupation: NaN,Drivers,Laborers,Sales staff
fact[,26]=as.character(fact[,26])
fact[,17]=as.character(fact[,17])

for(i in 1:213170){
  if(fact[i,26]=="XNA" || fact[i,26]=="Business Entity Type 3" || fact[i,26]=="Self-employed"){
  }
  else{
    fact[i,26]="BASE_ORG"
  }
  if(fact[i,17]=="NaN" || fact[i,17]=="Drivers" || fact[i,17]=="Laborers" || fact[i,17]=="Sales staff"){
  }
  else{
    fact[i,17]="BASE_OCC"
  }
}

fact[,26]=as.factor(fact[,26])
fact[,17]=as.factor(fact[,17])
fact=fact[,-18]
save(fact,file="facts.Rdata")

#------------------------------------------------------------------------------------------------------------------
i0=which(fact$TARGET==0)
i1=which(fact$TARGET==1)
v=replicate(43,0)

for(i in 1:100){
  i0=i0[sample(1:length(i0))]
  i1=i1[sample(1:length(i1))]
  f1=fact[c(i0[1:40000],i1[1:10000]),]
  rf <- randomForest(TARGET~., data=f1,replace=TRUE)
  v=v+importance(rf)
}
typeof(v)



rf <- randomForest(TARGET~., data=fact,replace=FALSE)
x11()
varImpPlot(rf,
           sort = T,
           n.var = 30,
           main = "Top 30 - Variable Importance")

v1=data.frame(v)
v1=v1/1000

save(v1,file="gini_scores_importance_sampling.Rdata")
v2=t(v1)
x11()
barplot(v2,cex.names = 0.1)

save(fact,file="fact.Rdata")

num_data=num_data[,-c(8)]
data=c(num_data,fact)
data=data.frame(data)



