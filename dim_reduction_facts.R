library(randomForest)
library(arules)
library(arulesViz)
library(ggplot2)
vec_fact=1:66
vec_num=1:66
for(i in 1:66){
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
fact=fact[sample(1:nrow(data)), ]

fT2517=fact[,c(1,25,17)]

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
fact[,25]=as.character(fact[,25])
fact[,17]=as.character(fact[,17])

for(i in 1:213170){
  if(fact[i,25]=="XNA" || fact[i,25]=="Business Entity Type 3" || fact[i,25]=="Self-employed"){
  }
  else{
    fact[i,25]="BASE_ORG"
  }
  if(fact[i,17]=="NaN" || fact[i,17]=="Drivers" || fact[i,17]=="Laborers" || fact[i,17]=="Sales staff"){
  }
  else{
    fact[i,17]="BASE_OCC"
  }
}

fact[,25]=as.factor(fact[,25])
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

v1=data.frame(v)
v1=v1/1000

save(v1,file="gini_scores_importance_sampling.Rdata")
v2=t(v1)
x11()
barplot(v2,cex.names = 0.1)






