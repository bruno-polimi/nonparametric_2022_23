data=read.csv("C:\\Users\\vital\\Desktop\\nonparam project\\dataset\\trainingset.csv")
#create one hot dataset for cathegorical variables, use the csv dataset
library(mltools)
library(data.table)

for(i in 1:73){
  if(typeof(data[,i])=="character"){
    data[,i]=as.factor(data[,i])
  }
}

data=data[,-c(1)]
newtrain <- one_hot(as.data.table(data))
save(newtrain,file="one_hot_train.Rdata")

newtrain=as.matrix(newtrain)
newtrain=data.frame(newtrain)
for(i in 2:191){
  for(j in 1:213170){
    if(newtrain[j,i]!=1 && newtrain[j,i]!=0){
      break
    }
    if(j==213170){
      newtrain[,i]=as.factor(newtrain[,i])
    }
  }
}
save(newtrain,file="one_hot_train_factor.Rdata")
