dataset=read.csv("C:\\Users\\vital\\Desktop\\nonparam project\\nonparamdataset\\application_data.csv")
dataset=data.frame(dataset)
save(dataset,file="application_data.Rdata")

#MCA and data preparation
percNA=apply(X=is.na(dataset),FUN=sum,MARGIN=2)/307511
t=which(percNA>0)
apply(X=is.na(dataset),FUN=sum,MARGIN=2)
which(is.na(dataset[10]))

d=dataset[which(dataset[10]=="NOAnnuity"),]

dataset[which(is.na(dataset[10])),10]="NOAnnuity"

library("FactoMineR")
which(sapply(dataset,class)=="character")
for(i in 1:122){
  total=0
  for(j in 1:307551){
    if(is.na(dataset[j,i])){
      total=total+1;
    }
    if(!is.na(dataset[j,i])){
      if(dataset[j,i]==0 || dataset[j,i]==1){
      total=total+1;
      }
    }
    if(total==307551){
      dataset[,i]=as.factor(dataset[,i])
    }
  }
  if(class(dataset[,i])=="character"){
    dataset[,i]==as.factor(dataset[,i])
    break
  }
}
sapply(dataset[1],class)
sapply(dataset,class)
for(i in 1:122){
  if(sapply(dataset[i],class)=="character"){
    dataset[i]=as.factor(dataset[i])
  }
}

dataset[,2]=as.factor(dataset[,2])
save(dataset,file="application_data_factor.Rdata")









