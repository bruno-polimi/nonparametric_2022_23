#this followed the analysis with the association rules so run that code first
data=data[,-c(31)]
data[,38]=as.character(data[,38])
data[,27]=as.character(data[,27])

#set base level factors
for(i in 1:213170){
  if(data[i,38]=="XNA" || data[i,38]=="Business Entity Type 3" || data[i,38]=="Self-employed"){
  }
  else{
    data[i,38]="BASE_ORG"
  }
  if(data[i,27]=="NaN" || data[i,27]=="Drivers" || data[i,27]=="Laborers" || data[i,27]=="Sales staff"){
  }
  else{
    data[i,27]="BASE_OCC"
  }
}

data[,38]=as.factor(data[,38])
data[,27]=as.factor(data[,27])

save(data,file="reduced_dataset.Rdata")