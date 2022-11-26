library(MASS)
library(car)

#wald F test

permutational_wald=function(X,A,b,response,iterations=1000){
  #X is the design matrix, the response Y is a column of X
  #A is the hypothesis matrix and b the hypothesis vector
  #st H0:A*beta=b vs H1:A*beta!=b
  #I'm taking the F value of the wald test
  vec=1:iterations
  m=lm(X[,c(response)]~X[,-c(response)],data=data.frame(X))
  F0=linearHypothesis(m,A,b)$F[2]
  
  #since A is the hypothesis matrix i need to find what are the columns
  #of X associated to each row of A
  miss=1:dim(A)[1]
  for(i in 1:dim(A)[1])
    miss[i]=which(A[i,]>0)
  
  
  #for some reason I get an error if I compute a lm with X[,-all columns]
  #so I have to divide the cases when I'm taking out all columns or only
  #some of them
  #I'm finding the residuals under H0 of the linear regression
  if(1+length(miss)<dim(X)[2]){
    msimple=lm(X[,c(response)]~X[,-c(response,miss)],data=data.frame(X))
    res=msimple$residuals
    fit=msimple$fitted
  }
  else{
    msimple=lm(X[,c(response)]~1,data=data.frame(X))
    res=msimple$residuals
    fit=msimple$fitted
  }
  
  #I permute the residual of the linear regression summing them on the 
  #predicted Y value, I then fit the complete linear model and compute
  #the wald test
  
  for(i in 1:iterations){
    s=sample(dim(X)[1])
    ynew=fit+res[s]
    
    m=lm(ynew~X[,-c(response)],data=data.frame(X))
    vec[i]=linearHypothesis(m,A,b)$F[2]
    
  }
  
  #return the permutational p value
  return (sum(vec>=F0)/iterations)
}