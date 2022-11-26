library(Dict)
library(purrr)
#genetic algo
history=Dict$new("initialization"=0)

find_score=function(X,response=1,gene,iterations=1000,alpha=0.05){
  s=gene_to_string(gene)
  if(is.null(history$get(s))==FALSE){
    return(history[s])
  }
  if(sum(gene)==0){
    history[gene_to_string(gene)]=0
    return(0)
  }
  P=permutational_wald(X,gene_to_hyp(gene),replicate(sum(gene),0),response,iterations)
  if(P<=alpha){
    score=0
  }
  else{
    score=sum(gene)
  }
  history[gene_to_string(gene)]=score
  return(score)
}

init_matrix=function(n_ind,n_col){
  mat=matrix(0,n_ind,n_col)
  for(i in 1:n_ind){
    for(j in 1:n_col){
      mat[i,j]=rbinom(1,1,0.5)
    }
  }
  return(mat)
}

gene_to_string=function(vec){
  s=paste(paste(vec),collapse = " ")
  return(s)
}

gene_to_hyp=function(vec){
  s=sum(vec)
  mat=matrix(0,s,length(vec))
  i=1
  for(j in 1:length(vec)){
    if(vec[j]==1){
      mat[i,j]=1
      i=i+1
    }
  }
  return(mat)
}

bubble_sort <- function(x,pop) {
  swap_performed <- TRUE
  while (swap_performed) {
    swap_performed <- FALSE
    for (i in 1:(length(x) - 1)) {
      if (x[i] < x[i + 1]) {
        tmp <- x[i]
        tmp_pop=pop[i,]
        x[i] <- x[i + 1]
        pop[i,]=pop[i+1,]
        x[i + 1] <- tmp
        pop[i+1,]=tmp_pop
        swap_performed <- TRUE
      }
    }
  }
  return(pop)
}

mutate=function(gene,mutation_rate=1/10000){
  for(i in 1:length(gene)){
    r=rbinom(1,1,mutation_rate)
    if(r==1){
      if(gene[i]==0){
        gene[i]=1
      }
      else{
        gene[i]=0
      }
    }
  }
  return(gene)
}

crossover=function(gene1,gene2){
  score1=history[gene_to_string(gene1)]
  score2=history[gene_to_string(gene2)]


  gene=1:length(gene1)
  for(i in 1:length(gene1)){
    r=rbinom(1,1,(score1+1)/(score1+score2+2))
    gene[i]=r*gene1[i]+(1-r)*gene2[i]
  }
  gene=mutate(gene)
  return(gene)
}

natural_selection=function(mat,score,surv){
  mat=bubble_sort(score,mat)
  n_ind=dim(mat)[1]
  for(i in (surv+1):n_ind){
    r=rdunif(2,1,surv)
    mat[i,]=crossover(mat[r[1],],mat[r[2],])
  }
  return(mat)
}


#-----------------------------------------------------------------------------------------------------------------
cov1=rnorm(100,1,3)
cov2=rnorm(100,3,4)
cov3=rnorm(100,10,5)
cov4=rnorm(100,20,3)
epsilon=rnorm(100,0,1)
y=cov2-cov1+epsilon
X=cbind(y,cov1,cov2,cov3,cov4)




mat=init_matrix(30,5)
scores=1:30
for(i in 1:30){
  scores[i]=find_score(X,gene=mat[i,])
}
for(i in 1:10){
  mat=natural_selection(mat,scores,15)
  for(i in 1:30){
    scores[i]=find_score(X,gene=mat[i,])
  }
}

mat
