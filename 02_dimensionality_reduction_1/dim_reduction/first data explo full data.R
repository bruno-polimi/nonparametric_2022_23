#this code must be used together with PCA_final_data.R
#analyse the PCs, plot and outliers
library(MASS)
library(rgl)
library(DepthProc)
library(hexbin)
library(aplpack)
library(robustbase)

tukey_depth=depth(u=data_pca,method='Tukey')
save(tukey_depth,file="tukey_depth.Rdata")

data_pca=data.frame(data_pca)
x11()
bagplot_matrix <- aplpack::bagplot.pairs(data_pca)

f1=which(new_data$TARGET==1)
f0=which(new_data$TARGET==0)

compl_data=c(data_pca,facts)
compl_data=data.frame(compl_data)
compl_data=compl_data[sample(1:213170),]

low_data=compl_data[c(1:5000),]

colors=1:19467
colors[which(new_data$TARGET==1)]="red"
colors[which(new_data$TARGET==0)]="blue"

x11()
pairs(data_pca[f1,1:5],col="red")

x11()
pairs(data_pca[f0,1:5],col="blue")

data_pca=data.frame(data_pca)
colMeans(data_pca[f0,])
colMeans(data_pca[f1,])

x11()
pairs(data_pca[sample(1:5000),1:5],col=colors)