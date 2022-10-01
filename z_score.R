library(ComplexHeatmap)
library(matrixStats)
x=read.csv("C:/Users/grewa/Downloads/GSE208390_gene_count_matrix.csv.gz",sep=",",header=T,row.names = 1)
x

function1=function(x){
  matrixc=x
  m <- as.numeric(unlist(matrixc))
  for (i in 1:ncol(x)) {
    matrixc[,i] = (x[,i]/sum(x[,i]))*1000000
    print(head(matrixc))
    matrixc[,i]= log2(matrixc[,i] +1)
    logfc=log2(matrixc+1)
  }
  data = matrixc
  for (i in 1:ncol(data)){
    z_score = (data - rowMeans(data))/rowSds(as.matrix(data))[row(data)]
    
  }
  z_score[is.na(z_score)]=0
  zsc = as.matrix(z_score)
  
  Heatmap(zsc)
  return(Heatmap(zsc[1:10],))
  
}

function1(x)
pdf('data.pdf',width = 10,height = 10)

plot(1:4,pch=20)

dev.off()
