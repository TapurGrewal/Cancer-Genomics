library(ComplexHeatmap)
library(matrixStats)
x=read.csv("C:/Users/grewa/Downloads/GSE208390_gene_count_matrix.csv.gz",sep=",",header=T,row.names = 1)
x

function1=function(x){
  matrix =x
  m <- as.numeric(unlist(matrix))
  for (i in 1:ncol(x)) {
    matcpm[,i] = (x[,i]/sum(x[,i]))*1000000
    print(head(matcpm))
    matcpm[,i]= log2(matcpm[,i] +1)
    logfc=log2(matcpm+1)
  }
  data = matcpm
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
