func = function(x){
  m =read.csv("C:/Users/grewa/Documents/Python/can_gen1.csv",sep=",",header=T,row.names = 1)
  x <- as.numeric(unlist(m))
  mat <- as.matrix(x)
  for (i in 1:ncol(mat)) {   
    mat[,i] = (mat[,i]/sum(mat[,i]))*1000000
    print(head(mat))
    mat[,i]= log2(mat[,i] +1)
    logfc=log2(mat+1)
    
  }
  return((Heatmap(mat)[1:10]))
}
data = func(x)
data



