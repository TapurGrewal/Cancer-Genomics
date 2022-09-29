
#Reading csv file
m =read.csv("C:/Users/grewa/Documents/Python/can_gen1.csv",sep=",",header=T,row.names = 1)

#cpm
cpmatrix = m
for(i in 1:ncol(m)){
  cpmatrix[,i]=(m[,i]/sum(m[,i]))*1000000
}
#log of cpm
logcpm=log2(cpmatrix+1)
summary(logcpm)

# z score
z_score = (logcpm - rowMeans(logcpm))/rowSds(as.matrix(logcpm))[row(logcpm)]

#variance using log 
variance = apply(logcpm, 1, var)
variance = sort(variance,decreasing = T)
top50 = variance[1:50]
map = z_score[names(top50),]

#Create a heatmap
library(ComplexHeatmap)
Heatmap(map)

