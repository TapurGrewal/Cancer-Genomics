#reading .csv file
m <- read.csv("C:/Users/grewa/Documents/Python/can_gen1.csv", row.names = 1)

#converting to matrix
m1 <-matrix(1:100,ncol=10)
m1

#normalization
sweep(m1, 2, colSums(m1), FUN = "/")
med.att <- apply(m1, 2, median)
sweep(m1, 2, med.att)

#calculating cpm
cpmatrix= m1
for(i in 1:ncol(m1)){
  cpmatrix[,i]=(m1[,i]/sum(m1[,i]))*1000000
}
cpmatrix

#calculating log
l <- log(cpmatrix, base = exp(1))
l

#saving in .rds
saveRDS(l, file = "logc.rds")

#calculating z_scores
z_scores <- (cpmatrix-mean(cpmatrix))/sd(cpmatrix)
z_scores

#Heatmap
heatmap(z_scores)

#To identify genes which are differential in tumor vs control samples
v1 <- cpmatrix[0:10,1:5]
v1
v2 <- cpmatrix[0:10,6:10]
v2
control <- rowMeans(v1)
control
tumor <- rowMeans(v2)
tumor

# Finding the p-value.
p_value=pt(q=-1.549, df=14, lower.tail = TRUE)
p_value

#log2Fc
log2Fc = tumor - control
log2Fc

#merging colomns
mat <- cbind(control, tumor, p_value, log2Fc)
mat



