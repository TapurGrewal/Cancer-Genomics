cancer <- read.csv("C:/Users/grewa/Documents/Python/can_gen1.csv", row.names = 1)
m1 <- matrix(cancer, ncol = 20)
m1
sweep(m1, 2, colSums(m1), FUN = "/")
med.att <- apply(m1, 2, median)
sweep(m1, 2, med.att)
c1 <- colSums(m1)
scale(m1, center = FALSE, scale = c1)
cpm(c1)
log(c1, base = exp(1))
z_scores <- (c1-mean(c1))/sd(c1)
z_scores
