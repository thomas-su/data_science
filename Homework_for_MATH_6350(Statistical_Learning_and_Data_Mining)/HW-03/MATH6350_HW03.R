rm(list=ls()) # to remove all objects from a specified environment
cat("\f") # to clean console

# STEP 0: Data Setup
library(readr)
setwd("~/Library/Mobile Documents/com~apple~CloudDocs/Study Files (Graduate)/MATH 6350/Homework/HW03/fonts/")
# We selected COMIC.csv, BOOKMAN.csv, MONOTYPE.csv.
COMIC <- data.frame(read_csv("COMIC.csv"))
BOOKMAN <- data.frame(read_csv("BOOKMAN.csv"))
MONOTYPE <- data.frame(read_csv("MONOTYPE.csv"))

c.names = names(COMIC) # All three font files shares same categories of columns

c.discard = match(c("fontVariant", "m_label", "orientation", "m_top", "m_left", "originalH", 
                    "originalW", "h", "w"),c.names) # discard 9 columns listed
# na.omit() function is to omit all rows that contain NA values
comic = na.omit(COMIC[,-c.discard]) 
bookman = na.omit(BOOKMAN[,-c.discard]) 
monotype = na.omit(MONOTYPE[,-c.discard])

CL2 = comic[comic[,match("strength", names(comic))] == 0.4 & 
              comic[,match("italic", names(comic))] == 0,]
CL1 = bookman[bookman[,match("strength", names(bookman))] == 0.4 & 
                bookman[,match("italic", names(bookman))] == 0,]
CL3 = monotype[monotype[,match("strength", names(monotype))] == 0.4 & 
                 monotype[,match("italic", names(monotype))] == 0,]
DATA = rbind(CL1, CL2, CL3)
str(DATA)

# STEP 1: Standardization
true_set = DATA[,1]
start_col = match("r0c0",names(DATA))
end_col = ncol(DATA)
X = DATA[,start_col: end_col]
m = sapply(X, mean) # same as apply(X, MARGIN = 2, mean)
s = sapply(X, sd)
SDATA = scale(X)

# STEP 2: Correlation Matrix Computation
R = cor(X)
# sigma = cov(X) # variance-covariance matrix "sigma"
# R=cov2cor(sigma) # either way to find correlation matrix
# all.equal(cor(X), cov2cor(sigma)) 

# STEP 3: Eigenvalues and Eigenvectors
D = eigen(R)$values
# or D = (summary(pca)$sdev)^2
Q = eigen(R)$vectors

#first 6 eigenvalues
D = eigen(R)$values; D[1:6]   
# first 6x6 eigenvectors 
Q = eigen(R)$vectors; Q[1:6,1:6]


# STEP 4: Principal Component Analysis
layout(matrix(c(1:3), 1, 3))

# plot of the eigenvalues vs ordinal component r
plot(1:400, D, ylab = "eigenvalues L_r", xlab = "r", main = "Eigenvalues vs. r")
#pca = princomp(X,cor=TRUE,scores=TRUE)
#summary(pca)
#plot(pca, type = "line")

#The variance explained by each principal component is obtained by squaring and then
#plot of the variance explained vs r
plot(1:400, D[1:400]/sum(D), xlab = "r", ylab = "variance explained", main = "Variance Explained vs. r")

# compute the proportion of variance explained by each principal component and then
# plot of the PVE(r) vs r
plot(1:400, cumsum(D)/sum(D), xlab = "r", ylab = "PVE(r)", main = "Percentage of Variance Explained: PVE(r) vs. r", yaxt = "n")
axis(2, at = seq(0, 1, 0.05))
abline(a = 0.95, b = 0, col = "red")

#transpose of the matrix W^T, ==> principal components Y1(n)...Yp(n) 
#Y1(n) = W11 * X1(n) + W21* X2(n) + … + Wp1 * Xp(n)
t(eigen(R)$vector)[1:6,1:6]


r = order(abs(cumsum(D)/sum(D) - 0.95), decreasing = FALSE)[1]
V = Q[, 1:r]
X = as.matrix(scale(X))
Y = data.frame(true_set, X%*%V)
CL1 = Y[Y[,match("true_set", names(Y))] == "COMIC",]
CL2 = Y[Y[,match("true_set", names(Y))] == "BOOKMAN",]
CL3 = Y[Y[,match("true_set", names(Y))] == "MONOTYPE",]

n1 = nrow(CL1)
n2 = nrow(CL2)
n3 = nrow(CL3)
N = sum(n1, n2, n3)

N == nrow(DATA) # verification

SX = Y[,-1]

r1 = round(n1 * 0.2) # a set testCL1 of r1 test cases
s1 = n1 - r1 # a set trainCL1 of s1 training cases
r2 = round(n2 * 0.2)
s2 = n2 - r2 
r3 = round(n3 * 0.2)
s3 = n3 - r3

trainCL1 = CL1[1:s1,]
trainCL2 = CL2[1:s2,]
trainCL3 = CL3[1:s3,]
testCL1 = CL1[(s1+1):n1,]
testCL2 = CL2[(s2+1):n2,]
testCL3 = CL3[(s3+1):n3,]

TRAINSET_TARGET = rbind(trainCL1, trainCL2, trainCL3)[,1] # true train set 
TESTSET_TARGET = rbind(testCL1, testCL2, testCL3)[,1] # true test set

TRAINSET = rbind(SX[1:s1,], SX[(n1+1):(n1+s2),], SX[(n1+n2+1):(n1+n2+s3),])
TESTSET = rbind(SX[(s1+1):n1,], SX[(n1+s2+1):(n1+n2),], SX[(n1+n2+s3+1):(n1+n2+n3),])

library(class)
test.pred = function(k) {
  set.seed(1)
  test.pred = knn(train = TESTSET, test = TRAINSET, cl = TESTSET_TARGET, k = k) 
  return(test.pred)
}
test.table = function(k) {
  test.pred = test.pred(k)
  test.table = table(TRAINSET_TARGET, test.pred)
  return(test.table)
}
testconf = function(k) {
  test.table = test.table(k)
  testconf = prop.table(test.table, margin = 1)
  return(testconf)
}
testperf = function(k) {
  test.table = test.table(k)
  testperf = sum(diag(test.table))/sum(test.table)
  return(testperf)
}
train.pred = function(k) {
  set.seed(1)
  train.pred = knn(train = TRAINSET, test = TESTSET, cl = TRAINSET_TARGET, k = k) 
  return(train.pred)
}
train.table = function(k) {
  train.pred = train.pred(k)
  train.table = table(TESTSET_TARGET, train.pred)
  return(train.table)
}
trainconf = function(k) {
  train.table = train.table(k)
  trainconf = prop.table(train.table, margin  = 1)
  return(trainconf)
}
trainperf = function(k) {
  train.table = train.table(k)
  trainperf = sum(diag(train.table))/sum(train.table)
  return(trainperf)
}

layout(matrix(c(1:2), 1, 2))
k = seq(1,50,1)
testperf_k = mapply(testperf, k)
trainperf_k = mapply(trainperf, k)
plot(k, testperf_k, type = "b", col = "red", xlab = "k", 
     ylab = "accuracy", main = "k versus accuracy", 
     xlim = c(0, 50),ylim = c(0.5, 0.85))
lines(k, trainperf_k, type = "b", col = "blue")
legend(0, 0.85, c("testperf", "trainperf"), lwd = 2, 
       col = c("red", "blue"), pch = 1, cex = 0.6)

set.seed(1)
plot(k, sapply(k, function(k) {kmeans(SX, k, iter.max = 50)$tot.withinss}),
     type = "b", pch = 19, col = "black", frame = FALSE,
     xlim = c(0, 50), 
     xlab = "k", ylab = "Total Within-Clusters Sum of Squares",
     main = "Elbow Method")


kbest = 9
test.table(kbest)
testconf(kbest)
testperf(kbest)
train.table(kbest)
trainconf(kbest)
trainperf(kbest)

# layout(matrix(c(1:6), 2, 3))
Y = Y[,-1]
# plot(Y[,1], Y[,2], xlab = "Y1", ylab = "Y2")
# plot(Y[,1], Y[,3], xlab = "Y1", ylab = "Y3")
# plot(Y[,1], Y[,4], xlab = "Y1", ylab = "Y4")
# plot(Y[,2], Y[,3], xlab = "Y2", ylab = "Y3")
# plot(Y[,2], Y[,4], xlab = "Y2", ylab = "Y4")
# plot(Y[,3], Y[,4], xlab = "Y3", ylab = "Y4")

layout(matrix(c(1:6), 2, 3))
P1 <- data.frame(x=Y[,1], y=Y[,2], z=true_set)
attach(P1); plot(x, y, xlab = "Y1", ylab = "Y2", col=c("red","blue","green")[z]); detach(P1)
legend("topright", inset=c(-0.02,0), legend = levels(P1$z), col=c("red","blue","green"), pch=1, title="Classes",cex = 0.45)

P2 <- data.frame(x=Y[,1], y=Y[,3], z=true_set)
attach(P2); plot(x, y, xlab = "Y1", ylab = "Y3", col=c("red","blue","green")[z]); detach(P2)
legend("topright", inset=c(-0.02,0), legend = levels(P2$z), col=c("red","blue","green"), pch=1, title="Classes",cex = 0.45)

P3 <- data.frame(x=Y[,1], y=Y[,4], z=true_set)
attach(P3); plot(x, y, xlab = "Y1", ylab = "Y4", col=c("red","blue","green")[z]); detach(P3)
legend("topright", inset=c(-0.02,0), legend = levels(P3$z), col=c("red","blue","green"), pch=1, title="Classes",cex = 0.45)

P4 <- data.frame(x=Y[,2], y=Y[,3], z=true_set)
attach(P4); plot(x, y, xlab = "Y2", ylab = "Y3", col=c("red","blue","green")[z]); detach(P4)
legend("topright", inset=c(-0.02,0), legend = levels(P4$z), col=c("red","blue","green"), pch=1, title="Classes",cex = 0.45)

P5 <- data.frame(x=Y[,2], y=Y[,4], z=true_set)
attach(P5); plot(x, y, xlab = "Y2", ylab = "Y4", col=c("red","blue","green")[z]); detach(P5)
legend("topright", inset=c(-0.02,0), legend = levels(P5$z), col=c("red","blue","green"), pch=1, title="Classes",cex = 0.45)

P6 <- data.frame(x=Y[,3], y=Y[,4], z=true_set)
attach(P6); plot(x, y, xlab = "Y3", ylab = "Y4", col=c("red","blue","green")[z]); detach(P6)
legend("topright", inset=c(-0.02,0), legend = levels(P6$z), col=c("red","blue","green"), pch=1, title="Classes",cex = 0.45)
