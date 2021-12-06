rm(list=ls()) # to remove all objects from a specified environment
cat("\f") # to clean console

# Data Setup
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

CL1 = bookman[bookman[,match("strength", names(bookman))] == 0.4 & 
                bookman[,match("italic", names(bookman))] == 0,]
CL2 = comic[comic[,match("strength", names(comic))] == 0.4 & 
              comic[,match("italic", names(comic))] == 0,]
CL3 = monotype[monotype[,match("strength", names(monotype))] == 0.4 & 
                 monotype[,match("italic", names(monotype))] == 0,]
DATA = rbind(CL1, CL2, CL3)
str(DATA)

# Standardization
true_set = DATA[,1]
start_col = match("r0c0",names(DATA))
end_col = ncol(DATA)
X = DATA[,start_col: end_col]
m = sapply(X, mean) # same as apply(X, MARGIN = 2, mean)
s = sapply(X, sd)
SDATA = scale(X)

# Correlation Matrix Computation
R = cor(X)
# sigma = cov(X) # variance-covariance matrix "sigma"
# R=cov2cor(sigma) # either way to find correlation matrix
# all.equal(cor(X), cov2cor(sigma)) 

# Eigenvalues and Eigenvectors
D = eigen(R)$values
# or D = (summary(pca)$sdev)^2
Q = eigen(R)$vectors


p = order(abs(cumsum(D)/sum(D) - 0.95), decreasing = FALSE)[1]
V = Q[, 1:p]
X = as.matrix(scale(X))
Y = data.frame(true_set, X%*%V)
CL2 = Y[Y[,match("true_set", names(Y))] == "COMIC",]
CL1 = Y[Y[,match("true_set", names(Y))] == "BOOKMAN",]
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
  set.seed(123)
  test.pred = knn(train = TESTSET, test = TRAINSET, cl = TESTSET_TARGET, k = k) 
  return(test.pred)
}
test.table = function(k) {
  test.pred = test.pred(k)
  test.table = table(TRAINSET_TARGET, test.pred)
  return(test.table)
}
testperf = function(k) {
  test.table = test.table(k)
  testperf = sum(diag(test.table))/sum(test.table)
  return(testperf)
}
train.pred = function(k) {
  set.seed(123)
  train.pred = knn(train = TRAINSET, test = TESTSET, cl = TRAINSET_TARGET, k = k) 
  return(train.pred)
}
train.table = function(k) {
  train.pred = train.pred(k)
  train.table = table(TESTSET_TARGET, train.pred)
  return(train.table)
}
trainperf = function(k) {
  train.table = train.table(k)
  trainperf = sum(diag(train.table))/sum(train.table)
  return(trainperf)
}
#------------------------------------------------------------------------------------------------------
# Question 1: Elbow Method for choosing the best k
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

# plot of total within-clusters sum of squares
set.seed(123)
#plot(k, sapply(k, function(k) {kmeans(x = SX, centers = k, nstart = 50, iter.max = 50)$tot.withinss}),
#     type = "b", pch = 19, col = "black", frame = FALSE,
#     xlim = c(0, 50), 
#     xlab = "k", ylab = "Total Within-Clusters Sum of Squares",
#     main = "Elbow Method")

k1 = c(1:20)
plot(k1, sapply(k1, function(k) {kmeans(x = SX, centers = k, nstart = 50, iter.max = 50)$tot.withinss}),
     type = "b", pch = 19, col = "black", frame = FALSE,
     xlim = c(0, max(k1)), 
     xlab = "k", ylab = "Total Within-Clusters Sum of Squares",
     main = "Elbow Method")
segments(min(k1), kmeans(x = SX, centers = min(k1), nstart = 50, iter.max = 50)$tot.withinss, 
         3, kmeans(x = SX, centers = 3, nstart = 50, iter.max = 50)$tot.withinss, 
         col = "red")
segments(3, kmeans(x = SX, centers = 3, nstart = 50, iter.max = 50)$tot.withinss, 
         max(k1), kmeans(x = SX, centers = max(k1), nstart = 50, iter.max = 50)$tot.withinss, 
         col = "red")
# or
#wssplot <- function(data, nc = 20, seed = 123) {
#  # nc: number of clustering
#  wss <- (nrow(data) - 1)*sum(apply(data, 2, var))
#  for (i in 2:nc) {
#    set.seed(seed)
#    wss[i] <- sum(kmeans(data, centers=i, nstart = 50, iter.max = 50)$withinss)
#  }
#  plot(1:nc, wss, type="b", 
#       xlab="Number of Clusters",
#       ylab="Total Within-Clusters Sum of Squares")
#}
#wssplot(SX)

#------------------------------------------------------------------------------------------------------
# Question 2: K-Means Clustering
kbest = 3

set.seed(123)
KM = kmeans(SX, kbest, nstart = 50, iter.max = 50)

centers = KM$centers
size = KM$size
cluster = KM$cluster # a vector of nrow(DATA) x 1

c = centers[,1:3] #dimension of kbest x 3
print(c)
library(rgl) 
plot3d(c, xlab = "x", ylab = "y", zlab = "z", col = "red")

t = max(size)

v = SX[which(cluster == which(size == t)),1:3]
colors = c("red", "green", "blue")
colors = colors[as.numeric(as.factor(true_set))][which(cluster == which(size == t))]
plot3d(v, xlab = "x", ylab = "y", zlab = "z", col = colors)
#------------------------------------------------------------------------------------------------------
# Question 3: Decision Tree
# class 1: BOOKMAN; class 2: COMIC; class3: MONOTYPE
s = function(j) {
  s = length(cluster[cluster==j])
  return(s)
}

f = function(c) { #class frequencies
  f_class = s(c)/length(cluster)
  return(f_class)
}

for (i in 1:kbest){ # gini indexes for each cluster
  cat(paste0("gin(CLU_",i,") ="), f(i)*(1-f(i)), "\n")
}

gini = function(kbest = 3) { # Impurity IMP(kbest) for clustering CLU_1,...,CLU_k
  gini = 0
  for (i in 1:kbest){
    gini = gini + f(i)*(1-f(i))
  }
  return(gini)
}
cat("Impurity IMP(Kbest) =", gini(kbest = kbest),"\n")

A = function(m, j) {
  A = length(intersect(which(as.integer(as.factor(true_set))==m),which(cluster==j)))
  return(A)
}

fm = function(j) {
  rbind(A(m = 1, j)/s(j), A(m = 2, j)/s(j), A(m = 3, j)/s(j))
}

FREQ_j = c()
for (k in 1:kbest) {
  FREQ_j = append(FREQ_j, fm(k))
}
FREQ_j = matrix(FREQ_j, nrow = 3)
print(FREQ_j) # frequency: dimension of number of classes * kbest

A_mj = c()
for (m in 1:3) {
  for (j in 1: kbest) {
    A_mj = append(A_mj, A(m, j))
  }
}
A_mj = matrix(A_mj, nrow = 3, byrow = TRUE)
print(A_mj)

TOP = function(j) {
  top = which(A_mj[,j]==max(A(1, j), A(2, j), A(3, j)))
  return(top)
}

#------------------------------------------------------------------------------------------------------
# Question 4: Decision Tree (Continued...)
j = function(n) { # standard output out$cluster of the kmeans function
  j = cluster[n]
  return(j)
}
Pred = function(n) {
  Pred = TOP(j(n))
  return(Pred)
}

pred = c()
for (i in 1:nrow(SX)) {
  pred = append(pred, Pred(i), after = length(pred))
}
pred = replace(pred, which(pred==1), "BOOKMAN")
pred = replace(pred, which(pred==2), "COMIC")
pred = replace(pred, which(pred==3), "MONOTYPE")
table_matrix = table(true_set, predict = pred[1:nrow(SX)])
CONF = prop.table(table_matrix, margin  = 1)
print(CONF)
#------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------
# reference: youtube.com/watch?v=tU3Adlru1Ng


#set.seed(123)
#pd = sample(x = 2, size = nrow(X), replace = TRUE, prob = c(0.8, 0.2)) #partition data
#TRAINSET = data.frame(X[pd==1,])
#TESTSET = data.frame(X[pd==2,])
#Or
#pd = sample(x = nrow(X), size = round(0.8*nrow(X)), replace = FALSE)
#TRAINSET = data.frame(X[pd,])
#TESTSET = data.frame(X[-pd,])

layout(matrix(c(1), 1, 1))

## Decision Tree with party's package
library(party)
tree_original = ctree(as.factor(TRAINSET_TARGET)~., data = TRAINSET)
tree_cut = ctree(as.factor(TRAINSET_TARGET)~., data = TRAINSET, 
                 # to make the tree smaller and less complicated by controlling some parameters
                 # mincriterion is the confidence level
                 # minsplit means the min sample size when the branch will split into two 
                 controls = ctree_control(mincriterion = 0.99, minsplit = 500))
plot(tree_original)
plot(tree_cut)
# Predict
predict(tree_original, TESTSET)

## Decision Tree with rpart's package
library(rpart)
tree_1 = rpart(as.factor(TRAINSET_TARGET)~., TRAINSET, method = "class")
library(rpart.plot)
rpart.plot(tree_1)
# Predict
predict(tree_1, TESTSET) #probability of all in test dataset

# Misclassification error for train data
tab1 = table(TRAINSET_TARGET, trainPred = predict(tree_original))
print(tab1)
1-sum(diag(tab1))/sum(tab1)

# Misclassification error with test data
testPred = predict(tree_original, newdata = TESTSET)
tab2 = table(TESTSET_TARGET, testPred)
print(tab2)
1-sum(diag(tab2))/sum(tab2)



