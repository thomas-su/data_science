rm(list=ls()) # to remove all objects from a specified environment
cat("\f") # to clean console

library(readr)
setwd("~/Library/Mobile Documents/com~apple~CloudDocs/Study Files (Graduate)/MATH 6350/Homework/HW02/fonts/")
# we selected COMIC.csv, BOOKMAN.csv, MONOTYPE.csv.
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

CL1 = comic[comic[,match("strength", names(comic))] == 0.4 & 
                    comic[,match("italic", names(comic))] == 0,]
CL2 = bookman[bookman[,match("strength", names(bookman))] == 0.4 & 
                      bookman[,match("italic", names(bookman))] == 0,]
CL3 = monotype[bookman[,match("strength", names(monotype))] == 0.4 & 
                       bookman[,match("italic", names(monotype))] == 0,]
DATA = rbind(CL1, CL2, CL3)
str(DATA)
n1 = nrow(CL1)
n2 = nrow(CL2)
n3 = nrow(CL3)
N = sum(n1, n2, n3)

start_col = match("r0c0",names(DATA))
end_col = ncol(DATA)

# X = function(j) {
#  X = DATA[,start_col: end_col]
#  X = X[, j]
#  return(X)
#}
X = DATA[,start_col: end_col]

# part 0
# m = function(j) {
#   m = mean(X(j))
#   return(m)
# } # define the function of mean
m = sapply(X, mean) # same as apply(X, MARGIN = 2, mean)

# s = function(j) {
#   std = sd(X(j))
#   return(std)
# } # define the function of standard deviation
s = sapply(X, sd)

# SDATA = function(i, j) {
#  SDATA = (DATA[i, (j + start_col - 1)] - m(j))/s(j)
#  return(SDATA)
#} # define the function of standardized value
SDATA = scale(X)

r = cor(X)

row.location = order(abs(r), decreasing = TRUE)[(length(diag(r)) + 1):(length(diag(r)) + 20)]%/%nrow(r) + 1
column.location = order(abs(r), decreasing = TRUE)[(length(diag(r)) + 1):(length(diag(r)) + 20)]%%nrow(r)
column.location[column.location == 0] = nrow(r)
max = 0
for (i in 1:10) {
  max = abs(r)[row.location[2*i], column.location[2*i]]
  print(max)
} # top ten highest absolute correlation values 1 as diag(r)
max_location = c()
for (i in 1:10) {
  max_location = c(row.location[2*i], column.location[2*i])
  print(max_location)
} # (i, j): location of top ten highest absolute values in correlation matrix

# part 1.0
r1 = round(n1 * 0.2) # a set testCL1 of r1 test cases
s1 = n1 - r1 # a set trainCL1 of s1 training cases
r2 = round(n2 * 0.2)
s2 = n2 - r2 
r3 = round(n3 * 0.2)
s3 = n3 - r3
# a remind that start_col = 4 and end_col = 403
trainCL1 = CL1[1:s1,c(1, start_col:end_col)]
trainCL2 = CL2[1:s2,c(1, start_col:end_col)]
trainCL3 = CL3[1:s3,c(1, start_col:end_col)]
testCL1 = CL1[(s1+1):n1,c(1, start_col:end_col)]
testCL2 = CL2[(s2+1):n2,c(1, start_col:end_col)]
testCL3 = CL3[(s3+1):n3,c(1, start_col:end_col)]
TRAINSET_TARGET = rbind(trainCL1, trainCL2, trainCL3)[,1] # true train set 
TESTSET_TARGET = rbind(testCL1, testCL2, testCL3)[,1] # true test set

TRAINSET = rbind(SDATA[1:s1,], SDATA[(n1+1):(n1+s2),], SDATA[(n1+n2+1):(n1+n2+s3),])
TESTSET = rbind(SDATA[(s1+1):n1,], SDATA[(n1+s2+1):(n1+n2),], SDATA[(n1+n2+s3+1):(n1+n2+n3),])

# part 1.1
library(class)
set.seed(1)
train.pred.12 = knn(train = TRAINSET, test = TESTSET, cl = TRAINSET_TARGET, k = 12) # predict training
test.pred.12 = knn(train = TESTSET, test = TRAINSET, cl = TESTSET_TARGET, k = 12) # predict test
train.table.12 = table(TESTSET_TARGET, train.pred.12) 
test.table.12 = table(TRAINSET_TARGET, test.pred.12)
# trainconf.12 = prop.table(train.table.12, margin  = 1) # confusion matrix for training set
# testconf.12 = prop.table(test.table.12, margin  = 1) # confusion matrix for test set
trainperf.12 = sum(diag(train.table.12))/sum(train.table.12) # percentage of correct classifications on TRAINSET
testperf.12 = sum(diag(test.table.12))/sum(test.table.12) # percentage of correct classifications on TESTSET
print(train.table.12)
print(trainperf.12)
print(test.table.12)
print(testperf.12)

# part 1.2
testperf = function(k) {
  set.seed(1)
  test.pred = knn(train = TESTSET, test = TRAINSET, cl = TESTSET_TARGET, k = k) 
  test.table = table(TRAINSET_TARGET, test.pred)
  testperf = sum(diag(test.table))/sum(test.table)
  return(testperf)
}
trainperf = function(k) {
  set.seed(1)
  train.pred = knn(train = TRAINSET, test = TESTSET, cl = TRAINSET_TARGET, k = k) 
  train.table = table(TESTSET_TARGET, train.pred)
  trainperf = sum(diag(train.table))/sum(train.table)
  return(trainperf)
}
k = c(5, 10, 15, 20, 30, 40, 50, 100)
testperf_k = mapply(testperf, k)
trainperf_k = mapply(trainperf, k)
plot(k, testperf_k, type = "b", col = "red", xlab = "k = c(5, 10, 15, 20, 30, 40, 50, 100)", 
     ylab = "", main = "", xlim = c(0, 100), ylim = c(0.4, 0.8))
lines(k, trainperf_k, type = "b", col = "blue")
legend(70, 0.8, c("testperf", "trainperf"), lwd = 2, 
       col = c("red", "blue"), pch = 1, cex = 0.8)

# k1 = seq(1, 1000, 1)
# testperf_k1 = mapply(testperf, k1)
# trainperf_k1 = mapply(trainperf, k1)
# plot(k1, testperf_k1, xlab = "", ylab = "", main = "", type = "l", 
#      col = "red", xlim = c(0, 1000), ylim = c(0.25, 0.8))
# lines(k1, trainperf_k1, type = "l", col = "green")
# legend(60, 0.8, c("testperf", "trainperf"), lwd = 2, 
#       col = c("red", "green"), pch = 1)

k2 = seq(1, 20, 1)
testperf_k2 = mapply(testperf, k2)
trainperf_k2 = mapply(trainperf, k2)
plot(k2, testperf_k2, type = "b", xlab = "k = c(1:20)", 
     ylab = "", main = "", col = "red",xlim = c(0, 20), ylim = c(0.55, 0.85))
lines(k2, trainperf_k2, type = "b", col = "blue")
legend(10, 0.85, c("testperf", "trainperf"), lwd = 2, 
       col = c("red", "blue"), pch = 1)

# part 1.3
kbest = 1
print(kbest)

# part 1.4
set.seed(1)
train.pred.kbest = knn(train = TRAINSET, test = TESTSET, cl = TRAINSET_TARGET, k = kbest) 
test.pred.kbest = knn(train = TESTSET, test = TRAINSET, cl = TESTSET_TARGET, k = kbest) 
train.table.kbest = table(TESTSET_TARGET, train.pred.kbest)
test.table.kbest = table(TRAINSET_TARGET, test.pred.kbest)
trainperf.kbest = sum(diag(train.table.kbest))/sum(train.table.kbest) 
testperf.kbest = sum(diag(test.table.kbest))/sum(test.table.kbest)

trainconf.kbest = prop.table(train.table.kbest, margin  = 1) 
print(trainconf.kbest) # confusion matrix for training set
testconf.kbest = prop.table(test.table.kbest, margin  = 1)
print(testconf.kbest) # confusion matrix for test set

# part 1.5
diag(testconf.kbest)
CI = function(p, CL, n) {
  p + c(-1, 1) * qnorm((1 + CL)/2) * sqrt(p * (1 - p)/n)
}
# assume the confidence level is 90%
CI(diag(testconf.kbest)[1], 0.90, nrow(testCL1))
CI(diag(testconf.kbest)[2], 0.90, nrow(testCL2))
CI(diag(testconf.kbest)[3], 0.90, nrow(testCL3))


# part 1.6
# a reminder that DATA = rbind(CL1, CL2, CL3)
#                 TRAINSET_TARGET = rbind(trainCL1, trainCL2, trainCL3)[,1] # true train set 
#                 TESTSET_TARGET = rbind(testCL1, testCL2, testCL3)[,1] # true test set

a = function(x) {
  start.col = function(x){
    start.col = 20*x-16
    return(start.col)
  }
  sequence = mapply((start.col),c(x)):(mapply(start.col,c(x))+9)
  return(sequence)
}
PACK1 = DATA[,c(a(1),a(2),a(3),a(4),a(5),a(6),a(7),a(8),a(9),a(10))]
SPACK1 = scale(PACK1) # standardized data matrix
TRAINSET.PACK1 = rbind(SPACK1[1:s1,], SPACK1[(n1+1):(n1+s2),], SPACK1[(n1+n2+1):(n1+n2+s3),])
TESTSET.PACK1 = rbind(SPACK1[(s1+1):n1,], SPACK1[(n1+s2+1):(n1+n2),], SPACK1[(n1+n2+s3+1):(n1+n2+n3),])

set.seed(1)
test.pred.kbest1 = knn(train = TESTSET.PACK1, test = TRAINSET.PACK1, cl = TESTSET_TARGET, k = kbest) 
test.table.kbest1 = table(TRAINSET_TARGET, test.pred.kbest1)
print(test.table.kbest1)
w1 = sum(diag(test.table.kbest1))/sum(test.table.kbest1) # percentage of correct classifications by PACK1 on test set
print(w1)

# part 1.7
b = function(x) {
  start.col = function(x) {
    start.col = 20 * x - 6
    return(start.col)
  }
  sequence = mapply((start.col),c(x)):(mapply(start.col,c(x))+9)
  return(sequence)
}
d = function(x) {
  start.col = function(x) {
    start.col = 20 * x + 194
    return(start.col)
  }
  sequence = mapply((start.col),c(x)):(mapply(start.col,c(x))+9)
  return(sequence)
}
e = function(x) {
  start.col = function(x) {
    start.col = 20 * x + 184
    return(start.col)
  }
  sequence = mapply((start.col),c(x)):(mapply(start.col,c(x))+9)
  return(sequence)
}

PACK2 = DATA[,c(b(1),b(2),b(3),b(4),b(5),b(6),b(7),b(8),b(9),b(10))]
PACK3 = DATA[,c(d(1),d(2),d(3),d(4),d(5),d(6),d(7),d(8),d(9),d(10))]
PACK4 = DATA[,c(e(1),e(2),e(3),e(4),e(5),e(6),e(7),e(8),e(9),e(10))]

SPACK2 = scale(PACK2) 
SPACK3 = scale(PACK3)
SPACK4 = scale(PACK4) 

TRAINSET.PACK2 = rbind(SPACK2[1:s1,], SPACK2[(n1+1):(n1+s2),], SPACK2[(n1+n2+1):(n1+n2+s3),])
TRAINSET.PACK3 = rbind(SPACK3[1:s1,], SPACK3[(n1+1):(n1+s2),], SPACK3[(n1+n2+1):(n1+n2+s3),])
TRAINSET.PACK4 = rbind(SPACK4[1:s1,], SPACK4[(n1+1):(n1+s2),], SPACK4[(n1+n2+1):(n1+n2+s3),])

TESTSET.PACK2 = rbind(SPACK2[(s1+1):n1,], SPACK2[(n1+s2+1):(n1+n2),], SPACK2[(n1+n2+s3+1):(n1+n2+n3),])
TESTSET.PACK3 = rbind(SPACK3[(s1+1):n1,], SPACK3[(n1+s2+1):(n1+n2),], SPACK3[(n1+n2+s3+1):(n1+n2+n3),])
TESTSET.PACK4 = rbind(SPACK4[(s1+1):n1,], SPACK4[(n1+s2+1):(n1+n2),], SPACK4[(n1+n2+s3+1):(n1+n2+n3),])

set.seed(1)
test.pred.kbest2 = knn(train = TESTSET.PACK2, test = TRAINSET.PACK2, cl = TESTSET_TARGET, k = kbest) 
test.table.kbest2 = table(TRAINSET_TARGET, test.pred.kbest2)
w2 = sum(diag(test.table.kbest2))/sum(test.table.kbest2) 
print(w2) # percentage of correct classifications by PACK2 on test set

test.pred.kbest3 = knn(train = TESTSET.PACK3, test = TRAINSET.PACK3, cl = TESTSET_TARGET, k = kbest) 
test.table.kbest3 = table(TRAINSET_TARGET, test.pred.kbest3)
w3 = sum(diag(test.table.kbest3))/sum(test.table.kbest3)
print(w3) # percentage of correct classifications by PACK3 on test set

test.pred.kbest4 = knn(train = TESTSET.PACK4, test = TRAINSET.PACK4, cl = TESTSET_TARGET, k = kbest) 
test.table.kbest4 = table(TRAINSET_TARGET, test.pred.kbest4)
w4 = sum(diag(test.table.kbest4))/sum(test.table.kbest4)
print(w4) # percentage of correct classifications by PACK4 on test set


# part 1.8
normalizer = 1/sum(w1,w2,w3,w4)
weight = function(x) {
  x*normalizer
}

W1 = PACK1/sum(PACK1)*weight(w1)
W2 = PACK2/sum(PACK2)*weight(w2)
W3 = PACK3/sum(PACK3)*weight(w3)
W4 = PACK4/sum(PACK4)*weight(w4)

sum(W1,W2,W3,W4)
W = cbind(W1,W2,W3,W4)
TRAINSET.W = rbind(W[1:s1,], W[(n1+1):(n1+s2),], W[(n1+n2+1):(n1+n2+s3),])
TESTSET.W = rbind(W[(s1+1):n1,], W[(n1+s2+1):(n1+n2),], W[(n1+n2+s3+1):(n1+n2+n3),])
set.seed(1)
train.pred.kbest.W = knn(train = TRAINSET.W, test = TESTSET.W, cl = TRAINSET_TARGET, k = kbest) 
test.pred.kbest.W = knn(train = TESTSET.W, test = TRAINSET.W, cl = TESTSET_TARGET, k = kbest) 

train.table.kbest.W = table(TESTSET_TARGET, train.pred.kbest.W)
test.table.kbest.W = table(TRAINSET_TARGET, test.pred.kbest.W)

trainperf.kbest.W = sum(diag(train.table.kbest.W))/sum(train.table.kbest.W) 
testperf.kbest.W = sum(diag(test.table.kbest.W))/sum(test.table.kbest.W)

trainconf.kbest.W = prop.table(train.table.kbest.W, margin  = 1) # confusion matrix for training set
testconf.kbest.W = prop.table(test.table.kbest.W, margin  = 1) # confusion matrix for test set





# d = sqrt(apply((replicate(400,TRAINSET_TARGET) - TRAINSET)^2, MARGIN = 2, sum)) # Euclidean Distance
# order(d ,decreasing = FALSE)[1:12]

# d = sqrt(apply((matrix(replicate(nrow(TRAINSET), t(rbind(trainCL1, trainCL2, trainCL3)[1,])),ncol = 400)-TRAINSET)^2,MARGIN = 1, sum))
# class = order(d ,decreasing = FALSE)[1:12]





