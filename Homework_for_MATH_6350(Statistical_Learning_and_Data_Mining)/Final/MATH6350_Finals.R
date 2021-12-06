rm(list = ls())
cat("\f")

options(digits = 6)

setwd("~/library/Mobile Documents/com~apple~CloudDocs/Study Files (Graduate)/MATH 6350/MATH6350_Midterm/Dataset")
BA = data.frame(read.csv("BA_02.csv", na.strings = ""))
c.discard = match(c("t"), names(BA)) 
BA = na.omit(BA[, -c.discard])
str(BA)
start_col = match("Open_t", names(BA))
end_col = match("Adj_Return_t.5", names(BA))

#-----------------
#Q1
t1_start = proc.time()

# PCA
X = BA[, start_col:end_col]
SDATA = scale(X)
#pca = princomp(X,cor=TRUE,scores=TRUE)
#summary(pca)
## we can conclude that 95.211% of the variance will be accounted for with first nine principle components
#plot(pca, type = "line")

sigma = cov(SDATA) # variance-covariance matrix "sigma"
R=cov2cor(sigma)
D = eigen(R)$values
# or D = (summary(pca)$sdev)^2
Q = eigen(R)$vectors
p = order(abs(cumsum(D)/sum(D) - 0.95), decreasing = FALSE)[1]
V = Q[, 1:p]

layout(matrix(c(1:2), nrow = 1, ncol = 2))
# The variance explained by each principal component is obtained by squaring and then
# plot of the variance explained vs r
plot(1:length(D), D[1:length(D)]/sum(D), panel.first = grid(), type = "o", col = "black", 
     xlab = "r", ylab = "variance explained", main = "Variance Explained vs. r")
text(x = (D[1:length(D)]/sum(D))[1:p], labels = signif((D[1:length(D)]/sum(D))[1:p], 3), 
     cex = 0.4, pos = rep(c(3, 1), times = ceiling(p))[1:p], font = 2, col = "blue")

# compute the proportion of variance explained by each principal component and then
# plot of the PVE(r) vs r
plot(1:length(D), cumsum(D)/sum(D), panel.first = grid(), type = "o", col = "black",
     xlab = "r", ylab = "PVE(r)", main = "Percentage of Variance Explained:\nPVE(r) vs. r", yaxt = "n")
text(x = (cumsum(D)/sum(D))[1:p], labels = signif((cumsum(D)/sum(D))[1:p], 3), 
     cex = 0.5, pos = 3, font = 2, col = "blue")
axis(2, at = seq(signif(D[1]/sum(D),1)-0.1, 1, 0.02))
abline(a = 0.95, b = 0, col = "red")

#---

library(ggplot2) 
library(GGally)
ggpairs(data = data.frame(Q[,1:3]), columnLabels = c("V1", "V2", "V3"))

Q1 = Q[,1] # first PCA eigenvector:V1
Q2 = Q[,2] # second PCA eigenvector:V2
Q3 = Q[,3] # third PCA eigenvector:V3

#library(rgl)
#plot3d(Q[,1:3])

library(plotly)
plot_ly(x = Q1, y = Q2, z = Q3, type = "scatter3d", mode = "markers")
plot_ly(z = ~Q[,1:3]) %>% add_surface()
## Surface Plot With Contours
#plot_ly(z = ~Q[,1:3]) %>% add_surface(
#  contours = list(
#    z = list(
#      show=TRUE,
#      usecolormap=TRUE,
#      highlightcolor="#ff0000",
#      project=list(z=TRUE)
#      )
#    )
#  )


#---

true_set = BA[,1]
Y = data.frame(true_set, SDATA%*%V)
CL1 = Y[Y[,match("true_set", names(Y))] == "Bearish",]
CL2 = Y[Y[,match("true_set", names(Y))] == "Bullish",]
CL3 = Y[Y[,match("true_set", names(Y))] == "Neither",]

n1 = nrow(CL1)
n2 = nrow(CL2)
n3 = nrow(CL3)
N = sum(n1, n2, n3)

N == nrow(BA) # verification

SX = Y[,-1]


library(pca3d)
pca2d(prcomp(BA[,-1], scale. = TRUE), group = BA[,1])
pca3d(prcomp(BA[,-1], scale. = TRUE), group = BA[,1])


t1_stop = proc.time()
t1 = t1_stop - t1_start
print(t1)

#------
#Q2
t2_start = proc.time()

kmean = function(k) {
  set.seed(123)
  kmeans(x = SX, centers = k, nstart = 50, iter.max = 50)
}
cluster = function(k) {
  kmean(k)$cluster
}
SWS = function(k) { # Sum/Total Within-Clusters Sum of Squares
  kmean(k)$tot.withinss
}
# Clustering Performance Perf(k): % reduction of total dispersion by k-clustering
Perf = function(k) { 
  1 - SWS(k)/SWS(1)
}
# Elbow Method for choosing the best k
k = c(1:50)
layout(matrix(c(1), 1, 1))
plot(k, sapply(k, SWS),
     type = "b", pch = 19, col = "black", frame = FALSE,
     xlim = c(0, max(k)), 
     xlab = "k", ylab = "Total Within-Clusters Sum of Squares",
     main = "Elbow Method")

layout(matrix(c(1:2), nrow = 1, ncol = 2))
k1 = c(1:20)
plot(k1, sapply(k1, Perf), col = "black", 
     type = "b", xlab = "k", ylab = "Perf(k)", 
     main = "Clustering Performance",
     panel.first=grid())

s = function(k, c) {
  length(cluster(k)[cluster(k)==c])
}
f = function(k, c) { #class frequencies
  s(k, c)/length(cluster(k))
}

G = function(k) {
  1 - sum((mapply(f, c(k), c(1:k)))^2)
}
plot(k1, sapply(k1, G), col = "black",
     type = "b", ylim = c(0, 1), xlab = "k", ylab = "G(k)",
     main = "Gini Impurity Index", panel.first=grid())
lines(k1, sapply(k1, function(k) {1 - 1/k}), type = "l", col = "red")
legend(0.5, 1, c("max Impurity", "Gini Index G(k)"), lwd = 2, 
       col = c("red", "black"), pch = 1, cex = 0.5)


kbest = 3


t2_stop = proc.time()
t2 = t2_stop - t2_start
print(t2)

#Q3
t3_start = proc.time()

fm = function(j) {
  A = function(m, j) {
    # class 1: Bearish; class 2: Bullish; class3: Neither
    length(intersect(which(as.integer(as.factor(true_set))==m),
                     which(cluster(3)==j)))
  }
  fm = rbind(A(m = 1, j)/s(k = kbest, c = j), 
             A(m = 2, j)/s(k = kbest, c = j), 
             A(m = 3, j)/s(k = kbest, c = j))
  return(fm)
}

for (j in 1:kbest) {
  cat("when j =", paste0(j, ", we have:\n"))
  cat("center A_j:\n") 
  print(kmean(j)$center)
  cat("Size S_j:\n")
  print(kmean(j)$size) # table(kmean(j)$cluster)
  cat("Dispersion DIS_j:", SWS(j), "\n")
  cat("Gini G_j:", G(j), "\n")
  cat("Frequency fm_j:\n")
  FREQ_j = c()
  for (k in 1:j) {
    FREQ_j = append(FREQ_j, fm(k))
  }
  FREQ_j = matrix(FREQ_j, nrow = j)
  print(FREQ_j) # frequencies of each class in CLj
  cat("\n")
}

library(scatterplot3d)
par(mfrow=c(1,1))
#scatterplot3d(Y[,2:4], 
#              color = c("red", "blue", "orange")[as.numeric(Y[,1])], 
#              pch=20, xlab='V1', ylab='V2', zlab='V3', 
#              main='3D Projection of Cluster')

scatterplot3d(Y[,2:4], 
              color = c("red", "blue", "orange")[kmean(kbest)$cluster], 
              pch=20, xlab='V1', ylab='V2', zlab='V3', 
              main='3D Projection of Cluster')

t3_stop = proc.time()
t3 = t3_stop - t3_start
print(t3)
#----

#Q4
t4_start = proc.time()

#---
table(true_set)

set.seed(123)
x1 = sample(x = nrow(CL1), 
            size = as.integer(0.8*nrow(CL1)), 
            replace = FALSE)
x2 = sample(x = nrow(CL2), 
            size = as.integer(0.8*nrow(CL2)), 
            replace = FALSE)
time1 = ceiling(as.integer(0.8*nrow(CL3))/length(x1))
time2 = ceiling(as.integer(0.8*nrow(CL3))/length(x2))
i1 = sample(x = setdiff(c(1:(length(x1)*time1)), seq(1, length(x1)*time1, time1)), 
            size = length(x1)*time1 - as.integer(0.8*nrow(CL3)), 
            replace = FALSE)
i2 = sample(x = setdiff(c(1:(length(x2)*time2)), seq(1, length(x2)*time2, time2)), 
            size = length(x2)*time2 - as.integer(0.8*nrow(CL3)), 
            replace = FALSE)

pd_CL1train = rep(x1, each = time1)[-i1]
pd_CL2train = rep(x2, each = time2)[-i2]
setdiff(x1, pd_CL1train) # verification for all the original 80% of cases are not missed, so we expect the output be integer(0)
setdiff(x2, pd_CL2train)
# setdiff(x, y) is the material that is in x, that is not in y

y1 = setdiff(c(1:nrow(CL1)), x1)
y2 = setdiff(c(1:nrow(CL2)), x2)
TIME1 = ceiling((nrow(CL3) - as.integer(0.8*nrow(CL3)))/length(y1))
TIME2 = ceiling((nrow(CL3) - as.integer(0.8*nrow(CL3)))/length(y2))
j1 = sample(x = setdiff(c(1:(length(y1)*TIME1)), seq(1, (length(y1)*TIME1), TIME1)), 
            size = length(y1)*TIME1 - (nrow(CL3) - as.integer(0.8*nrow(CL3))),
            replace = FALSE)
j2 = sample(x = setdiff(c(1:(length(y2)*TIME2)), seq(1, (length(y2)*TIME2), TIME2)), 
            size = length(y2)*TIME2 - (nrow(CL3) - as.integer(0.8*nrow(CL3))),
            replace = FALSE)
pd_CL1test = rep(y1, each = TIME1)[-j1]
pd_CL2test = rep(y2, each = TIME2)[-j2]

setdiff(y1, pd_CL1test) # verification for all the original 20% of cases are not missed, so we expect the output be integer(0)
setdiff(y2, pd_CL2test)
intersect(pd_CL1train, pd_CL1test) # verification for test set and training set do not intersect
intersect(pd_CL2train, pd_CL2test)

pd_CL3train = sample(x = nrow(CL3), size = round(0.8*nrow(CL3)), replace = FALSE)

trainCL1 = CL1[pd_CL1train,]
testCL1 = CL1[pd_CL1test,]

trainCL2 = CL2[pd_CL2train,]
testCL2 = CL2[pd_CL2test,]

trainCL3 = CL3[pd_CL3train,]
testCL3 = CL3[-pd_CL3train,]

trainset = rbind(trainCL1, trainCL2, trainCL3)[,-1] # newTRAJ
testset = rbind(testCL1, testCL2, testCL3)[,-1] # newTESJ

trainset_target = as.factor(rbind(trainCL1, trainCL2, trainCL3)[,1])
testset_target = as.factor(rbind(testCL1, testCL2, testCL3)[,1])
#---
#---
set.seed(123)
pd1 = sample(x = 2, size = nrow(CL1), replace = TRUE, prob = c(0.8, 0.2)) # partition data of CL1
pd2 = sample(x = 2, size = nrow(CL2), replace = TRUE, prob = c(0.8, 0.2)) # partition data of CL2
pd3 = sample(x = 2, size = nrow(CL3), replace = TRUE, prob = c(0.8, 0.2)) # partition data of CL3
traincl1 = CL1[pd1==1, ]
testcl1 = CL1[pd1==2, ]
traincl2 = CL2[pd2==1, ]
testcl2 = CL2[pd2==2, ]
traincl3 = CL3[pd3==1, ]
testcl3 = CL3[pd3==2, ]

TRAINSET_TARGET = rbind(traincl1, traincl2, traincl3)[,1] # true train set 
TESTSET_TARGET = rbind(testcl1, testcl2, testcl3)[,1] # true test set

TRAINSET = rbind(traincl1, traincl2, traincl3)[,-1]
TESTSET = rbind(testcl1, testcl2, testcl3)[,-1]


t4_stop = proc.time()
t4 = t4_stop - t4_start
print(t4)

#---
#Q5
t5_start = proc.time()

library(randomForest)
rf = function(ntrees) {
  set.seed(123)
  rf = randomForest(trainset_target~., data = trainset, ntree = ntrees, mtry = sqrt(p))
  return(rf)
}
library(caret)
trainconf = function(ntrees) {
  pred1 = predict(rf(ntrees), TRAINSET)
  trainconf = confusionMatrix(pred1, TRAINSET_TARGET)$table
  return(trainconf)
}
trainperf = function(ntrees) {
  pred1 = predict(rf(ntrees), TRAINSET)
  trainperf = confusionMatrix(pred1, TRAINSET_TARGET)$overall[1]
  return(trainperf)
}
testconf = function(ntrees) {
  pred2 = predict(rf(ntrees), TESTSET)
  testconf = confusionMatrix(pred2, TESTSET_TARGET)$table
  return(testconf)
}
testperf = function(ntrees) { # accuracy
  pred2 = predict(rf(ntrees), TESTSET)
  testperf = confusionMatrix(pred2, TESTSET_TARGET)$overall[1] 
  return(testperf)
}
n = c(100, 200, 300, 400)
for (i in n) {
  cat("ntree =", paste0(i, ":\n"))
  print(trainconf(i))
  print(trainperf(i))
  cat("\n")
}
for (i in n) {
  cat("ntree =", paste0(i, ":\n"))
  print(testconf(i))
  print(testperf(i))
  cat("\n")
}

plot(n, mapply(trainperf, n),
     ylim  = c(0.86, 0.93),
     panel.first=grid(),
     ylab = "accuracy",
     xlab = "ntree",
     main = "ntree vs. accuracy",
     col = "red", 
     type = "o")
lines(n, mapply(testperf, n), 
      col = "blue", 
      type = "o")
legend(100, 0.93, c("trainperf", "testperf"), lwd = 2, 
       col = c("red", "blue"), pch = 1, cex = 0.5)

t5_stop = proc.time()
t5 = t5_stop - t5_start
print(t5)


#Q6
t6_start = proc.time()

conf = function(x) {
  n = n[x]
  conf = testconf(n)
  return(conf)
}
coeff = function(k) {
  coeff = NULL
  for (i in c(1:length(n))) {
    coeff[i] = diag(matrix(mapply(conf, i), 3))[k]
  }
  return(coeff)
}
library(plyr) # for round_any() function
layout(matrix(c(1), 1, 1))
plot(n, coeff(1), type = "b", col = "red", 
     ylim = c(min(mapply(coeff, 1:3)), 5 + round_any(max(mapply(coeff, 1:3)), accuracy = 5, f = ceiling)), 
     panel.first=grid(), xlab = "ntree", ylab = "diagonal coefficient")
lines(n, coeff(2), type = "b", col = "green")
lines(n, coeff(3), type = "b", col = "blue") 
legend(n[1], 185, legend = c("coeff 1","coeff 2","coeff 3"), col=c("red","green","blue"), lty=1:1, cex=0.5)

BNT = 100 # best value for ntrees

t6_stop = proc.time()
t6 = t6_stop - t6_start
print(t6)


#Q7
t7_start = proc.time()

bestRF = rf(ntrees = BNT)
IM = bestRF$importance 
sort(IM, decreasing = TRUE) # display decreasing values of reordered importances

t7_stop = proc.time()
t7 = t7_stop - t7_start
print(t7)

#Q8
t8_start = proc.time()

target = rbind(trainCL1, trainCL2, trainCL3, testCL1, testCL2, testCL3)[,1]

Z1 = order(IM, decreasing = TRUE)[1] # the most important feature
layout(matrix(c(1:3), 1, 3))
c_1 = rbind(trainset, testset)[target=="Bearish",Z1]
c_2 = rbind(trainset, testset)[target=="Bullish",Z1]
c_3 = rbind(trainset, testset)[target=="Neither",Z1]
hist(x = c_1, breaks = 100, 
     main = "Histogram for Bearish", xlab = "most important feature")
hist(x = c_2, breaks = 100, 
     main = "Histogram for Bullish", xlab = "most important feature")
hist(x = c_3, breaks = 100, 
     main = "Histogram for Neither", xlab = "most important feature")

Z2 = order(IM, decreasing = FALSE)[1] # the least important feature
layout(matrix(c(1:3), 1, 3))
c_4 = rbind(trainset, testset)[target=="Bearish",Z2]
c_5 = rbind(trainset, testset)[target=="Bullish",Z2]
c_6 = rbind(trainset, testset)[target=="Neither",Z2]
hist(x = c_4, breaks = 100, 
     main = "Histogram for Bearish", xlab = "least important feature")
hist(x = c_5, breaks = 100, 
     main = "Histogram for Bullish", xlab = "least important feature")
hist(x = c_6, breaks = 100, 
     main = "Histogram for Neither", xlab = "least important feature")

# reference of Kolmogorov-Smirnov Goodness-of-Fit Test (KS test): 
# https://www.itl.nist.gov/div898/handbook/eda/section3/eda35g.htm
# used to decide if a sample comes from a population with a specific distribution
# H0:	The data follow a specified distribution
# Ha:	The data do not follow a specified distribution
p_value = function(x1, x2) {
  ks = ks.test(x1, x2, alternative = "two.sided")$p.value
  return(ks)
}
options(warn=-1) # turn the warning off


# Decision Rule:
# Reject H0 if test_statistic >= critical_value, and fail to reject H0 if test_statistic < critical_value
# Reject H0 if p-value <= alpha, and fail to reject H0 if p-value > alpha
alpha = 0.05

p_1 = p_value(c_1, c_2)
p_2 = p_value(c_1, c_3)
p_3 = p_value(c_2, c_3)
for (i in c(p_1, p_2, p_3)) {
  cat("p_value is", paste0(i, ".\n"))
  if (i <= alpha) {
    cat("The null hypothesis is rejected. Two samples do not follow the same specified distribution.\n")
  } else {
    cat("The null hypothesis is not rejected. Two samples follow the same specified distribution.\n")
  }
  cat("\n")
}

p_4 = p_value(c_4, c_5)
p_5 = p_value(c_4, c_6)
p_6 = p_value(c_5, c_6)
for (i in c(p_4, p_5, p_6)) {
  cat("p_value is", paste0(i, ".\n"))
  if (i <= alpha) {
    cat("The null hypothesis is rejected. Two samples do not follow the same specified distribution.\n")
  } else {
    cat("The null hypothesis is not rejected. Two samples follow the same specified distribution.\n")
  }
  cat("\n")
}

options(warn=0) # turn the warning back on

t8_stop = proc.time()
t8 = t8_stop - t8_start
print(t8)


#Q9 & Q10
t9_start = proc.time()

#SIZE = as.integer(length(CLUj)/kbest) # balanced size for each class
#
#x2 = sample(x = nrow(CL2_CLUj), 
#            size = as.integer(0.8*nrow(CL2_CLUj)), 
#            replace = FALSE)
#x3 = sample(x = nrow(CL3_CLUj), 
#            size = as.integer(0.8*nrow(CL3_CLUj)), 
#            replace = FALSE)
#time2 = ceiling(as.integer(0.8*SIZE)/length(x2))
#time3 = ceiling(as.integer(0.8*SIZE)/length(x3))
#i2 = sample(x = setdiff(c(1:(length(x2)*time2)), seq(1, length(x2)*time2, time2)), 
#            size = length(x2)*time2 - as.integer(0.8*SIZE), 
#            replace = FALSE)
#i3 = sample(x = setdiff(c(1:(length(x3)*time3)), seq(1, length(x3)*time3, time3)), 
#            size = length(x3)*time3 - as.integer(0.8*SIZE), 
#            replace = FALSE)
#
#pd_CL2train = rep(x2, each = time2)[-i2]
#pd_CL3train = rep(x3, each = time3)[-i3]
#setdiff(x2, pd_CL2train) # verification for all the original 80% of cases are not missed, so we expect the output be integer(0)
#setdiff(x3, pd_CL3train)
## setdiff(x, y) is the material that is in x, that is not in y
#
#y2 = setdiff(c(1:nrow(CL2_CLUj)), x2)
#y3 = setdiff(c(1:nrow(CL3_CLUj)), x3)
#TIME2 = ceiling((SIZE - as.integer(0.8*SIZE))/length(y2))
#TIME3 = ceiling((SIZE - as.integer(0.8*SIZE))/length(y3))
#j2 = sample(x = setdiff(c(1:(length(y2)*TIME2)), seq(1, (length(y2)*TIME2), TIME2)), 
#            size = length(y2)*TIME2 - (SIZE - as.integer(0.8*SIZE)),
#            replace = FALSE)
#j3 = sample(x = setdiff(c(1:(length(y3)*TIME3)), seq(1, (length(y3)*TIME3), TIME3)), 
#            size = length(y3)*TIME3 - (SIZE - as.integer(0.8*SIZE)),
#            replace = FALSE)
#pd_CL2test = rep(y2, each = TIME2)[-j2]
#pd_CL3test = rep(y3, each = TIME3)[-j3]
#
#setdiff(y2, pd_CL2test) # verification for all the original 20% of cases are not missed, so we expect the output be integer(0)
#setdiff(y3, pd_CL3test)
#intersect(pd_CL2train, pd_CL2test) # verification for test set and training set do not intersect
#intersect(pd_CL3train, pd_CL3test)
#
#trainCL2 = CL2_CLUj[pd_CL2train,]
#testCL2 = CL2_CLUj[pd_CL2test,]
#
#trainCL3 = CL3_CLUj[pd_CL3train,]
#testCL3 = CL3_CLUj[pd_CL3test,]
#
#pd_CL1train = sample(x = nrow(CL1_CLUj), size = round(0.8*SIZE), replace = FALSE)
#pd_CL1test = sample(x = c(1:nrow(CL1_CLUj))[-pd_CL1train], size = round(0.2*SIZE), replace = FALSE)
#
#intersect(pd_CL1train, pd_CL1test)
#
#trainCL1 = CL1_CLUj[pd_CL1train,]
#testCL1 = CL1_CLUj[pd_CL1test,]
#train_CLUj = rbind(trainCL1, trainCL2, trainCL3)
#test_CLUj = rbind(testCL1, testCL2, testCL3)
#
#---
#pd1 = sample(x = 2, size = nrow(CL1_CLUj), replace = TRUE, prob = c(0.8, 0.2)) # partition data of CL1
#pd2 = sample(x = 2, size = nrow(CL2_CLUj), replace = TRUE, prob = c(0.8, 0.2)) # partition data of CL2
#pd3 = sample(x = 2, size = nrow(CL3_CLUj), replace = TRUE, prob = c(0.8, 0.2)) # partition data of CL3
#traincl1 = CL1_CLUj[pd1==1, ]
#testcl1 = CL1_CLUj[pd1==2, ]
#traincl2 = CL2_CLUj[pd2==1, ]
#testcl2 = CL2_CLUj[pd2==2, ]
#traincl3 = CL3_CLUj[pd3==1, ]
#testcl3 = CL3_CLUj[pd3==2, ]
#
#TRAIN_CLUj = rbind(traincl1, traincl2, traincl3)
#TEST_CLUj = rbind(testcl1, testcl2, testcl3)



# parameter CL is the data of a class/category extracted from the original data set
pdTRAIN = function(CL, size) { # partition data
  # size: balanced size for each class
  set.seed(123)
  if (SIZE > nrow(CL)) {
    x = sample(x = nrow(CL), 
               size = as.integer(0.8*nrow(CL)), 
               replace = FALSE)
    time = ceiling(as.integer(0.8*SIZE)/length(x))
    i = sample(x = setdiff(c(1:(length(x)*time)), seq(1, length(x)*time, time)), 
               size = length(x)*time - as.integer(0.8*SIZE), 
               replace = FALSE)
    pd_CLtrain = rep(x, each = time)[-i]
    return(pd_CLtrain)
  } else if (SIZE <= nrow(CL)) {
    pd_CLtrain = sample(x = nrow(CL), size = round(0.8*SIZE), replace = FALSE)
    return(pd_CLtrain)
  }
}
pdTEST = function(CL, size) {
  set.seed(123)
  if (SIZE > nrow(CL)) {
    x = sample(x = nrow(CL), 
               size = as.integer(0.8*nrow(CL)), 
               replace = FALSE)
    y = setdiff(c(1:nrow(CL)), x)
    TIME = ceiling((SIZE - as.integer(0.8*SIZE))/length(y))
    j = sample(x = setdiff(c(1:(length(y)*TIME)), seq(1, (length(y)*TIME), TIME)), 
               size = length(y)*TIME - (SIZE - as.integer(0.8*SIZE)),
               replace = FALSE)
    pd_CLtest = rep(y, each = TIME)[-j]
    return(pd_CLtest)
  } else if (SIZE <= nrow(CL)) {
    pd_CLtrain = pdTRAIN(CL)
    pd_CLtest = sample(x = c(1:nrow(CL))[-pd_CLtrain], size = round(0.2*SIZE), replace = FALSE)
    return(pd_CLtest)
  }
}
balanced_train = function(CL, size) { 
  pd_CLtrain = pdTRAIN(CL, size)
  trainCL = CL[pd_CLtrain,]
  return(trainCL)
}
balanced_test = function(CL, size) {
  pd_CLtest = pdTEST(CL, size)
  testCL = CL[pd_CLtest,]
  return(testCL)
}
pd_original_train = function(CL) {
  set.seed(123)
  pd = sample(x = nrow(CL), size = round(0.8*nrow(CL)), replace = FALSE)
  return(pd)
}
original_train = function(CL) {
  original_train = CL[pd_original_train(CL),]
  return(original_train)
}
original_test = function(CL) {
  original_test = CL[-pd_original_train(CL),]
  return(original_test)
}

SIZE = as.integer(length(true_set)/length(table(true_set))) # balanced size for each class


# kbest = 3

CLUj = function(k) {
  kmeans(x = SX, 
         center = k, # kbest = 3
         nstart = 50, 
         iter.max = 50)$cluster
}

for (k in c(2:4)){
  set.seed(2020)
  SY = data.frame(SX)
  if (k == 4) {
    CL1_CLUj = SY[as.vector(CLUj(4)) == 1,]
    CL1_CLUj = data.frame(true_set = rep(1, nrow(CL1_CLUj)), CL1_CLUj)
    CL2_CLUj = SY[as.vector(CLUj(4)) == 2,]
    CL2_CLUj = data.frame(true_set = rep(2, nrow(CL2_CLUj)), CL2_CLUj)
    CL3_CLUj = SY[as.vector(CLUj(4)) == 3,]
    CL3_CLUj = data.frame(true_set = rep(3, nrow(CL3_CLUj)), CL3_CLUj)
    CL4_CLUj = SY[as.vector(CLUj(4)) == 4,]
    CL4_CLUj = data.frame(true_set = rep(4, nrow(CL4_CLUj)), CL4_CLUj)
    train_CLUj = rbind(balanced_train(CL1_CLUj, SIZE), 
                       balanced_train(CL2_CLUj, SIZE), 
                       balanced_train(CL3_CLUj, SIZE),
                       balanced_train(CL4_CLUj, SIZE))
    test_CLUj = rbind(balanced_test(CL1_CLUj, SIZE), 
                      balanced_test(CL2_CLUj, SIZE), 
                      balanced_test(CL3_CLUj, SIZE),
                      balanced_test(CL4_CLUj, SIZE))
    TRAIN_CLUj = rbind(original_train(CL1_CLUj), 
                       original_train(CL2_CLUj), 
                       original_train(CL3_CLUj),
                       original_train(CL4_CLUj))
    TEST_CLUj = rbind(original_test(CL1_CLUj), 
                      original_test(CL2_CLUj), 
                      original_test(CL3_CLUj),
                      original_test(CL4_CLUj))
    } else if (k == 2) {
      CL1_CLUj = SY[as.vector(CLUj(2)) == 1,]
      CL1_CLUj = data.frame(true_set = rep(1, nrow(CL1_CLUj)), CL1_CLUj)
      CL2_CLUj = SY[as.vector(CLUj(2)) == 2,]
      CL2_CLUj = data.frame(true_set = rep(2, nrow(CL2_CLUj)), CL2_CLUj)
      train_CLUj = rbind(balanced_train(CL1_CLUj, SIZE), 
                         balanced_train(CL2_CLUj, SIZE))
      test_CLUj = rbind(balanced_test(CL1_CLUj, SIZE), 
                        balanced_test(CL2_CLUj, SIZE))
      TRAIN_CLUj = rbind(original_train(CL1_CLUj), 
                         original_train(CL2_CLUj))
      TEST_CLUj = rbind(original_test(CL1_CLUj), 
                        original_test(CL2_CLUj))
      } else if (k == 3) {
        CL1_CLUj = SY[as.vector(CLUj(3)) == 1,]
        CL1_CLUj = data.frame(true_set = rep(1, nrow(CL1_CLUj)), CL1_CLUj)
        CL2_CLUj = SY[as.vector(CLUj(3)) == 2,]
        CL2_CLUj = data.frame(true_set = rep(2, nrow(CL2_CLUj)), CL2_CLUj)
        CL3_CLUj = SY[as.vector(CLUj(3)) == 3,]
        CL3_CLUj = data.frame(true_set = rep(3, nrow(CL3_CLUj)), CL3_CLUj)
        train_CLUj = rbind(balanced_train(CL1_CLUj, SIZE), 
                           balanced_train(CL2_CLUj, SIZE), 
                           balanced_train(CL3_CLUj, SIZE))
        test_CLUj = rbind(balanced_test(CL1_CLUj, SIZE), 
                          balanced_test(CL2_CLUj, SIZE), 
                          balanced_test(CL3_CLUj, SIZE))
        TRAIN_CLUj = rbind(original_train(CL1_CLUj), 
                           original_train(CL2_CLUj), 
                           original_train(CL3_CLUj))
        TEST_CLUj = rbind(original_test(CL1_CLUj), 
                          original_test(CL2_CLUj), 
                          original_test(CL3_CLUj))
      }
  
  rf_CLUj = randomForest(as.factor(as.character(train_CLUj[,1]))~., 
                         data = train_CLUj[,-1], 
                         ntree = BNT, 
                         mtry = sqrt(p))
  pred_CLUj_1 = predict(rf_CLUj, TEST_CLUj[,-1])
  testconf_CLUj = table(pred_CLUj_1, TEST_CLUj[,1])
  pred_CLUj_2 = predict(rf_CLUj, TRAIN_CLUj[,-1])
  trainconf_CLUj = table(pred_CLUj_2, TRAIN_CLUj[,1])
  cat("-----------------------------------\n")
  cat("when k =", paste0(k, ":\n\nconfusion matrix for test set:"))
  print(testconf_CLUj)
  cat("\naccuracy of test set:\n", sum(diag(testconf_CLUj))/sum(testconf_CLUj),"\n\n")
  cat("confusion matrix for training set:")
  print(trainconf_CLUj)
  cat("\naccuracy of training set:\n", sum(diag(trainconf_CLUj))/sum(trainconf_CLUj),"\n")
  cat("-----------------------------------\n")
}

t9_stop = proc.time()
t9 = t9_stop - t9_start
print(t9)



#Q11
t11_start = proc.time()

library(e1071)

newTRAIN = rbind(balanced_train(CL1, SIZE), balanced_train(CL2, SIZE))
newTEST = rbind(balanced_test(CL1, SIZE), balanced_test(CL2, SIZE))


svm_model = svm(as.factor(as.character(newTRAIN[,1]))~., 
                data = newTRAIN[,-1], 
                kernel = "linear", 
                scale = TRUE,
                cost = 1)

# when cost is small, margins will be wide, more tolerence for misclassification
summary(svm_model)

pred = predict(svm_model, newTEST[,-1])
conf = table(pred, true_test = as.factor(as.character(newTEST[,1])))
print(conf)
sum(diag(conf))/sum(conf)



sx = (rbind(newTRAIN, newTEST))[1:9]
plot(sx[,-1], col = as.factor(sx[,1]))

t11_stop = proc.time()
t11 = t11_stop - t11_start
print(t11)




t11_start = proc.time()

library(e1071)
true_cluster = as.vector(CLUj(3))
newTRAIN_CL1 = data.frame(true_cluster = true_cluster[as.integer(row.names(balanced_train(CL1, SIZE)))],
                          balanced_train(CL1, SIZE))[,-2]
newTRAIN_CL2 = data.frame(true_cluster = true_cluster[as.integer(row.names(balanced_train(CL2, SIZE)))],
                          balanced_train(CL2, SIZE))[,-2]

newTEST_CL1 = data.frame(true_cluster = true_cluster[as.integer(row.names(balanced_test(CL1, SIZE)))],
                         balanced_test(CL1, SIZE))[,-2]
newTEST_CL2 = data.frame(true_cluster = true_cluster[as.integer(row.names(balanced_test(CL2, SIZE)))],
                         balanced_test(CL2, SIZE))[,-2]

newTRAIN = rbind(newTRAIN_CL1, newTRAIN_CL2)
newTEST = rbind(newTEST_CL1, newTEST_CL2)

svm_model = svm(as.factor(as.character(newTRAIN[,1]))~., 
                data = newTRAIN[,-1], 
                kernel = "linear", 
                scale = TRUE,
                cost = 1)

# when cost is small, margins will be wide, more tolerence for misclassification
summary(svm_model)

pred = predict(svm_model, newTEST[,-1])
conf = table(pred, true_test = as.factor(as.character(newTEST[,1])))
print(conf)
sum(diag(conf))/sum(conf)



sx = (rbind(newTRAIN, newTEST))[1:9]
plot(sx[,-1], col = as.factor(sx[,1]))

t11_stop = proc.time()
t11 = t11_stop - t11_start
print(t11)


