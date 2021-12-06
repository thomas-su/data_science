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
true_set = as.factor(DATA[,1])
start_col = match("r0c0",names(DATA))
end_col = ncol(DATA)
X = DATA[,start_col: end_col]
m = sapply(X, mean) # same as apply(X, MARGIN = 2, mean)
s = sapply(X, sd)
SDATA = scale(X)

# Correlation Matrix Computation
R = cor(SDATA)
# sigma = cov(X) # variance-covariance matrix "sigma"
# R=cov2cor(sigma) # either way to find correlation matrix
# all.equal(cor(X), cov2cor(sigma)) 

# Eigenvalues and Eigenvectors
D = eigen(R)$values
# or D = (summary(pca)$sdev)^2
Q = eigen(R)$vectors


p = order(abs(cumsum(D)/sum(D) - 0.95), decreasing = FALSE)[1]
V = Q[, 1:p]

Y = data.frame(true_set, SDATA%*%V)
CL1 = Y[Y[,match("true_set", names(Y))] == "BOOKMAN",]
CL2 = Y[Y[,match("true_set", names(Y))] == "COMIC",] # subset(Y, Y[,match("true_set", names(Y))] == "COMIC")
CL3 = Y[Y[,match("true_set", names(Y))] == "MONOTYPE",]

n1 = nrow(CL1)
n2 = nrow(CL2)
n3 = nrow(CL3)
N = sum(n1, n2, n3)

N == nrow(DATA) # verification

SX = Y[,-1]

# Question 1
set.seed(123)
pd1 = sample(x = 2, size = nrow(CL1), replace = TRUE, prob = c(0.8, 0.2)) # partition data of CL1
pd2 = sample(x = 2, size = nrow(CL2), replace = TRUE, prob = c(0.8, 0.2)) # partition data of CL2
pd3 = sample(x = 2, size = nrow(CL3), replace = TRUE, prob = c(0.8, 0.2)) # partition data of CL3

trainCL1 = CL1[pd1==1, ]
testCL1 = CL1[pd1==2, ]
trainCL2 = CL2[pd2==1, ]
testCL2 = CL2[pd2==2, ]
trainCL3 = CL3[pd3==1, ]
testCL3 = CL3[pd3==2, ]

TRAINSET_TARGET = rbind(trainCL1, trainCL2, trainCL3)[,1] # true train set 
TESTSET_TARGET = rbind(testCL1, testCL2, testCL3)[,1] # true test set

TRAINSET = rbind(trainCL1, trainCL2, trainCL3)[,-1]
TESTSET = rbind(testCL1, testCL2, testCL3)[,-1]


# Question 2: Random Forest(RF)
# reference: https://www.youtube.com/watch?v=dJclNIN-TPo
library(randomForest)
?randomForest


# Question 3.1 
set.seed(123)
rf = randomForest(TRAINSET_TARGET~., data = TRAINSET, ntree = 100, mtry = sqrt(p))
print(rf)
attributes(rf)
rf$confusion # the confusion matrix of the prediction (based on Out-of-Bag data)

# prediction & confusion matrix - train data
library(caret)
pred1 = predict(rf, TRAINSET) 
trainconf = confusionMatrix(pred1, TRAINSET_TARGET)$table # or table(pred1, TRAINSET_TARGET)
print(trainconf)
trainperf = confusionMatrix(pred1, TRAINSET_TARGET)$overall[1] # or sum(diag(trainconf))/sum(trainconf)
print(trainperf)

# prediction & confusion matrix - test data
pred2 = predict(rf, TESTSET)
testconf = confusionMatrix(pred2, TESTSET_TARGET)$table
print(testconf)
testperf = confusionMatrix(pred2, TESTSET_TARGET)$overall[1] 
print(testperf)


# Question 3.2
rf = function(ntrees) {
  set.seed(123)
  rf = randomForest(TRAINSET_TARGET~., data = TRAINSET, ntree = ntrees, mtry = sqrt(p))
  return(rf)
}
testconf = function(ntrees) {
  pred2 = predict(rf(ntrees), TESTSET)
  testconf = confusionMatrix(pred2, TESTSET_TARGET)$table
  return(testconf)
}
n = c(10, 50, 100, 200, 300, 400)
conf = function(x) {
  n = n[x]
  conf = testconf(n)
  return(conf)
}
testperf = function(ntrees) { # accuracy
  pred2 = predict(rf(ntrees), TESTSET)
  testperf = confusionMatrix(pred2, TESTSET_TARGET)$overall[1] 
  return(testperf)
}
conf(1) 
conf(2) 
conf(3) 
conf(4)
conf(5) 
conf(6)

testperf(ntrees = 10) 
testperf(50)
testperf(100)
testperf(200)
testperf(300)
testperf(400)

plot(n, mapply(testperf, n), type = "b", 
     panel.first=grid(),
     xlab = "ntree", ylab = "accuracy (testperf)")

coeff = function(k) {
  coeff = NULL
  for (i in c(1:6)) {
    coeff[i] = diag(matrix(mapply(conf, i), 3))[k]
  }
  return(coeff)
}
plot(n, coeff(1), type = "b", col = "red", ylim = c(80, 111), 
     panel.first=grid(), xlab = "ntree", ylab = "diagonal coefficient")
lines(n, coeff(2), type = "b", col = "green")
lines(n, coeff(3), type = "b", col = "blue")  

bntr = 200

# Question 4.1
set.seed(123)
bestRF = randomForest(TRAINSET_TARGET~., data = TRAINSET, ntree = bntr, mtry = sqrt(p))
print(bestRF)

# prediction & confusion matrix - test data (for bestRF)
pred2_b = predict(bestRF, TESTSET)
testconf_b = confusionMatrix(pred2_b, TESTSET_TARGET)$table
testperf_b = confusionMatrix(pred2_b, TESTSET_TARGET)$overall[1] 


IM = function(k) {
  IM = bestRF$importance[k]
  return(IM)
}
L = function(k) {
  L = D[k]
  return(L)
}
k = 1:p
mapply(IM, k) # IM_1, IM_2, ..., IM_p
plot(mapply(L, k), mapply(IM, k), xlab = "eigenvalue: L_k", ylab = "bestRF's Importance: IM_k")

# Question 4.2
set.seed(111) # not set.seed(123), bc we need a different starting number used to generate a sequence of random numbers
pd1_new = sample(x = 2, size = nrow(CL1), replace = TRUE, prob = c(0.8, 0.2)) # partition data of CL1
pd2_new = sample(x = 2, size = nrow(CL2), replace = TRUE, prob = c(0.8, 0.2)) # partition data of CL2
pd3_new = sample(x = 2, size = nrow(CL3), replace = TRUE, prob = c(0.8, 0.2)) # partition data of CL3

trainCL1_new = CL1[pd1_new==1, ]
testCL1_new = CL1[pd1_new==2, ]
trainCL2_new = CL2[pd2_new==1, ]
testCL2_new = CL2[pd2_new==2, ]
trainCL3_new = CL3[pd3_new==1, ]
testCL3_new = CL3[pd3_new==2, ]

TRAINSET_TARGET_new = rbind(trainCL1_new, trainCL2_new, trainCL3_new)[,1] # true train set 
TESTSET_TARGET_new = rbind(testCL1_new, testCL2_new, testCL3_new)[,1] # true test set

TRAINSET_new = rbind(trainCL1_new, trainCL2_new, trainCL3_new)[,-1]
TESTSET_new = rbind(testCL1_new, testCL2_new, testCL3_new)[,-1]

bestRF_new = randomForest(TRAINSET_TARGET_new~., data = TRAINSET_new, ntree = bntr, mtry = sqrt(p))
print(bestRF_new)

## prediction & confusion matrix - train data (new)
#library(caret)
#pred1_new = predict(bestRF_new, TRAINSET_new) 
#trainconf_new = confusionMatrix(pred1_new, TRAINSET_TARGET_new)$table # or table(pred1, TRAINSET_TARGET)
#print(trainconf_new)
#trainperf_new = confusionMatrix(pred1_new, TRAINSET_TARGET_new)$overall[1] # or sum(diag(trainconf))/sum(trainconf)
#print(trainperf_new)

# prediction & confusion matrix - test data (new)
pred2_new = predict(bestRF_new, TESTSET_new)
testconf_new = confusionMatrix(pred2_new, TESTSET_TARGET_new)$table
testperf_new = confusionMatrix(pred2_new, TESTSET_TARGET_new)$overall[1] 
 

CI = function(p, CL = 0.95, n) {
  ci = p + c(-1, 1)*qnorm(1-(1-CL)/2)*sqrt(p*(1-p)/n)
  return(ci)
}

print(testperf_b)  
print(testperf_new) 

CI(p = testperf_b, n = nrow(TESTSET))
CI(p = testperf_new, n = nrow(TESTSET_new))

print(testconf_b)
print(testconf_new)


# Question 5.1

# CL1 versus {CL2+CL3}
D1 = CL1
D0 = rbind(CL2, CL3)

set.seed(123)
x = sample(x = nrow(D1), size = as.integer(0.8*nrow(D1)), replace = FALSE)
time1 = ceiling(as.integer(0.8*nrow(D0))/length(x))
i = sample(x = setdiff(c(1:(length(x)*time1)), seq(1, length(x)*time1, time1)), 
           size = length(x)*time1 - as.integer(0.8*nrow(D0)), 
           replace = FALSE)
pd_D1train = rep(x, each = time1)[-i]
setdiff(x, pd_D1train) # verification for all the original 80% of cases are not missed, so we expect the output be integer(0)
# setdiff(x, y) is the material that is in x, that is not in y

y = setdiff(c(1:nrow(D1)), x)
TIME1 = ceiling((nrow(D0) - as.integer(0.8*nrow(D0)))/length(y))
j = sample(x = setdiff(c(1:(length(y)*TIME1)), seq(1, (length(y)*TIME1), TIME1)), 
           size = length(y)*TIME1 - (nrow(D0) - as.integer(0.8*nrow(D0))),
           replace = FALSE)
pd_D1test = rep(y, each = TIME1)[-j]
setdiff(y, pd_D1test) # verification for all the original 20% of cases are not missed, so we expect the output be integer(0)

intersect(pd_D1train, pd_D1test) # verification for test set and training set do not intersect

trainD1 = D1[pd_D1train,]
testD1 = D1[pd_D1test,]

pd_D0train = sample(x = nrow(D0), size = round(0.8*nrow(D0)), replace = FALSE)
trainD0 = D0[pd_D0train,]
testD0 = D0[-pd_D0train,]

trainset_D = rbind(trainD1, trainD0)[,-1]
testset_D = rbind(testD1, testD0)[,-1]

trainset_target_D = as.factor(rbind(trainD1, trainD0)[,1])
testset_target_D = as.factor(rbind(testD1, testD0)[,1])

library(plyr) # for mapvalues() function
trainset_target_D = mapvalues(trainset_target_D, c("BOOKMAN", "COMIC", "MONOTYPE"), c("D1", "D0", "D0"))
testset_target_D = mapvalues(testset_target_D, c("BOOKMAN", "COMIC", "MONOTYPE"), c("D1", "D0", "D0"))


RF1 = randomForest(as.factor(trainset_target_D)~., data = trainset_D, ntree = bntr, mtry = sqrt(p))

pred_D = predict(RF1, testset_D)
testconf_D = confusionMatrix(pred_D, as.factor(testset_target_D))$table
M1 = testconf_D
testperf_D = confusionMatrix(pred_D, as.factor(testset_target_D))$overall[1]
A1 = testperf_D


# CL2 versus {CL1+CL3}
E1 = CL2
E0 = rbind(CL1, CL3)

set.seed(123)
x2 = sample(x = nrow(E1), size = as.integer(0.8*nrow(E1)), replace = FALSE)
time2 = ceiling(as.integer(0.8*nrow(E0))/length(x2))
i2 = sample(x = setdiff(c(1:(length(x2)*time2)), seq(1, length(x2)*time2, time2)), 
            size = length(x2)*time2 - as.integer(0.8*nrow(E0)), 
            replace = FALSE)
pd_E1train = rep(x2, each = time2)[-i2]
setdiff(x2, pd_E1train) # verification for all the original 80% of cases are not missed, so we expect the output be integer(0)

y2 = setdiff(c(1:nrow(E1)), x2)
TIME2 = ceiling((nrow(E0) - as.integer(0.8*nrow(E0)))/length(y2))
j2 = sample(x = setdiff(c(1:(length(y2)*TIME2)), seq(1, (length(y2)*TIME2), TIME2)), 
            size = length(y2)*TIME2 - (nrow(E0) - as.integer(0.8*nrow(E0))),
            replace = FALSE)
pd_E1test = rep(y2, each = TIME2)[-j2]
setdiff(y2, pd_E1test) # verification for all the original 20% of cases are not missed, so we expect the output be integer(0)

intersect(pd_E1train, pd_E1test) # verification for test set and training set do not intersect


trainE1 = E1[pd_E1train,]
testE1 = E1[pd_E1test,]

pd_E0train = sample(x = nrow(E0), size = round(0.8*nrow(E0)), replace = FALSE)
trainE0 = E0[pd_E0train,]
testE0 = E0[-pd_E0train,]

trainset_E = rbind(trainE1, trainE0)[,-1]
testset_E = rbind(testE1, testE0)[,-1]

trainset_target_E = as.factor(rbind(trainE1, trainE0)[,1])
testset_target_E = as.factor(rbind(testE1, testE0)[,1])

trainset_target_E = mapvalues(trainset_target_E, c("BOOKMAN", "COMIC", "MONOTYPE"), c("E0", "E1", "E0"))
testset_target_E = mapvalues(testset_target_E, c("BOOKMAN", "COMIC", "MONOTYPE"), c("E0", "E1", "E0"))


RF2 = randomForest(as.factor(trainset_target_E)~., data = trainset_E, ntree = bntr, mtry = sqrt(p))

pred_E = predict(RF2, testset_E)
testconf_E = confusionMatrix(pred_E, as.factor(testset_target_E))$table
M2 = testconf_E
testperf_E = confusionMatrix(pred_E, as.factor(testset_target_E))$overall[1]
A2 = testperf_E


# CL3 versus {CL1+CL2}
F1 = CL3
F0 = rbind(CL1, CL2)

set.seed(123)
x3 = sample(x = nrow(F1), size = as.integer(0.8*nrow(F1)), replace = FALSE)
time3 = ceiling(as.integer(0.8*nrow(F0))/length(x3))
i3 = sample(x = setdiff(c(1:(length(x3)*time3)), seq(1, length(x3)*time3, time3)), 
            size = length(x3)*time3 - as.integer(0.8*nrow(F0)), 
            replace = FALSE)
pd_F1train = rep(x3, each = time3)[-i3]
setdiff(x3, pd_F1train) # verification for all the original 80% of cases are not missed, so we expect the output be integer(0)

y3 = setdiff(c(1:nrow(F1)), x3)
TIME3 = ceiling((nrow(F0) - as.integer(0.8*nrow(F0)))/length(y3))
j3 = sample(x = setdiff(c(1:(length(y3)*TIME3)), seq(1, (length(y3)*TIME3), TIME3)), 
            size = length(y3)*TIME3 - (nrow(F0) - as.integer(0.8*nrow(F0))),
            replace = FALSE)
pd_F1test = rep(y3, each = TIME3)[-j3]
setdiff(y3, pd_F1test) # verification for all the original 20% of cases are not missed, so we expect the output be integer(0)

intersect(pd_F1train, pd_F1test) # verification for test set and training set do not intersect


trainF1 = F1[pd_F1train,]
testF1 = F1[pd_F1test,]

pd_F0train = sample(x = nrow(F0), size = round(0.8*nrow(F0)), replace = FALSE)
trainF0 = F0[pd_F0train,]
testF0 = F0[-pd_F0train,]

trainset_F = rbind(trainF1, trainF0)[,-1]
testset_F = rbind(testF1, testF0)[,-1]

trainset_target_F = as.factor(rbind(trainF1, trainF0)[,1])
testset_target_F = as.factor(rbind(testF1, testF0)[,1])

trainset_target_F = mapvalues(trainset_target_F, c("BOOKMAN", "COMIC", "MONOTYPE"), c("F0", "F0", "F1"))
testset_target_F = mapvalues(testset_target_F, c("BOOKMAN", "COMIC", "MONOTYPE"), c("F0", "F0", "F1"))

RF3 = randomForest(as.factor(trainset_target_F)~., data = trainset_F, ntree = bntr, mtry = sqrt(p))

pred_F = predict(RF3, testset_F)
testconf_F = confusionMatrix(pred_F, as.factor(testset_target_F))$table
M3 = testconf_F
testperf_F = confusionMatrix(pred_F, as.factor(testset_target_F))$overall[1]
A3 = testperf_F


print(RF1)
print(RF2)
print(RF3)

print(A1)
print(A2)
print(A3)

print(M1)
print(M2)
print(M3)


# Question 5.2
B1 = confusionMatrix(mapvalues(predict(bestRF, testset_D), c("BOOKMAN", "COMIC", "MONOTYPE"), c("D1", "D0", "D0")), 
                     as.factor(testset_target_D))$overall[1]
B2 = confusionMatrix(mapvalues(predict(bestRF, testset_E), c("BOOKMAN", "COMIC", "MONOTYPE"), c("E0", "E1", "E0")),
                     as.factor(testset_target_E))$overall[1]
B3 = confusionMatrix(mapvalues(predict(bestRF, testset_F), c("BOOKMAN", "COMIC", "MONOTYPE"), c("F0", "F0", "F1")), 
                     as.factor(testset_target_F))$overall[1]

BM1 = confusionMatrix(mapvalues(predict(bestRF, testset_D), c("BOOKMAN", "COMIC", "MONOTYPE"), c("D1", "D0", "D0")), 
                      as.factor(testset_target_D))$table
BM2 = confusionMatrix(mapvalues(predict(bestRF, testset_E), c("BOOKMAN", "COMIC", "MONOTYPE"), c("E0", "E1", "E0")),
                      as.factor(testset_target_E))$table
BM3 = confusionMatrix(mapvalues(predict(bestRF, testset_F), c("BOOKMAN", "COMIC", "MONOTYPE"), c("F0", "F0", "F1")), 
                      as.factor(testset_target_F))$table


print(B1)
print(B2)
print(B3)

print(BM1)
print(BM2)
print(BM3)
