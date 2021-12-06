rm(list=ls()) # to remove all objects from a specified environment
cat("\f") # to clean console
library(readr)
setwd("~/Library/Mobile Documents/com~apple~CloudDocs/Study Files (Graduate)/MATH 6350/Homework/HW01")
Auto <- read_csv("cleanDataAuto.csv")
View(Auto)
names(Auto)
# ls(Auto)
X = data.matrix(Auto[, 2: ncol(Auto)])
# x1 = cyl; x2 = dis, x3 = hor, x4 = wei, x5 = acc
n = nrow(X)
p = ncol(X)
#------------------------------------------------------------
# 1
Xmeans = colMeans(X)
print(Xmeans)

for (i in 1: p) {
  cat(paste0("standard deviation for ", 
             names(Auto[i+1])), "is", sd(X[, i]),"\n\n")
} # standard deviation for X_1 to X_5

#------------------------------------------------------------
# 2
mpg = data.matrix(Auto[,1])
hist(mpg, main = "")
mtext("histogram of mpg", 3)
layout(matrix(c(1: 6), ncol = 3, nrow = 2, byrow = TRUE))
for (i in 1: p) {
  hist(X[, i], main = "", xlab = "", ylab = "")
  mtext(paste("histogram of",names(Auto[i + 1])), 3)
}
# histogram

#------------------------------------------------------------
# 3
layout(matrix(c(1: 6), ncol = 3, nrow = 2, byrow = TRUE))
mpg = data.matrix(Auto[,1])
for (i in 1: p) {
  plot(X[, i], mpg)
  mtext(paste("scatterplot of", names(Auto[i + 1])), 3)
  abline(lm(mpg~X[, i]), col = "red")
}

#------------------------------------------------------------
# 4

#------------------------------------------------------------
# 5
for (i in 1: p) {
  cat("Correlation of", names(Auto[i+1]), "and mpg:\n",
      cor(Auto[, i + 1], Auto[, 1]), "\n\n")
} #Correlation

#------------------------------------------------------------
# 6

#------------------------------------------------------------
# 7
#for (i in 1: p) {
#  cat("Quantile of", names(Auto[, i]), ":\n")
#  print(quantile(X[, i], probs = seq(0, 1, by = 0.01)))
#  cat("\n")
#} #quantile for X_1 to X_5
mpg = data.matrix(Auto[,1])
quantile = quantile(mpg, probs = seq(0, 1, by = 0.01))
print(quantile)
layout(matrix(c(1), ncol = 1, nrow = 1, byrow = TRUE))
plot(seq(0, 1, by = 0.01), quantile, 
     main = "Quantile Curve for MPG",xlab = "Percentile Varies",
     ylab = "Quantile Value")
# qqnorm(mpg)

#------------------------------------------------------------
# 8
mpg = data.matrix(Auto[,1])
LOWmpg.table = X[mpg <= quantile(mpg, probs = c(0.33)),]
HIGHmpg.table = X[mpg > quantile(mpg, probs = c(0.66)),]


#------------------------------------------------------------
# 9
layout(matrix(c(1: 10), ncol = 5, nrow = 2, byrow = TRUE))
for (i in 1: 5) {
  hist(LOWmpg.table[, i], main = "", xlab = "")
  mtext(paste(names(Auto[i + 1]),"in LOWmpg table"), side = 1, line = 3)
}

for (i in 1: 5) {
  hist(HIGHmpg.table[, i], main = "", xlab = "")
  mtext(paste(names(Auto[i + 1]),"in HIGHmpg table"), side = 1, line = 3)
}

#------------------------------------------------------------
# 10


#------------------------------------------------------------
# 11
mL = function(i) {mean(LOWmpg.table[, i])}
stdL = function(i) {sd(LOWmpg.table[, i])}
mH = function(i) {mean(HIGHmpg.table[, i])}
stdH = function(i) {sd(HIGHmpg.table[, i])}

for (i in 1: p) {
  cat(paste0("mean for ", names(Auto[i + 1]),
             " associated to all cases with LOWmpg is\n", 
             mL(i),"\n\n"))
}

for (i in 1: p) {
cat(paste0("standard deviation for ", names(Auto[i + 1]),
            " associated to all cases with LOWmpg is\n", 
            stdL(i),"\n\n"))
}

for (i in 1: p) {
  cat(paste0("mean for ", names(Auto[i + 1]),
             " associated to all cases with HIGHmpg is\n", 
             mH(i),"\n\n"))
}

for (i in 1: p) {
  cat(paste0("standard deviation for ", names(Auto[i + 1]),
             " associated to all cases with HIGHmpg is\n", 
             stdH(i),"\n\n"))
}

#------------------------------------------------------------
# 12
mL = matrix(colMeans(LOWmpg.table))
mH = matrix(colMeans(HIGHmpg.table))
stdL = matrix(apply(LOWmpg.table, MARGIN = 2, sd)) 
stdH = matrix(apply(HIGHmpg.table, MARGIN = 2, sd))
s = function(F) sqrt((stdL^2 + stdH^2) / nrow(LOWmpg.table))[F]
discr = function(F) abs(mH[F,1] - mL[F,1]) / s(F)

for (i in 1 : 5) {
  print(s(i))
}

for (i in 1 : 5) {
  print(discr(i))
}

#------------------------------------------------------------
# 13
thr = (mL * stdH + mH * stdL) / (stdH + stdL)
print(thr)

# reminder that X = data.matrix(Auto[, 2: ncol(Auto)])
scoreF = function(j) {
  score = c()
  if (mH[j,] > mL[j,]) {
    for (i in 1: nrow(X)) {
      if (X[i, j] > thr[j, ]) {
        score[i] = 1} else {
          score[i] = -1
        }
    }
  } else {
    for (i in 1: nrow(X)) {
      if (X[i, j] < thr[j, ]) {
        score[i] =  1} else {
          score[i] = - 1
        }
    }
  }
  print(score)
}
scoreF(1) 
scoreF(2)
scoreF(3) 
scoreF(4) 
scoreF(5)



#------------------------------------------------------------
# 14
fullscoreF = function(j) {
  score = 0
  if (mH[j,] > mL[j,]) {
    for (i in 1: nrow(X)) {
      if (X[i, j] > thr[j, ]) {
        score = score + 1} else {
          score = score -1
        }
    }
  } else {
    for (i in 1: nrow(X)) {
      if (X[i, j] < thr[j, ]) {
        score = score + 1} else {
          score = score - 1
        }
    }
  }
  print(score)
}
fullscoreF(1)
fullscoreF(2)
fullscoreF(3)
fullscoreF(4)
fullscoreF(5)

fs = 0
for (i in 1 : 5) {
  fs = fs + fullscoreF(i)
  paste(fs)
}







score.f1 = c()
if (mH[1,] > mL[1,]){
  for (i in 1: nrow(X)) {
    if (X[i, 1] > thr.F1){
      score.f1[i] = 1} else {
        score.f1[i] = -1
      }
  }
} else {
  for (i in 1: nrow(X)) {
    if (X[i, 1] < thr.F1){
      score.f1[i] =  1} else {
        score.f1[i] = - 1
      }
  }
}
score.f2 = c()
if (mH[2,] > mL[2,]){
  for (i in 1: nrow(X)) {
    if (X[i, 2] > thr.F2){
      score.f2[i] = 1} else {
        score.f2[i] = -1
      }
  }
} else {
  for (i in 1: nrow(X)) {
    if (X[i, 2] < thr.F2){
      score.f2[i] =  1} else {
        score.f2[i] = - 1
      }
  }
}
score.f3 = c()
if (mH[3,] > mL[3,]){
  for (i in 1: nrow(X)) {
    if (X[i, 3] > thr.F1){
      score.f3[i] = 1} else {
        score.f3[i] = -1
      }
  }
} else {
  for (i in 1: nrow(X)) {
    if (X[i, 3] < thr.F3){
      score.f3[i] =  1} else {
        score.f3[i] = - 1
      }
  }
}
score.f4 = c()
if (mH[4,] > mL[4,]){
  for (i in 1: nrow(X)) {
    if (X[i, 4] > thr.F4){
      score.f4[i] = 1} else {
        score.f4[i] = -1
      }
  }
} else {
  for (i in 1: nrow(X)) {
    if (X[i, 4] < thr.F4){
      score.f4[i] =  1} else {
        score.f4[i] = - 1
      }
  }
}
score.f5 = c()
if (mH[5,] > mL[5,]){
  for (i in 1: nrow(X)) {
    if (X[i, 5] > thr.F5){
      score.f5[i] = 1} else {
        score.f5[i] = -1
      }
  }
} else {
  for (i in 1: nrow(X)) {
    if (X[i, 5] < thr.F5){
      score.f5[i] =  1} else {
        score.f5[i] = - 1
      }
  }
}
score = cbind(score.f1, score.f2, score.f3, score.f4, score.f5)
fullscore = c(apply(score, 1, sum))

highmpg = function(A) {length(fullscore[fullscore >= A])}
lowmpg = function(A) {length(fullscore[fullscore <= -A])}
for (i in 1:10){
  classify = c(highmpg(i),lowmpg(i))
  print(classify)
}



#layout(matrix(c(1: 6), ncol = 3, nrow = 2, byrow = TRUE))
#for (i in 1: p) {
#  a = rnorm(1000, mean(X[,i]), sd(X[,i]))
#  plot(ecdf(a), main = "")
#  mtext(paste("CDF of",names(Auto[i+1])), 3)
#} #CDFs for X_1 fo X_5





