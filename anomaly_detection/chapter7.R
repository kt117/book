install.packages("FNN")
library(FNN)
X <- read.table(file = "sandbox_r/qtdbsel102.txt")
w <- 100
nk <- 1
Xtr <- X[1 : 3000, 2]
Dtr <- embed(Xtr, w)
X <- X[3001 : 6000, 2]
D <- embed(X, w)
d <- knnx.dist(Dtr, D, k = nk)
a <- d[, 1]
plot(a, ylab = "anomaly score", type = "l")

dt <- read.table(file = "sandbox_r/qtdbsel102.txt")
xi <- dt[3001 : 6000, 2]
w <- 50
m <- 2
k <- w / 2
L <- k / 2
Tt <- length(xi)
score <- rep(0, Tt)

for(t in (w + k) : (Tt - L + 1)){
  tstart <- t- w - k + 1
  tend <- t - 1
  X1 <- t(embed(xi[tstart : tend], w))
  X1 <- X1[w : 1,]
  
  tstart <- t- w - k + 1 + L
  tend <- t - 1 + L
  X2 <- t(embed(xi[(tstart) : (tend)], w))
  X2 <- X2[w : 1,]
  
  U1 <- svd(X1)$u[, 1 : m]
  U2 <- svd(X2)$u[, 1 : m]
  sig1 <- svd(t(U1) %*% U2)$d[1]
  score[t] <- 1 - sig1 ^ 2
}

set.seed(1)
tt <- 0.1
x1 <- seq(0, 10, by = tt)
x2 <- seq(10.1, 20, by = tt)
x3 <- seq(20.2, 30, by = tt)
y1 <- sin(pi * x1) + rnorm(length(x1), sd = 0.07)
y2 <- sin(2 * pi * x2) + rnorm(length(x2), sd = 0.07)
y3 <- sin(pi * x3) + rnorm(length(x3), sd = 0.07)
xi <- c(y1, y2, y3)

Dtr <- nottem[1 : 120]
xi <- nottem[121 : 240]
Tt <- length(xi)
ar.model <- ar(Dtr)
print(ar.model)
r <- ar.model$order
alpha <- ar.model$ar
xmean <- ar.model$x.mean
sig2 <- ar.model$var.pred
N <- Tt - r
X <- t(embed(xi - xmean, r))[, 1 : N]
ypred <- t(X) %*% alpha + xmean
y <- xi[(1 + r) : Tt]
a <- (y - as.numeric(ypred)) ^ 2 / sig2

