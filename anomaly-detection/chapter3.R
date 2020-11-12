library(car)
data(Davis)

N <- length(Davis$weight)
mu <- mean(Davis$weight)
si <- sd(Davis$weight) * sqrt((N - 1) / N)
kmo <- (mu / si) ^ 2
smo <- si ^ 2 / mu
ml <- fitdistr(Davis$weight, "gamma")
kml <- ml$estimate["shape"]
sml <- 1 / ml$estimate["rate"]

a <- Davis$weight / smo - (kmo - 1) * log(Davis$weight / smo)
th <- order(a, decreasing = T)[0.01 * N]
plot(a, ylab = "anomary score")
lines(0 : 200, rep(a[th], length(0 : 200)), col = "red", lty = 2)

pi0 <- 0.6
pi1 <- 0.4
mu0 <- 3
mu1 <- 0
sig0 <- 0.5
sig1 <- 3

N <- 1000
attr <- sample(0 : 1, N, replace = T, prob = c(pi0, pi1))
x <- rep(-99, N)
x[which(attr == 0)] <- rnorm(length(which(attr == 0)), mu0, sig0)
x[which(attr == 1)] <- rnorm(length(which(attr == 1)), mu1, sig1)

pi0 <- 0.5
pi1 <- 0.5
mu0 <- 5.0
mu1 <- -5.0
sig0 <- 1.0
sig1 <- 5.0


for (iteration in 1:10) {
  piN0 <- pi0 * dnorm(x, mu0, sig0)
  piN1 <- pi1 * dnorm(x, mu1, sig1)
  qn0 <- piN0 / (piN0 + piN1)
  qn1 <- piN1 / (piN0 + piN1)
  pi0 <- sum(qn0) / N
  pi1 <- sum(qn1) / N
  mu0 <- sum(qn0 * x) / (N * pi0)
  mu1 <- sum(qn1 * x) / (N * pi1)
  sig0 <- sqrt(sum(qn0 * (x - mu0) * (x - mu0)) / (N * pi0))
  sig1 <- sqrt(sum(qn1 * (x - mu1) * (x - mu1)) / (N * pi1))
}

library(car)
library(KernSmooth)
x <- Davis[, c("weight", "height")]
h <- c(dpik(x$weight), dpik(x$height))
est <- bkde2D(x, bandwidth = h, gridsize = c(10 ^ 3, 10 ^ 3))
d <- list(x = est$x1, y = est$x2, z = est$fhat)
image(d, col = terrain.colors(7), xlim = c(35, 110), ylim = c(145, 200))
contour(d, add = T)

aa <- colSums(K) - diag(K)
lowerLim <- 10 ^ (-20)
aa[(aa < lowerLim)] <- lowerLim
a <- (-1) * log(aa / (n - 1))
plot(a, xlab = "sample ID", ylab = "anomaly score")

install.packages("mclust")
library(mclust)
library(car)
X <- Davis[-12, c("weight", "height")]
result <- Mclust(X)
print(summary(result, parameters = TRUE))
plot(result)

pi <- result$parameters$pro
X <- Davis[, c("weight", "height")]
XX <- cdens(modelName = result$modelName, X, parameters = result$parameters)
a <- -log(as.matrix(XX) %*% as.matrix(pi))
plot(a, ylab = "anomary score")

install.packages("kernlab")
library(kernlab)
x <- rbind(matrix(rnorm(120), ncol = 2), matrix(rnorm(120, mean = 3), ncol = 2))
x <- scale(x)
rbf <- rbfdot(sigma = 0.5)
ocsvm <- ksvm(x, type = "one-svc", kernel = rbf, nu = 0.1)

colorcode <- rep(0, nrow(x))
colorcode[ocsvm@alphaindex] <- 1
plot(x, pch = 21, bg = colorcode)
