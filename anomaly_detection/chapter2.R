install.packages("car")
library(car)
data(Davis)
Davis
hist(Davis$weight, xlim = c(35, 105), breaks = 14)

mu <- mean(Davis$weight)
s2 <- mean((Davis$weight - mu) ^ 2)
c(mu, s2)

a <- (Davis$weight - mu)^ 2 / s2
th <- qchisq(0.99, 1)
plot(a, xlab = "index", ylab = "anomaly score")
lines(0 : 200, rep(th, length(0 : 200)), col = "red", lty = 2)

X <- cbind(Davis$weight, Davis$height)
plot(X[, 1], X[, 2], pch = 16, xlab = "weight", ylab = "height")

mx <- colMeans(X)
Xc <- X - matrix(1, nrow(X), 1) %*% mx
Sx <- t(Xc) %*% Xc / nrow(X)
a <- rowSums((Xc %*% solve(Sx)) * Xc)
plot(a, xlab = "index", ylab = "anomaly score")
lines(0 : 200, rep(th, length(0 : 200)), col = "red", lty = 2)

library(MASS)
X <- road / road$drivers
X <- as.matrix((log(X[, -2] + 1)))
mx <- colMeans(X)
Xc <- X - matrix(1, nrow(X), 1) %*% mx
Sx <- t(Xc) %*% Xc / nrow(X)
a <- rowSums((Xc %*% solve(Sx)) * Xc) / ncol(X)
plot(a, xlab = "index", ylab = "anomaly score", ylim = c(-1, 30) / ncol(X))
lines(0 : 30, rep(1, length(0 : 30)), col = "red", lty = 2)

xc_prime <- Xc["Calif",]
SN1 <- 10 * log10(xc_prime ^ 2 / diag(Sx))
barplot(SN1)
