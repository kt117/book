library(MASS)
X <- UScrime[, -c(2, 16)]
M <- ncol(X)
y <- UScrime[, 16]
N <- length(y)
lambdas <- seq(0, 5, length = 50)
model <- lm.ridge(y ~ ., cbind(X, y), lambda = lambdas)
bestIdx <- which.min(model$GCV)
coefs <- coef(model)[bestIdx,]
lam <- model$lambda[bestIdx]
ypred <- as.matrix(X) %*% as.matrix(coefs[2 : 15]) + coefs[1]

sig2 <- (lam * sum(coefs[2 : 15] ^ 2) + sum(as.numeric(ypred) - y) ^ 2) / N
X_ <- t(scale(X, scale=F))
H <- t(X_) %*% solve(X_ %*% t(X_) + lam * diag(M), X_)
TrHN <- sum(diag(H)) / N
a <- (as.numeric(ypred) - y) ^ 2 / ((1 - TrHN) ^ 2 * sig2)
plot(a, xlab = "index", ylab = "anomaly score")
th <- sort(a)[N * (1 - 0.05)]
lines(0 : 50, rep(th, length(0 : 50)), col = "red", lty = 2)
