library(MASS)
cc <- c(
  "Min.Price",
  "Price",
  "Max.Price",
  "MPG.city",
  "MPG.highway",
  "EngineSize",
  "Horsepower",
  "RPM",
  "Rev.per.mile",
  "Fuel.tank.capacity",
  "Length",
  "Wheelbase",
  "Width",
  "Turn.circle",
  "Weight"
  )
mask <- is.element(colnames(Cars93), cc)
Xc <- t(scale(Cars93[, mask]))
colnames(Xc) <- t(Cars93[, "Make"])
S <- Xc %*% t(Xc)
evd <- eigen(S)
plot(evd$values, type = "b", xlab = "index", ylab = "eigenvalue")

m <- 2
x2 <- t(evd$vectors[, 1 : m]) %*% Xc
a1 <- colSums(Xc * Xc) - colSums(x2 * x2)
idx <- order(a1, decreasing = T)[1 : 6]
print(a1[idx])

G <- t(Xc) %*% Xc
evd <- eigen(G)
Lam_12 <- diag(evd$values[1 : m] ^ {- 1 / 2})
xx2 <- Lam_12 %*% t(evd$vectors[, 1 : m]) %*% t(Xc) %*% Xc
aa1 <- colSums(Xc * Xc) - colSums(xx2 * xx2)
idx <- order(aa1, decreasing = T)[1 : 3]
print(aa1[idx])

library(kernlab)
m <- 2
sig <- 0.1
li <- c(-6, 7)
kpc <- kpca(t(Xc), kernel = "rbfdot", kpar = list(sigma = sig), features = m)
Zt <- rotated(kpc)
plot(Zt[, 1], Zt[, 2], Xlab = "1st PC", ylab = "2nd PC", cex = 3, col = 3, xlim = li, ylim = li, main = sig)
text(Zt[, 1], Zt[, 2], c(1 : 93), cex = 0.8, xlim = li, ylim = li)
