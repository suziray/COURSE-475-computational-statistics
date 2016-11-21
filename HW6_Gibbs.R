set.seed(1000)
N <- 25000
burn <- 5001
n <- 16
a <- 2
b <- 4
x0 <- 10
y0 <- .5

X <- matrix(0, N, 2)
X[1,] <- c(x0, y0)
for(i in 2:N){
  X[i, 1] <- rbinom(1, n, X[i-1, 2])
  X[i, 2] <- rbeta(1, X[i-1, 1] + a, n - X[i-1, 1] + b)
}

b <- burn + 1
x <- X[b:N,]

colMeans(x)
var(x[,1])
var(x[,2])

y <- rbeta(20000,2,4)
x2 <- numeric(20000)
for(i in 1:20000){
  x2[i] <- rbinom(1,16,y[i])
}
mean(x2)
var(x2)

a <- ppoints(100)
Q1 <- quantile(x[,1], a)
Q2 <- quantile(x2, a)
qqplot(Q1, Q2, main="", xlab="(a) Quantiles", ylab="(c) Quantiles")
