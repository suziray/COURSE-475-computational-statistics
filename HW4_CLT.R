N <- 1000
x <- vector(length = N)
x1 <- vector(length = N)
n <- 15
# n<- 100
for(i in 1:N){
  samp <- rexp(n, rate = 1/7)
  #samp <- rweibell(n, shape=5, scale=1)
  x[i] <- mean(samp)
  x1[i] <- samp[1]
}
hist(x, main="Histogram of mean"))
hist(sqrt(n)*x, main="Histogram of sqrt(n)*x")
hist(sqrt(n)*(x-mean(x1))/sqrt(var(x1)), main="Histogram of sqrt(n)*(x-mean)/std")