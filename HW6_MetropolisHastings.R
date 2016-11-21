rw.Metropolis <- function(n, sigma, x0, N, k, dis, b){
  x <- numeric(N)
  p <- numeric(N-1)
  x[1] <- x0
  u <- runif(N)
  f <- 0
  for (i in 2:N){
    if(dis == 0){
      y <- rnorm(1, x[i-1], sigma)
    } else if (dis == 1){
      y <- runif(1, x[i-1]-sigma, x[i-1]+sigma)
    }
    p[i-1] <- min(dt(y,n) / dt(x[i-1], n), 1)
    if(u[i] <= p[i-1]){
      x[i] <- y
    } else {
      x[i] <- x[i-1]
      if(i >= b){
        f <- f+1
      }
    }
  }
  
  x <- x[b:N]
  p <- p[b:(N-1)]
  #autocorrelation
  ac <- numeric(k)
  a <- mean(x)
  den <- sum((x-a)^2)
  for(i in 1:k){
    nu <- 0.0
    for(j in 1:(N-b+1-i)){
      nu <- nu + (x[j] - a)*(x[j+i] - a)
    }
    ac[i] <- nu/den
  }
  return(list(x=x, f=f/(N-b+1), p=p, ac=ac))
}

set.seed(1000)
n <- 5
N <- 25000
k <- 50
b <- 5001
sigma <- c(1, sqrt(.6), sqrt(.2), 1)

x0 <- 25
rw1 <- rw.Metropolis(n, sigma[1], x0, N, k, 0, b)
rw2 <- rw.Metropolis(n, sigma[2], x0, N, k, 0, b)
rw3 <- rw.Metropolis(n, sigma[3], x0, N, k, 0, b)
rw4 <- rw.Metropolis(n, sigma[4], x0, N, k, 1, b)

#Mean
avg <- c(mean(rw1$x), mean(rw2$x), mean(rw3$x), mean(rw4$x))
print(avg)

#Acceptance Rate
ar <- c(mean(rw1$p), mean(rw2$p), mean(rw3$p), mean(rw4$p))
print(ar)
#Rejection Rate
print(c(rw1$f, rw2$f, rw3$f, rw4$f))

#Autocorrelation
attach(mtcars)
par(mfrow=c(2,2))
plot(rw1$ac, main="norm, mean = 0, var = 1", xlab="lag", ylab="ACF", ylim=c(0,1))
plot(rw2$ac, main="norm, mean = 0, var = 0.6", xlab="lag", ylab="ACF", ylim=c(0,1))
plot(rw3$ac, main="norm, mean = 0, var = 0.2", xlab="lag", ylab="ACF", ylim=c(0,1))
plot(rw4$ac, main="unif, -1, 1", xlab="lag", ylab="ACF", ylim=c(0,1))

#Quantiles Q-Q plot
a <- ppoints(100)
QT <- qt(a, n)
Q1 <- quantile(rw1$x, a)
Q2 <- quantile(rw2$x, a)
Q3 <- quantile(rw3$x, a)
Q4 <- quantile(rw4$x, a)
frame()
attach(mtcars)
par(mfrow=c(2,2))
qqplot(QT, Q1, main="norm, mean = 0, var = 1",
       xlab="t Quantiles", ylab="Sample Quantiles")
qqplot(QT, Q1, main="norm, mean = 0, var = 0.6",
       xlab="t Quantiles", ylab="Sample Quantiles")
qqplot(QT, Q1, main="norm, mean = 0, var = 0.2",
       xlab="t Quantiles", ylab="Sample Quantiles")
qqplot(QT, Q1, main="unif, -1, 1",
       xlab="t Quantiles", ylab="Sample Quantiles")


rw.MetropolisThin <- function(n, sigma, x0, N, k, dis, b, t){
  x <- numeric(N)
  p <- numeric(N-1)
  x[1] <- x0
  u <- runif(N)
  f <- 0
  for (i in 2:N){
    if(dis == 0){
      y <- rnorm(1, x[i-1], sigma)
    } else if (dis == 1){
      y <- runif(1, x[i-1]-sigma, x[i-1]+sigma)
    }
    p[i-1] <- min(dt(y,n) / dt(x[i-1], n), 1)
    if(u[i] <= p[i-1]){
      x[i] <- y
    } else {
      x[i] <- x[i-1]
    }
  }
  
  x <- x[b:N]
  x <- x[seq(1, length(x), t)]
  p <- p[b:(N-1)]
  p <- p[seq(1, length(p), t)]
  #autocorrelation
  ac <- numeric(k)
  a <- mean(x)
  den <- sum((x-a)^2)
  for(i in 1:k){
    nu <- 0.0
    for(j in 1:(length(x)-i)){
      nu <- nu + (x[j] - a)*(x[j+i] - a)
    }
    ac[i] <- nu/den
  }
  return(list(x=x, p=p, ac=ac))
}
t <- 10
rw4 <- rw.MetropolisThin(n, sigma[4], x0, N*t, k, 1, b, t)
plot(rw4$ac, main="unif, -1, 1, thinning", xlab="lag", ylab="ACF", ylim=c(0,1))
