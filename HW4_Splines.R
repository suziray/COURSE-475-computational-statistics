# Generate data in the form of a sine wave
set.seed(1)
n <- 1e2
noise <- rgamma(n=n, shape = 6, rate = 100)
dat <- data.frame(
  x = 1:n,
  y = sin(seq(0, 5*pi, length.out = n)) + noise - mean(noise)
)
approxData <- data.frame(
  with(dat, approx(x, y, xout = seq(1, n, by = 10), method = "linear")),
  method = "approx()"
)
splineData <- data.frame(
  with(dat, spline(x, y, xout = seq(1, n, by = 10))),
  method = "spline()"
)
smoothData <- data.frame(
  x = 1:n,
  y = as.vector(smooth(dat$y)),
  method = "smooth()"
)
loessData <- data.frame(
  x = 1:n,
  y = predict(loess(y~x, dat, span = 0.1)),
  method = "loess()"
)

library(ggplot2)
ggplot(rbind(approxData, aes(x, y)) + 
  geom_point(dat = dat, aes(x, y), alpha = 0.2, col = "red") +
  geom_line(col = "blue") +
  facet_wrap(~method) +
  ggtitle("Interpolation and smoothing functions in R") +
  theme_bw(16)
