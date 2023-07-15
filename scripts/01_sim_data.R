# https://stats.stackexchange.com/a/300242

set.seed(20)

# f2 <- function(x) 0.2 * x^11 * (10 * (1 - x))^6 +
  # 10 * (10 * x)^3 * (1 - x)^10
f2 <- function(x) 1 * x^11 * (10 * (1 - x))^6 +
  35 * (10 * x)^3 * (1 - x)^10

ysim <- function(n = 500, scale = 2) {
  x <- runif(n)
  e <- rnorm(n, 0, scale)
  f <- f2(x)
  y <- f + e
  data.frame(y = y, x2 = x, f2 = f)
}

df <- ysim()

plot(df$x2, df$y)
