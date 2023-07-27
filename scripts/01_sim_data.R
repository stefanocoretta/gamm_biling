# https://stats.stackexchange.com/a/300242

set.seed(20)

# f2 <- function(x) 0.2 * x^11 * (10 * (1 - x))^6 +
  # 10 * (10 * x)^3 * (1 - x)^10
f2 <- function(x, o = 1, p = 35) {
  o * x^11 * (10 * (1 - x))^6 +
  p * (10 * x)^3 * (1 - x)^10
}

ysim <- function(n = 500, scale = 2, o = 1, p = 35) {
  x <- runif(n)
  e <- rnorm(n, 0, scale)
  f <- f2(x, o, p)
  y <- f + e
  data.frame(y = y, x2 = x, f2 = f)
}

df <- ysim()

plot(df$x2, df$y, ylim = c(-5, 40))

# Changing p changes the first peak

df_2 <- ysim(p = 11)

plot(df_2$x2, df_2$y, ylim = c(-5, 40))

# Changing o changes the second peak

df_3 <- ysim(o = 2)

plot(df_3$x2, df_3$y, ylim = c(-5, 40))
