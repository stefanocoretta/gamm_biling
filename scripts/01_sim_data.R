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


# JVC attempt -----------------------------------------------------------------

# Cross-sectional design (Based on Lightbrown's study of French Canadian
# learners of English)
# - test 100 learners on -ing production based on proficiency
# - low proficiency score higher because they generalize -ing to all forms
# - med proficiency score lower because they begin using -ing and present for
#   progressive
# - high proficiency score highest because they learn to only use -ing for
#   progressive
library("dplyr")

n <- 100

dat <- tibble(
  beta_0 = 15, # slope controls range of scores
  proficiency = runif(n, -2, 2), # suppose standardized prof. score
  score = beta_0 + proficiency^2 + rnorm(n, 0, .5) # gen. parabola
) %>%
  mutate(score = score / max(score) * 100) %>% # scale to 0-100
  # no perfect scores at low proficiency (generalization stragegy isn't 100% accurate)
  mutate(score = if_else(proficiency < -1, .$score - 10, .$score - 5))

plot(x = dat$proficiency, y = dat$score)
