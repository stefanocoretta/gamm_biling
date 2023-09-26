# https://stats.stackexchange.com/a/300242
library(tidyverse)

set.seed(20)

# f2 <- function(x) 0.2 * x^11 * (10 * (1 - x))^6 +
  # 10 * (10 * x)^3 * (1 - x)^10
f2 <- function(x, o = 1, p = 35) {
  o * x^11 * (10 * (1 - x))^6 +
  p * (10 * x)^3 * (1 - x)^10
}

ysim <- function(n = 500, scale = 2, o = 1, p = 35) {
  x <- runif(n, max = 0.65)
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



# JVC attempt -----------------------------------------------------------------

# Cross-sectional design (Based on Lightbrown's study of French Canadian
# learners of English)
# - test 100 learners on -ing production based on proficiency
# - low proficiency score higher because they generalize -ing to all forms
# - med proficiency score lower because they begin using -ing and present for
#   progressive
# - high proficiency score highest because they learn to only use -ing for
#   progressive

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

# Proficiency * time ----

set.seed(4785)

n <- 100
timep <- 11
proficiency <- rep(runif(n, -2, 2), timep)
time_b <- rnorm(100, mean = 0.02, sd = 0.01)
proficiency <- proficiency + proficiency * time_b * seq(0:10) + rnorm(n, 0, .5)

dat <- tibble(
  beta_0 = 15, # slope controls range of scores
  proficiency,
  score = beta_0 + proficiency^2 + rnorm(n, 0, .5) # gen. parabola
) %>%
  mutate(score = score / max(score) * 100) %>% # scale to 0-100
  # no perfect scores at low proficiency (generalization stragegy isn't 100% accurate)
  mutate(score = if_else(proficiency < -1, .$score - 10, .$score - 5))

plot(x = dat$proficiency, y = dat$score)


# Second attempt

set.seed(4785)
subjs <- 100
proficiency <- runif(subjs, -2, 2)
proficiency_b <- rnorm(subjs, 0, 0.1)
timep <- 11
intercept <- 15

proficiency_subjs <- list()
score_subjs <- list()

for (subj in 1:subjs) {
  proficiency_subj <- proficiency[subj] + proficiency[subj] * proficiency_b[subj] * 0:10
  score_subj <- intercept + proficiency_subj^2 + rnorm(timep, 0, .5)

  proficiency_subjs[subj] <- list(proficiency_subj)
  score_subjs[subj] <- list(score_subj)
}

dat <- tibble(
  score = unlist(score_subjs),
  proficiency = unlist(proficiency_subjs),
  subj = as.factor(paste("s", rep(1:subjs, each = timep), sep = "")),
  time = rep(0:10, length.out = 1100)
)

dat %>%
  ggplot(aes(proficiency, score, group = subj)) +
  geom_point()


library(mgcv)

gam_1 <- gam(
  score ~ s(proficiency) + s(time) + s(subj, bs = "re"),
  data = dat
)

plot(gam_1, select = 1)
plot(gam_1, select = 2)
plot(gam_1, select = 3)
