# Christopher Peters
# EXST 7083
# Assessing normality in small sample sizes

# install.packages("nortest")
library(nortest)

# install.packages("boot")
library(boot)

# install.packages("tseries")
library(tseries)

# Suppose

sample.size <- 16

# Various normal distributions
normal <- rnorm(sample.size, mean = 0, sd = 1)
normal_wide <- rnorm(sample.size, mean = 0, sd = 3)
normal_skinny <- rnorm(sample.size, mean = 0, sd = 0.5)

# Other distributions
uniform <- runif(sample.size, min = 0, max = 1)
chisq <- rchisq(sample.size, df = 3)
gamma <- rgamma(sample.size, shape = 2, rate = 2)

# Tests
# 1. SW
# 2. KS
# 3. AD
# 4. JB

shapiro.test(normal)
shapiro.test(normal)
ks.test(normal, "pnorm", alternative = "two.sided")
ad.test(normal)
jarque.bera.test(normal)

sw_test <- function(data, indices, n){
  d <- data[indices[1:n]]
  return(shapiro.test(d[indices])$p.value)
} 

ad_test <- function(data, indices, n){
  d <- data[indices[1:n]]
  return(ad.test(d[indices])$p.value)
} 

ks_test <- function(data, indices, n){
  d <- data[indices[1:n]]
  return(ks.test(d[indices], "prnom", alternative = "two.sided")$p.value)
} 

jb_test <- function(data, indices, n){
  d <- data[indices[1:n]]
  return(ks.test(d[indices], "prnom", alternative = "two.sided")$p.value)
}


norm.ps <- boot(data = normal, statistic = sw_test, R = 100, n = sample.size)$t
gamma.ps <- boot(data = gamma, statistic = sw_test, R = 100, n = sample.size)$t






