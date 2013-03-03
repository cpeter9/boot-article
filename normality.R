# Christopher Peters
# EXST 7083
# Assessing normality in small sample sizes

# install.packages("nortest")
library(nortest)

install.packages("boot")
library(boot)

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

sw_test <- function(data, indices) return(shapiro.test(data[indices])$p.value)

norm.ps <- boot(data = normal, statistic = sw_test, R = 100)$t
gamma.ps <- boot(data = gamma, statistic = sw_test, R = 100)$t






