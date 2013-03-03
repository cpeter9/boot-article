# Christopher Peters
# EXST 7083
# Assessing normality in small sample sizes

# install.packages("nortest")
library(nortest)

# install.packages("boot")
library(boot)

# install.packages("tseries")
library(tseries)

library(reshape2)
library(ggplot2)

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

sw_test <- function(data, indices){
  return(shapiro.test(data[indices])$p.value)
} 

ad_test <- function(data, indices){
  return(ad.test(data[indices])$p.value)
} 

ks_test <- function(data, indices){
  return(ks.test(data[indices], "pnorm", alternative = "two.sided")$p.value)
} 

jb_test <- function(data, indices){
  return(jarque.bera.test(data[indices])$p.value)
}


norm.ps <- boot(data = normal, statistic = sw_test, R = 100, n = sample.size)$t
gamma.ps <- boot(data = gamma, statistic = sw_test, R = 100, n = sample.size)$t

boot(data = normal, statistic = jb_test, R = 100, n = sample.size)

# Loop over small sample sizes
smallest.size <- 10
largest.size <- 1000
size.by <- 10
boot.size <- 100
for(i in seq(smallest.size, largest.size, by = size.by)){
  set.seed(i)
  sample.size <- i
  
  # Various normal distributions
  normal <- rnorm(sample.size, mean = 0, sd = 1)
  normal_wide <- rnorm(sample.size, mean = 0, sd = 3)
  normal_skinny <- rnorm(sample.size, mean = 0, sd = 0.5)
  
  # Other distributions
  uniform <- runif(sample.size, min = 0, max = 1)
  chisq <- rchisq(sample.size, df = 3)
  gamma <- rgamma(sample.size, shape = 2, rate = 2)
  
  # SW test
  norm.sw <- boot(data = normal, statistic = sw_test, R = boot.size)$t
  norm.wide.sw <- boot(data = normal_wide, statistic = sw_test, R = boot.size)$t
  norm.skinny.sw <- boot(data = normal_skinny, statistic = sw_test, R = boot.size)$t
  uniform.sw <- boot(data = uniform, statistic = sw_test, R = boot.size)$t
  chisq.sw <- boot(data = chisq, statistic = sw_test, R = boot.size)$t
  gamma.sw <- boot(data = gamma, statistic = sw_test, R = boot.size)$t
  
  if(sample.size == smallest.size) {
    temp <- as.data.frame(list(sample.size = sample.size,
                               norm.sw = norm.sw, 
                               norm.wide.sw = norm.wide.sw, 
                               uniform.sw = uniform.sw, 
                               chisq.sw = chisq.sw, 
                               gamma.sw = gamma.sw))} else {
    temp <- rbind(temp, as.data.frame(list(sample.size = sample.size,
                               norm.sw = norm.sw, 
                               norm.wide.sw = norm.wide.sw, 
                               uniform.sw = uniform.sw, 
                               chisq.sw = chisq.sw, 
                               gamma.sw = gamma.sw)))}
}


output <- melt(temp, "sample.size")

ggplot(output, aes(x = sample.size, y = value, colour = variable)) +
  geom_smooth() +
  geom_hline(yintercept = 0.05, colour = "red")


aggregate(output$value > 0.05, list(output$variable), table)

B <- 100
p_vals <- rep(NA, B)
for(i in 1:B){
  normal <- rnorm(150, mean = 0, sd = 1)
  p_vals[i] <- shapiro.test(normal)$p.value
}


output <- aggregate(output$value, list(output$variable, output$sample.size), mean)
  names(output) <- c("dist.test", "sample.size", "value")

ggplot(output, aes(x = sample.size, y = value, colour = dist.test)) +
  geom_smooth() +
  geom_hline(yintercept = 0.05, colour = "red")


ggplot(output, aes(x = value, fill = variable)) +
  geom_density(alpha = 0.2) +
  geom_vline(xintercept = 0.05, colour = "red")

table(output$variable, output$value > 0.05)



