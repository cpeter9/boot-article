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

# install.packages("stringr")
library(stringr)

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

# Loop over small sample sizes
smallest.size <- 10
largest.size <- 100
size.by <- 10
boot.size <- 100

for(i in c(".sw", ".ks", ".ad", ".jb")){ # create fill vectors to put bootstapped p-values in
  norm <- rep(NA, boot.size)
  norm.wide <- rep(NA, boot.size)
  norm.skinny <- rep(NA, boot.size)
  uniform <- rep(NA, boot.size)
  chisq <- rep(NA, boot.size)
  gamma <- rep(NA, boot.size)
  
  if(i == ".sw") {p_fill <- as.data.frame(cbind(norm, norm.wide, norm.skinny, uniform, chisq, gamma))
                 names(p_fill) <- paste(names(p_fill), i, sep = "")} else
                 {temp <- as.data.frame(cbind(norm, norm.wide, norm.skinny, uniform, chisq, gamma))
                  names(temp) <- paste(names(temp), i, sep = "")
                  p_fill <- cbind(p_fill, temp)}
}


for(i in seq(smallest.size, largest.size, by = size.by)){
  sample.size <- i
  
  for(j in 1:boot.size){
  # Various normal distributions
  normal <- rnorm(sample.size, mean = 0, sd = 1)
  normal_wide <- rnorm(sample.size, mean = 0, sd = 3)
  normal_skinny <- rnorm(sample.size, mean = 0, sd = 0.5)
  
  # Other distributions
  uniform <- runif(sample.size, min = 0, max = 1)
  chisq <- rchisq(sample.size, df = 3)
  gamma <- rgamma(sample.size, shape = 2, rate = 2)
  
  # SW test
  p_fill$norm.sw[j] <- shapiro.test(normal)$p.value
  p_fill$norm.wide.sw[j] <- shapiro.test(normal_wide)$p.value
  p_fill$norm.skinny.sw[j] <- shapiro.test(normal_skinny)$p.value
  p_fill$uniform.sw[j] <- shapiro.test(uniform)$p.value
  p_fill$chisq.sw[j] <- shapiro.test(chisq)$p.value
  p_fill$gamma.sw[j] <- shapiro.test(gamma)$p.value
  
  # KS test
  p_fill$norm.ks[j] <- ks.test(normal, "pnorm", alternative = "two.sided")$p.value
  p_fill$norm.wide.ks[j] <- ks.test(normal_wide, "pnorm", alternative = "two.sided")$p.value
  p_fill$norm.skinny.ks[j] <- ks.test(normal_skinny, "pnorm", alternative = "two.sided")$p.value
  p_fill$uniform.ks[j] <- ks.test(uniform, "pnorm", alternative = "two.sided")$p.value
  p_fill$chisq.ks[j] <- ks.test(chisq, "pnorm", alternative = "two.sided")$p.value
  p_fill$gamma.ks[j] <- ks.test(gamma, "pnorm", alternative = "two.sided")$p.value
  
  # AD test
  p_fill$norm.ad[j] <- ad.test(normal)$p.value
  p_fill$norm.wide.ad[j] <- ad.test(normal_wide)$p.value
  p_fill$norm.skinny.ad[j] <- ad.test(normal_skinny)$p.value
  p_fill$uniform.ad[j] <- ad.test(uniform)$p.value
  p_fill$chisq.ad[j] <- ad.test(chisq)$p.value
  p_fill$gamma.ad[j] <- ad.test(gamma)$p.value
  
  # AD test
  p_fill$norm.jb[j] <- jarque.bera.test(normal)$p.value
  p_fill$norm.wide.jb[j] <- jarque.bera.test(normal_wide)$p.value
  p_fill$norm.skinny.jb[j] <- jarque.bera.test(normal_skinny)$p.value
  p_fill$uniform.jb[j] <- jarque.bera.test(uniform)$p.value
  p_fill$chisq.jb[j] <- jarque.bera.test(chisq)$p.value
  p_fill$gamma.jb[j] <- jarque.bera.test(gamma)$p.value
  }
  
  if(sample.size == smallest.size) {
    temp <- as.data.frame(cbind(sample.size = sample.size,
                                p_fill))} else {
                                 temp <- rbind(temp, cbind(sample.size, p_fill))}
}

output <- melt(temp, "sample.size")
output$variable <- as.character(output$variable)

# seperate.tests
substrRight <- function(x, n){
  substr(x, nchar(x) - n + 1, nchar(x))
}

substrLeft <- function(x, n){
  substr(x, 1, nchar(x) - n)
}

output$test <- substrRight(output$variable, 2)
output$test <- substrRight(output$variable, 2)



ggplot(output, aes(x = sample.size, y = value, colour = variable)) +
  geom_smooth() +
  geom_hline(yintercept = 0.05, colour = "red")







