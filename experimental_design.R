## Author: Christopher Peters
## Email: peters@teamtreehouse.com
## TeamTreeHouse.com
## To simulate experiments and determine power


# Suppose we want to analyze differences in average badge earning

# Lets find the distribution of badge earning in the first two weeks

# Establish connection to SQL database
drv <- dbDriver("MySQL")
connection <- dbConnect(drv, 
                        db = "vitamin_production",
                        user = "treehouse",
                        password = "u4mMibBhwxNp6Z",
                        host = "treehouse-mysql-stats-2.cugxjxwyegle.us-east-1.rds.amazonaws.com")

# Find badges earned by each user within two weeks of signing up
data <- dbGetQuery(connection, "select person_badges.user_id, person_badges.badge_id, person_badges.created_at, users.email, badges.name, badges.image
                  from person_badges LEFT JOIN users on users.id = person_badges.user_id
                    LEFT JOIN badges on badges.id = person_badges.badge_id
                  where person_badges.created_at > DATE_SUB(NOW(), INTERVAL 1 WEEK)")

# Look at histogram of badges earned, right-tailed, OK, but what does a random sampled mean look like, t-distributed?
hist(table(data$user_id))

write.csv(data, file = "C:/R_stuff/school/article/data.csv")

sample_a <- sample(table(data$user_id), 100, replace = TRUE)
sample_b <- sample(table(data$user_id), 100, replace = TRUE)

raw.samples <- as.data.frame(t(rbind(sample_a, sample_b)))
raw.samples <- melt(raw.samples)

library(ggplot2)
ggplot(raw.samples, aes(x = value)) +
  geom_histogram(aes(y = ..density..)) +
  facet_grid(variable ~ . )
  stat_function(fun = dt,
                args = c(df = 100,
                         ncp = mean(sample_diff$sample_diff)),
                colour = "red") +
  stat_function(fun = dnorm,
                args = c(mean = mean(sample_diff$sample_diff),
                         sd = sd(sample_diff$sample_diff)),
                colour = "blue") +
  ggtitle("Histogram of mean badges earned within two weeks of membership\nplotted against theoretical t-distribution (red) and normal distribution (blue)")


# Take differences from B sample groups
B <- 200
t_p_vals <- rep(NA, B)
wmw_p_vals <- rep(NA, B)

begin <- 10; end <- 200; move_by <- 10
t_type_1 <- rep(NA, length(seq(begin, end, move_by)))
wmw_type_1 <- rep(NA, length(seq(begin, end, move_by)))


for(j in seq(begin, end, move_by)){
for(i in 1:B){
  # sample_a <- sample(table(data$user_id), sample.size, replace = TRUE)
  # sample_b <- sample(table(data$user_id), sample.size, replace = TRUE)  
  
#   sample_a <- rchisq(sample.size, 2, ncp = 1)
#   sample_b <- rchisq(sample.size, 2, ncp = 1)
#   
  sample_a <- rnorm(sample.size, mean = 0, sd = 1)
  sample_b <- rnorm(sample.size, mean = 0, sd = 1)
  
  sample_a <- rnorm(sample.size, mean = 0, sd = 1)
  sample_b <- rnorm(sample.size, mean = 0, sd = 1)
  
  t_p_vals[i] <- t.test(sample_a, sample_b, alternative = "two.sided", var.equal = TRUE, conf.level = 0.95)$p.value
  wmw_p_vals[i] <- wilcox.test(sample_a, sample_b, alternative = "two.sided", conf.level = 0.95)$p.value
  }
t_type_1[j/move_by] <- ifelse(is.na(table(t_p_vals <= 0.05)[2]), 0, table(t_p_vals <= 0.05)[2]) / B
wmw_type_1[j/move_by] <- ifelse(is.na(table(wmw_p_vals <= 0.05)[2]), 0, table(wmw_p_vals <= 0.05)[2]) / B
}

output <- as.data.frame(list(t_test = t_type_1, wmw_test = wmw_type_1))
output <- melt(output)
output$sample.size <- seq(begin, end, move_by)


ggplot(output, aes(x = sample.size, y = value, colour = variable)) +
  geom_smooth() +
  geom_point()



# Convert badge means to a data.frame
sample_diff <- as.data.frame(sample_diff)
sample_sd <- as.data.frame(sample_sd)


# Now lets check if it's t-distributed centered on mean(badge.means), looks normal distributed or t-dist with infinite DF
t <- rt(10000, df = sample.size, ncp = mean(sample_diff))
chi <- rt(10000, df = mean(sample_sd^2))

library(ggplot2)
ggplot(sample_diff, aes(x = sample_diff)) +
  geom_histogram(aes(y = ..density..)) +
  stat_function(fun = dt,
                args = c(df = sample.size,
                         ncp = mean(sample_diff$sample_diff)),
                colour = "red") +
  stat_function(fun = dnorm,
                args = c(mean = mean(sample_diff$sample_diff),
                         sd = sd(sample_diff$sample_diff)),
                colour = "blue") +
  ggtitle("Histogram of mean badges earned within two weeks of membership\nplotted against theoretical t-distribution (red) and normal distribution (blue)")


ggplot(sample_sd, aes(x = sample_sd)) +
  geom_histogram(aes(y = ..density..)) +
  stat_function(fun = dchisq,
                args = c(df = 1.71),
                colour = "red") +
  ggtitle("Histogram of mean badges earned within two weeks of membership\nplotted against theoretical t-distribution (red) and normal distribution (blue)")


# Take B samples and calculate mean
B <- 1000
sample.size <- 30
badge.means <- rep(NA, B)

for(i in 1:B){
  badge.sample <- sample(table(data$user_id), sample.size, replace = TRUE)
  badge.means[i] <- mean(badge.sample)
  badge.
}

# Convert badge means to a data.frame
badge.means <- as.data.frame(badge.means)

# Now lets check if it's t-distributed centered on mean(badge.means), looks normal distributed or t-dist with infinite DF
t <- rt(1000, df = sample.size, ncp = mean(badge.means))

library(ggplot2)
ggplot(badge.means, aes(x = badge.means)) +
  geom_histogram(aes(y = ..density..)) +
  stat_function(fun = dt,
                  args = c(df = sample.size,
                           ncp = mean(badge.means$badge.means)),
                           colour = "red") +
  stat_function(fun = dnorm,
                args = c(mean = mean(badge.means$badge.means),
                         sd = sd(badge.means$badge.means)),
                colour = "blue") +
  ggtitle("Histogram of mean badges earned within two weeks of membership\nplotted against theoretical t-distribution (red) and normal distribution (blue)")

# Now that we've established that the mean of these variables behave t-distributed, we can use
# a t-test with no problem, next we'll be testing the power of the t-test against a chi-square test.

# a. what is the power given a sample of 50 and alpha = 0.05 using t-test
B <- 1000
ct <- rep(NA, B)

sample.size <- 50

for(i in 1:B){
  set.seed(i) # need to change seed on each bootstrapped draw
  x <- rt(sample.size, df = 1000) + mean(badge.means$badge.means) # centered on average mean of sample
  y <- rt(sample.size, df = 1000) + (mean(badge.means$badge.means) * 1.1) # centered on average mean of sample + 10%
  
  ct[i] <- t.test(y, x, alternative = "greater", conf.level = 0.95)$p.value
}

# Calculate true power of test to reject null that they're from the same population
true.power <- table(ct < 0.05)[2] / B
true.power # 87% power!

# So how are we using the pwr.t.test function wrong?
library(pwr)
pwr.t.test(d = 0.5852387 , n = 50, sig.level = 0.05, type="two.sample", alternative = "greater") # EFFECT IS IN ABS VALUE NOT PCT!
# Power given as 89%!!!

# b. what is the power given a sample of 50 and alpha = 0.05 using chi-square test
B <- 1000
ct <- rep(NA, B)

sample.size <- 50

for(i in 1:B){
  x <- sample(table(data$user_id), sample.size, replace = TRUE) # centered on average mean of sample
  y <-  x * 1.1

  ct[i] <- chisq.test(y, x, simulate.p.value = TRUE, B = 100)$p.value
}
  
# Calculate true power of test to reject null that they're from the same population
true.power <- table(ct < 0.05)[2] / B
true.power # 100% power!

table(ct > 0.05)[1] / B # Alpha registers at 0.053, pretty close!


# c. Test the alpha of 0.05 given a sample of 50 and alpha = 0.05 using t.test
B <- 1000
ct <- rep(NA, B)

sample.size <- 50

for(i in 1:B){
  set.seed(i) # need to change seed on each bootstrapped draw
  x <- rt(sample.size, df = 1000) + mean(badge.means$badge.means) # centered on average mean of sample
  y <- rt(sample.size, df = 1000) + mean(badge.means$badge.means) # centered on the same mean
  
  ct[i] <- t.test(y, x, alternative = "greater", conf.level = 0.95)$p.value
}

table(ct > 0.05)[1] / B # Alpha registers at 0.053, pretty close!




