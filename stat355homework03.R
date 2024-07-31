# Macie Wheeler
# Homework 3

### Question 1

## Part a

pnorm(2300, mean = 2000, sd = 200)

# P(X < 2300 hours) is about 93.32%.

## Part b

pnorm(2322, mean = 2000, sd = 200) - pnorm(1856, mean = 2000, sd = 200)

# P(1856 < X < 2322) is about 71.05%.

## Part c

(1 - (pnorm(1970, mean = 2000, sd = 200))) ^ 2

# The probability that 2 bulbs last longer than 1970 hours is about 31.3%.

# Part d

qnorm(.25, mean = 2000, sd = 200)

# The first quartile of light bulb lifetimes is 1865.102 hours.

# Part e

qnorm(.96, mean = 2000, sd = 200)

# The best 4% of bulbs last at least 2350.137 hours.

## Part f

pnorm(12500, mean = (2000 * 6), sd = sqrt((200 ^ 2) * 6))

# P(Y < 12,500) is about 84.63%.

### Question 2

## Part a

n <- 100
p <- .01

n * p
n * (1 - p)

# It is not appropriate to use the Normal approximation to the Binomial here. 
# This is because np and n(1-p) are not both greater than 5.

## Part b

pnorm(2.5, mean = n * p, sd = sqrt(n * p * (1 - p)))

# The probability that Dr. Keaton hits the bullseye fewer than 3 times is about 93.42%.

## Part c

sum(dbinom(0:2, size = n, prob = p))

# The exact probability that Dr. Keaton hits the bullseye fewer than 3 times is about 92.1%.

### Question 3

## Part a

n <- 100
p <- .3

n * p
n * (1 - p)

# It is appropriate to use the Normal approximation to the Binomial here. 
# This is because np and n(1-p) are both greater than 5.

## Part b

pnorm(33.5, mean = n * p, sd = sqrt(n * p *(1 - p)))

# The probability that Dr. Keaton hits the bullseye fewer than 34 times is about 77.75%.

## Part c

sum(dbinom(0:33, size = n, prob = p))

# The exact probability that Dr. Keaton hits the bullseye fewer than 34 times is about 78%.

### Question 4

## Part a

shape1 <- 22
shape2 <- 2
x <- seq(0, 1, by = .02)

plot(dbeta(x, shape1, shape2))

# The shape of the probability density function curve of the Beta distribution is
# highly left-skewed.

## Part b

randoms <- rbeta(4, shape1, shape2)

# sample's mean
mean(randoms)
#sample's standard deviation
sd(randoms)

### Question 5

## Part a

shape1 <- 22
shape2 <- 2
count <- 1000
means <- rep(NA, count)

for (i in 1:count) {
  sample <- rbeta(4, shape1, shape2)
  means[i] <- mean(sample)
}

hist(means)

# The shape of the histogram is slightly left-skewed and looks somewhat symmetric.

## Part b

# I don't think that n = 4 is large enough to use the Central Limit Theorem reliably,
# since it's supposed to be used when n is greater than or equal to 30.

qqnorm(means)

# The Normal Q-Q plot isn't a completely straight line, so I don't think n is large
# enough to use the CLT reliably.

## Part c

# mean of the sample means
mean(means)
# standard deviation of the sample means
sd(means)

# Based on the CLT, I would expect the sample mean to be .92, and the sample 
# standard deviation to be .0532/sqrt(4) = .0266.

## Part d

1 - pnorm(.91, mean = .92, sd = (.0532 / sqrt(4)))

# The probability of getting a sample mean above .91, by the CLT, is about 64.7%.

# Number of my simulated sample means that were actually above .91.
length(means[means > .91])

### Question 6

## Part a

shape1 <- 22
shape2 <- 2
count <- 1000
means <- rep(NA, count)

for (i in 1:count) {
  sample <- rbeta(200, shape1, shape2)
  means[i] <- mean(sample)
}

hist(means)

# This histogram is a pretty symmetric, and is starting to look like a 
# Normal curve.

## Part b

# I think that n is large enough to use the CLT reliably since it is larger 
# than 30, which is the recommended "large enough" n value.

qqnorm(means)

# The Normal Q-Q plot is very linear, so I believe that n is large enough to 
# use the CLT reliably.

## Part c

# mean of the sample means
mean(means)
# standard deviation of the sample means
sd(means)

# Based on the CLT, I would expect the sample mean to be .92, and the sample 
# standard deviation to be .0532/sqrt(200) = .00376.

## Part d

1 - pnorm(.91, mean = .92, sd = (.0532 / sqrt(200)))

# The probability of getting a sample mean above .91, by the CLT, is about 99.6%.

# Number of my simulated sample means that were actually above .91.
length(means[means > .91])

### Question 7

## Part a

bootsize <- 10000
means <- rep(NA, bootsize)

for (i in 1:bootsize) {
  s <- sample(PlantGrowth$weight[PlantGrowth$group == 'trt2'], replace = T)
  means[i] <- mean(s)
}

hist(means)

## Part b

# 5th and 95th percentiles
quantile(means, c(.05, .95))

## Part c

m = PlantGrowth$weight[PlantGrowth$group == 'trt2']

t.test(x = m, conf.level = .9)$conf.int

# We are 90% confident that the true mean plant growth weight for population of 
# the trt2 group that this sample represents is between 5.269449 and 5.782551.