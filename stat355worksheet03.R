# Macie Wheeler
# Worksheet 3

###
# Cheryl is pretty good at playing Pokémon Go. Each time she encounters a
# Pokémon in the wild, there is a 92% chance she catches it. Cheryl wants to
# be the very best, so she doesn’t let the results of her previous Pokémon
# encounter affect the next one. While playing on Saturday, Cheryl will
# encounter 37 Pokémon.
###

### Question 1

# Plot the PMF of the number of Pokémon Cheryl will catch on Saturday.

n <- 37
p <- .92

probs <- dbinom(x = 0:n, size = n, prob = p)
plot(y = probs, x = 0:n, type = 'h', lwd = 4, ylim = c(0, max(probs)), 
     xlab = 'Number of Pokemon Caught', ylab = 'Probability')

### Question 2

# Find the expected number of Pokémon Cheryl will catch on Saturday.

n * p

### Question 3

# Find the standard deviation of the number of Pokémon Cheryl will catch on Saturday.

sqrt(n * p * (1 - p))

### Question 4

# Find the probability Cheryl catches exactly 32 Pokémon on Saturday.

dbinom(x = 32, size = n, prob = p)

### Question 5

# Find the probability Cheryl catches at most 35 Pokémon on Saturday.

pbinom(q = 35, size = n, prob = p)

sum(dbinom(x = 0:35, size = n, prob = p))

1 - sum(dbinom(x = 36:37, size = n, prob = p))

### Question 6

# Find the probability that between 3 and 6 (inclusive) Pokémon escape 
# from Cheryl on Saturday.

n <- 37
p <- 1 - .92

sum(dbinom(x = 3:6, size = n, prob = p))

# or

n <- 37
p <- .92
37 - (3:6)

sum(dbinom(x = 31:34, size = n, prob = p))

### Question 7

# Simulate Cheryl’s Saturday once. How many Pokémon did she catch in your simulation?

n <- 37
p <- .92

rbinom(1, size = n, prob = p)

### Question 8

# Simulate Cheryl’s Saturday 100 more times. What was the average number of 
# Pokémon she caught across your 100 simulations.

n <- 37
p < .92

saturdays <- rbinom(100, size = n, prob = p)
saturdays

hist(saturdays)

mean(saturdays)

### Question 9

# Repeat Question #7 5000 more times, and save the average from each time.
# Plot those 5000 averages in a histogram. What do you observe?

n <- 37
p < .92

means = rep(0, 5000)

for (i in 1:5000) {
  saturdays <- rbinom(100, size = n, prob = p)
  means[i] = mean(saturdays)
}

hist(means)
mean(means)
median(means)

###
# While playing on Sunday, Cheryl will encounter 74 Pokémon (twice as many as Saturday).
###

### Question 10

# Plot the PMF of the number of Pokémon Cheryl will catch on Sunday. How does
# that compare to Saturday’s PMF from Question #1?

n = 37
p = .92

par(mfrow=c(2,1))

probs <- dbinom(x = 0:n, size = n, prob = p)
plot(y = probs, x = 0:n, type = 'h', lwd = 4, ylim = c(0, max(probs)), xlim = c(0, 74),
     xlab = 'Number of Pokemon Caught', ylab = 'Probability', main = 'Saturday')

n = 74

probs = dbinom(x = 0:n, size = n, prob = p)
plot(y = probs, x = 0:n, type = 'h', lwd = 4, ylim = c(0, max(probs)), xlim = c(0, 74), 
     xlab = 'Number of Pokemon Caught', ylab = 'Probability', main = 'Sunday')

### Question 11

# Find the standard deviation of the number of Pokémon Cheryl catches on Sunday.
# How does that compare to your answer in #3?

n = 74
p = .92

sqrt(n * p * (1-p))

n = 37

sqrt(n * p * (1-p))

### Question 12

# Find the probability Cheryl catches exactly 64 Pokémon on Sunday. How does
# that compare to your answer in #4?

n = 37
dbinom(32, n, p)

n = 74
dbinom(64, n, p)

###
# Cheryl is hoping to catch a Snorlax this weekend. A Snorlax’s weight is a
# Normally distributed random variable with a mean of 460 kgand a standard
# deviation of 35 kg. Assume the weight of one Snorlax does not affect the
# weight of another Snorlax.
###

### Question 13

# Plot the distribution of Snorlaxweights.



### Question 14

# The middle 95% (approx.) of Snorlaxes weigh between _______kg and _______kg.



### Question 15

# Find the probability that the next Snorlax Cheryl catches weighs between
# 422 and 487 kg.



### Question 16

# Find the probability that the next Snorlax Cheryl catches weighs exactly 465.831kg.



### Question 17

# Doug is bragging to Cheryl that only 1% of Snorlaxes weigh more than his Snorlax.
# How much does Doug’s Snorlax weigh?



### Question 18

# Find the probability that both of the next 2 Snorlaxes Cheryl catches each
# weigh less than 400 kg.



### Question 19

# Find the probability that the total combined weight of the next 3 Snorlaxes
# Cheryl catches is more than 1500kg.



###
# Lastly, let’s look back at those Binomial samples you generated earlier
# (or if you didn’t save them, just make new ones).
###

### Question 20

# Make a Normal Q-Q plot of those 100 random Binomial values from Question #8.
# Do those look Normal?



### Question 21

# Make a Normal Q-Q plot of those 5000 averages from Question #9. Do those look Normal?


