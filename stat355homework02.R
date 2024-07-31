# Macie Rose Wheeler
# Homework 2

### Question 1

hist(iris$Sepal.Width, main = 'Histogram of Sepal Width', xlab = 'Sepal Width')

# Based on the histogram I would expect the mean to be higher than the median
# since the histogram is slightly right-skewed.

mean(iris$Sepal.Width)
median(iris$Sepal.Width)

# Based on the actual calculations the mean is bigger than the median by .057333.

### Question 2

quantile(iris$Sepal.Width, .85)

# Only 15% of flowers have a sepal width higher than 3.5 cm.

### Question 3

pairs(iris[ , c(1:4)])

# Petal length and petal width appear to have the strongest relationship.
# Sepal length and sepal width appear to have the weakest relationship.

### Question 4

hist(PlantGrowth$weight, breaks = seq(from = 3.5, to = 6.5, by = .3), main =
       'Histogram of PlantGrowth Weight', xlab = 'PlantGrowth Weight')

### Question 5

boxplot(PlantGrowth$weight ~ PlantGrowth$group, ylab = 'PlantGrowth Weight', 
        xlab = 'PlantGrowth Group')

# Approximately 75% of "trt1" weights are below "trt2" weights.

### Question 6

mintrt2 <- min(PlantGrowth$weight[PlantGrowth$group == 'trt2'])

valuesBelowMin <- length(PlantGrowth$weight[PlantGrowth$group == 'trt1'
                                & PlantGrowth$weight < mintrt2])

values <- length(PlantGrowth$weight[PlantGrowth$group == 'trt1'])

valuesBelowMin / values

# 80% of the "trt1" weights are actually below "trt2" weights.

### Question 7

groupDataWithWeight <- table(PlantGrowth$group[PlantGrowth$weight > 5])
barplot(groupDataWithWeight, col = rainbow(3, s = .3), xlab = 'PlantGrowth Group', 
        main = 'Number of Plants per Group With Weight Greater Than 5')

### Question 8

dieRollSums <- (sample(1:6, 1000, replace = TRUE) + sample(1:6, 1000, replace = TRUE))
dieRollSums

dieRollSumsGreaterThan9 <- length(dieRollSums[dieRollSums >= 9])
dieRollSumsGreaterThan9

# 281 of my 1000 simulated sums were 9 or greater.
# There are 36 possible combinations of summing two dice and 10 of those are
# for sums that are 9 or greater. Therefore, the probability of getting sums of
# 9 or greater would be (10/36) = .2778 = 27.78%. That probability multiplied
# by the number of repetitions, 1000, is about 278. That number is the number
# of sums that I would expect to be 9 or higher.

### Question 9

# standard deviation of K
sqrt((100 * .01) * (1 - .01))
# standard deviation of C
sqrt((100 * .3) * (1 - .3))

# Dr. Keaton will have the more consistent number of bullseyes.

dbinom(x = 34, size = 100, prob = .3)
# P(C = 34) is .05788395

sum(dbinom(x = 25:32, size = 100, prob = .3))
# P(25 <= C <= 32) is .5971484

sum(dbinom(x = 1:100, size = 100, prob = .01))
# P(K >= 1) is .6339677

kprobs <- dbinom(x = 0:100, size = 100, prob = .01)
plot(y = kprobs, x = 0:100, type = 'h', lwd = 4, ylim = c(0, max(kprobs)), 
     xlab = 'Number of Times Dr. Keaton Hit a Bullseye', ylab = 'Probability')

cprobs <- dbinom(x = 0:100, size = 100, prob = .3)
plot(y = cprobs, x = 0:100, type = 'h', lwd = 4, ylim = c(0, max(cprobs)), 
     xlab = 'Number of Times Dr. Craig Hit a Bullseye', ylab = 'Probability')

# Dr. Keaton's graph is highly right-skewed.
# Dr. Craig's graph is right-skewed but not to the same extent as Dr. Keaton's.
# Dr. Craig's graph is somewhat symmetric.
# Dr. Keaton's graph is not unifrom or symmetric.