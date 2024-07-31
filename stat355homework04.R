# Macie Wheeler
# Homework 4

hw4 <- read.csv('/Users/maciewheeler/Downloads/hw4.csv')

### Question 1

data <- hw4$purdue
n <- length(data)

## Part a

alpha <- .1
t.test(x = data, conf.level = 1 - alpha)$conf.int

# We are 90% confident that the true mean number of hours all Purdue students
# spend on their mobile devices daily is between 3.3172 and 3.672366 hours.

# Based on this confidence interval it looks like Purdue's average differs from
# the national average of 3.7 hours since it's not in the confidence interval.

## Part b

# Changing the confidence level to 95% will make our confidence interval bigger.

alpha <- .05
t.test(x = data, conf.level = 1 - alpha)$conf.int

# We are 95% confident that the true mean number of hours all Purdue students
# spend on their mobile devices daily is between 3.280308 and 3.709258 hours.

## Part c

# A larger sample size would make the confidence interval from part b smaller.

## Part d

alpha <- .05
mu0 <- 3.7

t.test(x = data, conf.level = 1 - alpha, alternative = 'two.sided', mu = mu0)

# The null hypothesis is that Purdue's true mean daily mobile hours is equal to the 
# national average of 3.7 hours.
# The alternative hypothesis is that Purdue's true mean daily mobile hours differs
# from the national average of 3.7 hours.
# The p-value is .05983.
# With a p-value of .05983 that is greater than an alpha of .05 we would fail to 
# reject the null hypothesis. There is not sufficient enough evidence, at an 
# alpha of .05, that Purdue's mean daily mobile hours is not equal to the national
# average of 3.7 hours.

## Part e

# A larger sample size would decrease the p-value from part d.

## Part f

alpha <- .05
qt(c(alpha/2, 1 - alpha/2), df = n - 1)

# The critical values that would be used in part d are -2.073873 and 2.073873.

## Part g

wilcox.test(data, mu = 3.7)

# With a p-value of .02538 that is less than an alpha of .05 we would reject the null
# hypothesis. We have significant enough evidence that the distribution of the 
# number of hours all Purdue students spend on their mobile devices daily is not
# located at 3.7.
# This is different than what we concluded in part d, since we failed to reject the
# null hypothesis there.

### Question 2

purduedata <- hw4$purdue
iudata <- hw4$iu[!is.na(hw4$iu)]

## Part a

# An independent samples t-test would be most appropriate here, since the size
# of each of the two data columns is different, which wouldn't allow us to pair
# up the data values.

## Part b

alpha <- .1
t.test(x = purduedata, y = iudata, conf.level = 1 - alpha)$conf.int

# We are 90% confident that the true mean number of hours that all Purdue students
# spend on their mobile devices daily is between .7284399 and .1567317 hours less
# than Indiana University students.

# Because the difference doesn't include 0, I am 90% confident that there is a 
# significant difference between schools.

## Part c

alpha <- .05
t.test(x = iudata, y = purduedata, conf.level = 1 - alpha, alternative = 'greater')

# The null hypothesis is that there is not a difference between Purdue and Indiana
# University's true mean number of hours that their students spend on mobile devices
# daily.
# The alternative hypothesis is that Indiana University's true mean number of
# hours that their students spend on mobile devices daily is greater than Purdue's.
# The p-value is .006506.
# With a p-value of .006506, that is less than an alpha of .05, we can reject the 
# null hypothesis. We have sufficient enough evidence that the true mean number of 
# hours that Indiana University's students spend on mobile devices daily is greater 
# than Purdue's.

## Part d

wilcox.test(iudata, purduedata, alternative = 'greater')

# With a p-value of .00497 that is less than the alpha of .05 we would reject 
# the null hypothesis. We have significant evidence that the distribution of
# Indiana University's number of hours that their students spend on mobile devices daily
# is greater than Purdue University's.
# This is the same as what we concluded in part c, since we also rejected the
# null hypothesis.

### Question 3

onlinedata <- hw4$online[!is.na(hw4$online)]
localdata <- hw4$local[!is.na(hw4$local)]

## Part a

# A paired t-test would be appropriate here since both of our data columns have
# the same number of elements and can be paired up against each other.

## Part b

alpha <- .02
t.test(x = onlinedata, y = localdata, paired = TRUE, conf.level = 1 - alpha)$conf.int

# We are 98% confident that the online prices are between about $6.03 more and 
# about $0.12 less than the local retailer prices.
# There is not a significant difference between the online prices and the retail 
# prices since the interval includes 0.

## Part c

alpha <- .01
t.test(x = onlinedata, y = localdata, paired = TRUE, conf.level = 1 - alpha,
       alternative = 'less')

# The null hypothesis is that the true population mean price difference between
# the online prices vs. the local retailer prices is 0.
# The alternative hypothesis is that the true population mean price for online prices
# is lower than the true population mean for local retailer prices.
# The p-value is .01204.
# With a p-value of .01204, that is greater than the alpha value of .01, we would
# fail to reject the null hypothesis. We don't have sufficient enough evidence
# to show that the true population mean price for online prices is lower than
# the true population mean price for local retailer prices.

## Part d

alpha <- .01
wilcox.test(onlinedata, localdata, paired = TRUE, conf.level = 1 - alpha,
            alternative = 'less')

# With a p-value of .01546, that is greater than the alpha of .01, we would fail
# to reject the null hypothesis. We don't have sufficient enough evidence to show
# that the distribution of online prices is lower than the distribution of local
# prices.
# This is the same as what we concluded in part c, since we also failed to reject
# the null hypothesis.

### Question 4

vote <- 529
voters <- 1000

## Part a

alpha <- .1
prop.test(x = vote, n = voters, conf.level = 1 - alpha)$conf.int

# We are 90% confident that the true population proportion of votes for the candidate
# is between about .5024929 and .5553476. Since the interval does not include
# .50, we are 90% confident that the candidate has a majority.

## Part b

alpha <- .05
p0 <- .5

prop.test(x = vote, n = voters, p = p0, alternative = 'greater')

# The null hypothesis is that the true population proportion of votes for the 
# candidate is .5.
# The alternative hypothesis is that the true population proportion of votes for 
# the candidate is greater than .5, or a majority.
# The p-value is .03573.
# With a p-value of .03573 which is smaller than the alpha of .05, we can reject
# the null hypothesis. We have sufficient enough evidence that the true population
# proportion of votes for the candidate is greater than .5, or a majority.

## Part c

# The conclusions shouldn't agree since the alpha values are different. The alpha
# value in part a was .1 and the alpha value in part b was .05. Decreasing an 
# alpha value in return increases the width of a confidence interval. Therefore, 
# you can't always be sure that you'll get the same conclusion.

### Question 5

marvelyes <- 252
marvelfans <- 300
dcyes <- 112
dcfans <- 150
  
## Part a

alpha <- .05
prop.test(x = c(marvelyes, dcyes), n = c(marvelfans, dcfans), conf.level = 1 - alpha)$conf.int

# We are 95% confident that the proportion of Marvel's fans who said yes to coming
# to the Black Widow movie is between .00730744 and .17935923 higher than DC's fans.
# Since that interval does not include 0, it appears that there is a difference.

## Part b

alpha <- .05
prop.test(x = c(marvelyes, dcyes), n = c(marvelfans, dcfans), conf.level = 1 - alpha,
          alternative = 'two.sided')

# The null hypothesis is that there is no difference in population proportion between
# Marvel and DC's fans who are going to see Black Widow.
# The alternative hypothesis is that there is a difference in population proportion
# between Marvel and DC's fans who are going to see Black Widow.
# The p-value is .02466.
# With a p-value of .02466, which is less than alpha of .05, we can reject the 
# null hypothesis. We have sufficient enough evidence that there is a difference
# in population proportion between Marvel and DC's fans who are going to see Black
# Widow.