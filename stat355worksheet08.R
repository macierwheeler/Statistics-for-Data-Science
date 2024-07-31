# Macie Wheeler

## For the next several questions, we’ll be building a regression model to predict
## Wind based on Temp.

### Question 4

# Make a scatterplot of the two variables with the regression line added.

attach(airquality)

plot(x = Temp, y = Wind, pch = 16)

wt <- lm(Wind ~ Temp)
names(wt)

abline(wt, col = 'red', lwd = 3)

cor(Wind, Temp)

### Question 5

# Interpret the slope of the regression line. If it makes sense, also interpret
# the intercept.

# The slope interpreted is that as the temperature increases by 1 degree the 
# wind speed decreases by -.1705.

### Question 6

# Do you have enough evidence at alpha = .05 to conclude that the population
# regression line has a non-zero slope?

cor.test(Wind, Temp)

summary(wt) # use the p-value for the slope (the non intercept variable)

anova(wt)

### Question 7

# Interpret this model’s R^2.

summary(wt)$r.squared

cor(Wind, Temp)^2

### Question 8

# Visually check the model’s assumptions.

plot(wt)

# first is to check constant variance, mean of 0, and independence
# second is to check normality

### Question 9

# Interpret this model’s s (model standard deviation).

summary(wt) # look at residual standard error

### Question 10

# Find and interpret both a 90% confidence interval and a 90% prediction interval
# corresponding to a Temp of 70 degrees.

predict(wt, data.frame(Temp = 70), interval = 'predict', level = .90) # for one day (interval is the lwr to the upr)

predict(wt, data.frame(Temp = 70), interval = 'confidence', level = .90) # for all days (interval is the lwr to the upr)
