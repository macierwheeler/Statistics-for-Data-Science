# Macie Wheeler
# Homework 5

### Question 1

## Part a

boxplot(iris$Sepal.Width ~ iris$Species, col= topo.colors(3), xlab = 'Species', 
        ylab = 'Sepal Width')

# The mean sepal width of Setosa seems to be significantly different from both
# Virginica and Versicolor's mean sepal width.
# The mean sepal width between Versicolor and Virginica seems to be slightly different.

## Part b

iris_anova <- aov(iris$Sepal.Width ~ iris$Species)
summary(iris_anova)

# There seems to be a significant difference in mean sepal widths across all species
# since with a p-value of 2e-16, which is smaller than .05, we would reject the
# null hypothesis that there is no difference in means since there is enough
# evidence to show that there is a difference in the means.

## Part c

pairwise.t.test(iris$Sepal.Width, iris$Species, p.adj = 'bonferroni', pool.sd = FALSE)$p.value

# Setosa and Versicolor, Setosa and Virginica, and Versicolor and Virginica seem 
# to all have significantly different means at an alpha of .05.

## Part d

var(iris$Sepal.Width[iris$Species == 'virginica'])
var(iris$Sepal.Width[iris$Species == 'setosa'])
var(iris$Sepal.Width[iris$Species == 'versicolor'])

# The variance for Virginica is .1040041, for Setosa the variance is .1436898, and
# for Versicolor the variance is .09846939. The variances seem to be relatively 
# similar across all 3 of the species groups.

qqnorm(iris_anova$residuals)
qqline(iris_anova$residuals)

# The residuals look relatively Normal as if they could have come from a Normal
# Distribution due to the relatively straight line for the Q-Q plot of the 
# residuals.

### Question 2

## Part a

boxplot(ChickWeight$weight[ChickWeight$Time == 21] ~ ChickWeight$Diet[ChickWeight$Time == 21], 
        xlab = 'Diet', ylab = 'Weight', col = rainbow(4))

# Diet 1 and Diet 3 seem to have a significant difference between their mean weights.
# Diet 2 and Diet 4 seem to have somewhat similar mean weight, so no significant difference.
# Diet 1 and Diet 4 also seem to have a difference between their mean weights, but
# not very significant.

## Part b

chick_weight_anova <- aov(ChickWeight$weight[ChickWeight$Time == 21] ~ as.factor(ChickWeight$Diet[ChickWeight$Time == 21]))
summary(chick_weight_anova)

# It seems like there is a significant difference in the means since with a p-value
# of .007, which is smaller than .05, we would reject the null hypothesis that
# there is not a difference in the means since there is enough evidence to show
# that there is a difference in the means.

## Part c

TukeyHSD(chick_weight_anova)

# Diet 3 and Diet 1 have a significant difference in means at an alpha of .05.

## Part d

var(ChickWeight$weight[ChickWeight$Diet == 1 & ChickWeight$Time == 21])
var(ChickWeight$weight[ChickWeight$Diet == 2 & ChickWeight$Time == 21])
var(ChickWeight$weight[ChickWeight$Diet == 3 & ChickWeight$Time == 21])
var(ChickWeight$weight[ChickWeight$Diet == 4 & ChickWeight$Time == 21])

# The variance for diet 1 is 3445.933, for diet 2 it is 6105.567, for diet 3 it 
# is 5129.789, and for diet 4 it is 1879.028. The variances don't look very similar
# across all 4 of the diet groups.

qqnorm(chick_weight_anova$residuals)
qqline(chick_weight_anova$residuals)

# The residuals look relatively Normal as if they could have come from a Normal
# Distribution due to the relatively straight line from the Q-Q plot of the 
# residuals.

### Question 3

## Part a

boxplot(warpbreaks$breaks ~ warpbreaks$wool*warpbreaks$tension, col = c(5, 5, 7, 7, 3, 3),
        xlab = 'Wool:Tension', ylab = 'Breaks')

# Within wool A the mean breaks is highest with a level L of tension and lowest with
# a level M of tension. There seems to be a significant difference between breaks means with
# a level L of tension and a level M of tension and between breaks means with a level L of 
# tension and a level H of tension.
# Within wool B the mean breaks is highest with a level L of tension and lowest with
# a level H of tension. There seems to be a slightly significant difference between
# breaks means with a tension level of L and a tension level of H and between breaks
# means with a tension level of M and a tension level of H.

## Part b

warpbreaks_twoway_anova <- aov(warpbreaks$breaks ~ warpbreaks$wool*warpbreaks$tension)
summary(warpbreaks_twoway_anova)

# The interaction between wool and tension is significant at an alpha of .1 with
# a p-value of .021044.
# Both the main effects of wool and tension are significant at an alpha of .1 with
# p-values of .058213 and .000693 respectively.

## Part c

interaction.plot(x.factor = warpbreaks$tension, trace.factor = warpbreaks$wool,
                 response = warpbreaks$breaks, col = (6:7), lwd = 3, type = 'b', 
                 pch = 1)

# The interaction plot shows that the effect of tension on breaks isn't relatively
# clear, since the lines decrease, increase, and stay relatively flat at points.
# The effect of wool on breaks isn't relatively clear either, since the lines aren't
# very separated.
# It seems as if wool depends on tension, or vice versa.

## Part d

warpbreaks_anova <- aov(warpbreaks$breaks ~ warpbreaks$wool)
summary(warpbreaks_anova)

# The results of the test show that there is not a significant difference in mean
# breaks across wool at an alpha of .1, which is smaller that a p-value of .108. 
# We would fail to reject the null since there is not significant evidence that
# the means are different across wool. This contradicts our previous results. In 
# order to reconcile this difference, other tests could potentially be used.

### Question 4

## Part a

cor(iris[1:4])

## Part b

# Petal length and petal width have the strongest linear relationship with an 
# r of .9628654. This means that the correlation between those two variables is 
# positive and the strength of the relationship is strong.

## Part c

# Sepal length and sepal width have the weakest linear relationship with an r
# of -.1175698. This means that the correlation between those two variables is
# negative and the strength of the relationship is weak.

## Part d

cor.test(iris$Sepal.Length, iris$Sepal.Width, conf.level = .01)
cor.test(iris$Sepal.Length, iris$Petal.Length, conf.level = .01)
cor.test(iris$Sepal.Length, iris$Petal.Width, conf.level = .01)

cor.test(iris$Sepal.Width, iris$Petal.Length, conf.level = .01)
cor.test(iris$Sepal.Width, iris$Petal.Width, conf.level = .01)

cor.test(iris$Petal.Length, iris$Petal.Width, conf.level = .01)

# Sepal length and petal length have a population correlation different from 0, 
# since with a p-value of 2.2e-16, which is less than an alpha of .01, there is
# significant enough evidence that sepal length and petal length are correlated in
# the population.

# Sepal length and petal width have a population correlation different from 0, 
# since with a p-value of 2.2e-16, which is less than an alpha of .01, there is
# significant enough evidence that sepal length and petal width are correlated in
# the population.

# Sepal width and petal length have a population correlation different from 0, 
# since with a p-value of 4.513e-08, which is less than an alpha of .01, there is
# significant enough evidence that sepal width and petal length are correlated in
# the population.

# Sepal width and petal width have a population correlation different from 0, 
# since with a p-value of 4.073e-06, which is less than an alpha of .01, there is
# significant enough evidence that sepal width and petal width are correlated in
# the population.

# Petal length and petal width have a population correlation different from 0, 
# since with a p-value of 2.2e-16, which is less than an alpha of .01, there is
# significant enough evidence that petal length and petal width are correlated in
# the population.

### Question 5

## Part a

mod <- lm(iris$Sepal.Length ~ iris$Petal.Length)
mod$coefficients

# The least squares regression line is y = 4.3066034 + .4089223x
# The slope interpreted is:
# As petal length increases by 1 cm sepal length increases by .4089223 cm.

## Part b

plot(iris$Sepal.Length ~ iris$Petal.Length, pch = 16, col = 'purple')
abline(mod, lwd = 3, col = 'darkgreen')

## Part c

cor.test(iris$Sepal.Length, iris$Petal.Length, conf.level = .05)

# For the t-test the test statistic t is 21.646. With a p-value of 2.2e-16, which
# is smaller than an alpha of .05, we would reject the null hypothesis that the
# slope equals 0, since we have significant enough evidence to support that the slope 
# does not equal 0.

anova(mod)

# For the F-test the test statistic F is 468.55. With a p-value of 2.2e-16, which
# is smaller than an alpha of .05, we would reject the null hypothesis that the 
# slope equals 0, since we have significant enough evidence to support that the 
# slope does not equal 0.

## Part d

qqnorm(mod$residuals)
qqline(mod$residuals)

# The data looks like it follows a Normal Distribution based off of the residuals
# Normal Q-Q plot, which follows a relatively straight line.

## Part e

plot(mod$residuals ~ mod$fitted.values)

# The data looks to relatively follow the constant variance assumption. The data
# doesn't look as if it has a fanning out/funneling look.

## Part f

summary(mod)$r.squared

# About 75.99% of the variability in sepal length can be explained by its linear
# relationship with petal length.

## Part g

4.3066034 + (.4089223 * 3.4)

# I would predict the sepal length of an iris with a petal length of 3.4 cm 
# to be 5.696939 cm.

## Part h

anova(mod)
2 *sqrt(24.525/148)

# I would expect 95% of the irises to have a sepal length within plus or minus
# .8141485 cm of their predicted values from the regression line.

