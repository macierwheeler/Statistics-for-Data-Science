# Macie Wheeler
# Homework 6

### Question 1

## Part a

attach(iris)

mod1 <- lm(Sepal.Length ~ Petal.Length * Species, data = iris)
summary(mod1)

## Part b

# Versicolor: .8283x + 2.4076
# Virginica: .9957x + 1.0597
# Setosa: .5423x + 4.2132

# As petal length for the species Versicolor increases by 1 cm, I would predict
# sepal length for the species Versicolor to increase by .8283 cm.
# As petal length for the species Virginica increases by 1 cm, I would predict
# sepal length for the species Virginica to increase by .9957 cm.
# As petal length for the species Setosa increases by 1 cm, I would predict
# sepal length for the species Setosa to increase by .5423 cm.

## Part c

plot(Sepal.Length ~ Petal.Length, data = iris, pch = 16)
legend('topleft', fill = c(2, 4, 6), legend = c('Setosa', 'Versicolor', 'Virginica'))
abline(a = coef(mod1)[1], b = coef(mod1)[2], col = 2, lwd = 2)
abline(a = sum(coef(mod1)[c(1, 3)]), b = sum(coef(mod1)[c(2, 5)]), col = 4, lwd = 2)
abline(a = sum(coef(mod1)[c(1, 4)]), b = sum(coef(mod1)[c(2, 6)]), col = 6, lwd = 2)

## Part d

predict(mod1, data.frame(Petal.Length = 3.4, Species = 'versicolor'),
                interval = 'prediction', level = .95)

# I would predict the sepal length for a Versicolor iris with a petal length that
# is 3.4 cm long to be about 5.22 cm.

## Part e

anova(mod1)

# The null hypothesis is that the Petal Length:Species interaction slopes are all
# equal to zero.
# The alternative hypothesis is that the Petal Length:Species interaction slopes 
# are not all equal to zero.
# With a p-value of .1895, which is higher than an alpha of .05, we would fail
# to reject the null hypothesis, since we don't have significant enough evidence
# that the slopes for the Petal Length:Species interactions are not all equal to 
# zero.

## Part f

mod2 = lm(Sepal.Length ~ Petal.Length + Species, data = iris)
summary(mod2)

# As petal length for all of the species (Versicolor, Virginica, and Setosa)
# increases by 1 cm, I would predict sepal length for each of the species to
# increase by .90456 cm.

## Part g

predict(mod2, data.frame(Petal.Length = 5.5, Species = 'virginica'),
        interval = 'prediction', level = .90)

# Using the no-interaction model, I'm 90% sure that the sepal length of a
# Virginica iris with a 5.5-cm-long petal will be between 5.975822 cm and 
# 7.106103 cm.

## Part h

summary(mod2)

# The R squared value for the no-interaction model is .8367. This means that about
# 83.67% of the variability in sepal length can be explained by its 
# relationship with petal length and species as predictors.

### Question 2

## Part a

attach(LifeCycleSavings)

modallint = lm(sr~.^2, data = LifeCycleSavings)
anova(modallint)

# It looks as if pop15 and ddpi are both significant/useful. This is due to the 
# fact that pop15 has a p-value of .0006, which is lower than an alpha of .05,
# and that ddpi has a p-value of .0459, which is lower than an alpha of .05.

## Part b

summary(modallint)

# None of the predictors look significant/useful now, as none of them have a
# p-value that is smaller than an alpha of .05.

## Part c

step(modallint)
modallint2 = lm(sr ~ pop15 + dpi + ddpi + dpi:ddpi, data = LifeCycleSavings)

# The predictors included in this reduced model are pop15, dpi, and ddpi, as well
# as the interaction of dpi:ddpi.

## Part d

summary(modallint)
summary(modallint2)

# The R squared for the full model was .412, whereas the R squared for the 
# reduced model was .3745. This shows that the R squared in the reduced model
# is smaller than the R squared in the full model.
# The adjusted R squared for the full model was .2612, whereas the adjusted R
# squared for the reduced model was .3189. This shows that the adjusted R
# squared in the reduced model is larger than the adjusted R squared in the full
# model.
# Based off of the adjusted R squared value, which seems more reliable than R 
# squared, I would choose the reduced model, since it's adjusted R squared was
# higher than the adjusted R squared of the full model.

## Part e

anova(modallint, modallint2)

# Given a relatively small F value of .4146 and a larger p-value of .8648, 
# which is larger than an alpha of .05, there seems as if there isn't a significant 
# difference in the fit of the two models.
# It doesn't seem as if we've lost anything of value by removing those predictors,
# it seems as if we haven't removed enough.

## Part f

summary(modallint2)

# The predictor ddpi looks very insignificant based on its t-test, seeing as its
# p-value is .850521, which is a lot larger than an alpha of .05. I think that
# the stepwise procedure decided to keep ddpi since the interaction dpi:ddpi
# had the smallest p-value when doing a t-test on the full model.

### Question 3

## Part a

beaver2$tempF <- ((beaver2$temp / 5) * 9) + 32

## Part b

attach(beaver2)

logmod <- glm(activ ~ tempF, family = binomial(link = 'logit'))

plot(activ ~ tempF, pch = 16)
axis(4, at = 0:1, c('activity inside', 'activity outside'), las = 1)

alltempF = seq(min(tempF), max(tempF), len = 1000)
phats = predict(logmod, newdata = data.frame(tempF = alltempF), type = 'response')
lines(phats ~ alltempF, col = 2, lwd = 2)

## Part c

summary(logmod)
exp(coef(logmod)[2])

# If the temperature in degrees Fahrenheit is increased by 1 degree, the odds that
# the activity was outside the retreat is 3505.832 times higher than it was 
# before.

## Part d

predict(logmod, newdata = data.frame(tempF = 98.7), type = 'response')

# If the beaver's body temperature is 98.7 degrees Fahrenheit, the probability
# that it is active is about .2%.

## Part e

predict(logmod, newdata = data.frame(tempF = 99.7), type = 'response')

# If the beaver's body temperature is 99.7 degrees Fahrenheit, the probability
# that it is active is about 88.63%.

### Question 4

## Part a

library(nnet)
attach(iris)

multi1 = multinom(Species ~ Sepal.Length)
predict(multi1, data.frame(Sepal.Length = 6.3), type = 'probs')

# The probability of a Setosa iris to have a sepal length of 6.3 cm is about .7%.
# The probability of a Versicolor iris to have a sepal length of 6.3 cm is about 46.78%.
# The probability of a Virginica iris to have a sepal length of 6.3 cm is about 52.56%.

## Part b

multi2 = multinom(Species ~ Petal.Length)
predict(multi2, data.frame(Petal.Length = 5.1), type = 'probs')

# The probability of a Setosa iris to have a petal length of 5.1 cm is about 0%.
# The probability of a Versicolor iris to have a petal length of 5.1 cm is about 10.7%.
# The probability of a Virginica iris to have a petal length of 5.1 cm is about 89.3%.

## Part c

summary(multi1)
summary(multi2)

# The residual deviance for the first model is 182.0679 and the AIC is 190.0679.
# The residual deviance for the second model is 33.48589 and the AIC is 41.48589.

# The second model appears to be the better model based on these metrics, since
# they are lower in the second model for both residual deviance and AIC.

## Part d

multi3 = multinom(Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width)
predict(multi3, data.frame(Sepal.Length = 6.3, Sepal.Width = 2.8,
                           Petal.Length = 5.1, Petal.Width = 1.5), type = 'probs')

# The probability that a flower with a sepal length of 6.3, a sepal width of 2.8, a 
# petal length of 5.1, and a petal width of 1.5 is of the species Setosa is about
# 0%.
# The probability that a flower with a sepal length of 6.3, a sepal width of 2.8, a 
# petal length of 5.1, and a petal width of 1.5 is of the species Versicolor is about
# 79.4%.
# The probability that a flower with a sepal length of 6.3, a sepal width of 2.8, a 
# petal length of 5.1, and a petal width of 1.5 is of the species Virginica is about
# 20.6%.

## Part e

Species[Sepal.Length == 6.3 & Sepal.Width == 2.8 & Petal.Length == 5.1 & Petal.Width == 1.5]

# The flower described in part d is of the species Virginica.
