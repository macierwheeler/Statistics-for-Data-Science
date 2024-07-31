### Covariance & Correlation ###

attach(mtcars)

plot(mpg~hp, pch=16)

n=length(mpg)   # n is the number of PAIRS


sum((mpg-mean(mpg))*(hp-mean(hp)))/(n-1)
cov(mpg,hp)

cov(mpg,hp)/(sd(mpg)*sd(hp))
r=cor(mpg,hp)
r


### Hypothesis test for Population Correlation ###

# H0: rho  =  0
# H1: rho =/= 0

t=r*sqrt((n-2)/(1-r^2))
t

2*pt(abs(t),n-2,lower.tail=FALSE)

cor.test(mpg,hp, alternative='two.sided')
# p is low, so reject H0.
# Significant evidence that mpg and hp are correlated in the population.


### Simple linear regression: PPT Example ###

# The data
ads=(1:5)*100
sales=c(1,1,2,2,4)*1000
plot(sales~ads, pch=16, cex=3, col='purple')

# The regression line
mod=lm(sales~ads)
abline(mod, lwd=3,col='darkgreen')
names(mod)
mod$coef
# predicted sales = -100 + 7 * ads

# predicted values
-100+7*ads
yhat=mod$fit
yhat

# residuals
round(mod$res,10)
e=round(sales-mod$fit,10)
e

sum(e)
SSE=sum(e^2)
SSE

# calculating slope & intercept
cor(sales,ads)*sd(sales)/sd(ads)
b1=mod$coef[2]
b1
mean(sales)-unname(b1)*mean(ads)
b0=mod$coef[1]
b0

# Estimating the model standard deviation
s=sqrt(SSE/(length(ads)-2))
s
# About 68% of the dots should be within s of the line,
# About 95% of the dots should be within 2s of the line, etc.

# Testing H0: beta_1 = 0 vs H1: beta_1 =/= 0
t=b1/(s/sqrt(sum((ads-mean(ads))^2)))
t
2*pt(abs(t),df=length(ads)-2,lower.tail=FALSE)

summary(mod)$coef[2,]

cor.test(ads, sales)$stat
cor.test(ads, sales)$p.value

# ANOVA table
anova(mod)
t^2

# coefficient of determination
cor(ads,sales)^2
summary(mod)$r.squared
R2=anova(mod)[1,2]/sum(anova(mod)[,2])
R2


# CIs and PIs

# yhat for x=350
sum(c(1,350)*mod$coef)
# Estimated sales is $2350 when advertising is $350

predict(mod, data.frame(ads=350), interval='predict', level=.95)
# 95% confident that sales will be between $217 and $4483 
#   for one month when advertising is $350

predict(mod, data.frame(ads=350), interval='confidence', level=.95)
# 95% confident that average sales will be between $1436 and $3264 
#   across all months when advertising is $350


# plotting CI and PI across all x
newx=100:500
cis=predict(mod, data.frame(ads=newx), interval='confidence')
pis=predict(mod, data.frame(ads=newx), interval='predict')

plot(sales~ads, pch=16, cex=3, col='purple',
     xlab='Advertising ($)', ylab='Sales ($)',
     ylim=range(pis[,2:3]), xlim=range(newx))
abline(mod, lwd=3,col='darkgreen', xlim=range(ads))
for (i in 2:3) {
  points(y=cis[,i], x=newx, lwd=3, lty=2, col='darkgreen', type='l')
  points(y=pis[,i], x=newx, lwd=3, lty=3, col='purple', type='l')
}


# Checking assumptions
qqnorm(mod$res); qqline(mod$res)
plot(mod$res~mod$fit)
#or plot(mod)



### Simple linear regression: MPG Example ###

plot(mpg~hp, data=mtcars, pch=16)

mod1=lm(mpg~hp, data=mtcars)
mod1

abline(mod1,lwd=2,col=2)

summary(mod1)
anova(mod1)

# Maybe a straight line isn't the best way to describe this relationship...

mod2=lm(mpg~hp+I(hp^2), data=mtcars)
mod2$coef

curve(coef(mod2)[1]+coef(mod2)[2]*x+coef(mod2)[3]*x^2, add=T, lwd=2, col=3)

mod3=lm(log(mpg)~hp, data=mtcars)
mod3
curve(exp(coef(mod3)[1]+coef(mod3)[2]*x), add=T, lwd=2, col=4)

# Or maybe we should introduce other predictors...

plot(mpg~hp, col=2*(am+1), data=mtcars, pch=15+am)
legend('topright', col=c(2,4), legend=c('am=0 (automatic)','am=1 (manual)'), pch=15:16)

mod4=lm(mpg~hp+as.factor(am), data=mtcars)
summary(mod4)
anova(mod4)

abline(a=coef(mod4)[1], b=coef(mod4)[2], col=2, lwd=2)
abline(a=sum(coef(mod4)[c(1,3)]), b=coef(mod4)[2], col=4, lwd=2)

mod5=lm(mpg~hp+wt, data=mtcars)
summary(mod5)
anova(mod5)

library(rgl)
plot3d(mpg~hp+wt, data=mtcars, size=8, col=4)
planes3d(coef(mod5)[2], coef(mod5)[3], -1, coef(mod5)[1], col='red', alpha=0.5)
# browseURL(paste("file://", writeWebGL(dir=file.path(tempdir(), "webGL"), width=1200), sep=""))
# https://www.stat.purdue.edu/~keatont/mtcars3d.html
# https://www.stat.purdue.edu/~keatont/mtcars3dplane.html 