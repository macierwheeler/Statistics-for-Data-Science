### Logistic Regression ###

# Logistic curve
curve(exp(x)/(1+exp(x)), xlim=c(-10,10), col='darkgreen', lwd=3)
abline(h=0:1, lty=3)


### AM vs MPG ###

attach(mtcars)
# am is already coded as 0/1 : 0 = automatic, 1 = manual

plot(am~mpg, pch=16); axis(4, at=0:1, c('a','m'), las=1)

logmod1=glm(am~mpg, family=binomial(link=logit))   # or just family=binomial (logit is default link)

summary(logmod1)

allmpg = seq(min(mpg),max(mpg),len=1000)
logoddshats = predict(logmod1, newdata=data.frame(mpg=allmpg))
phats = exp(logoddshats)/(1+exp(logoddshats))
# or phats = predict(logmod1, newdata=data.frame(mpg=allmpg), type='response')
lines(phats ~ allmpg, col=2, lwd=2)

abline(h=0.5, v=allmpg[which(abs(phats-.5)==min(abs(phats-.5)))], lty=2, lwd=2, col=2)

exp(coef(logmod1)[2])
# If mpg is increased by 1, the odds that the car
#   is a manual are 1.36 times what they were before


### AM vs Weight ###

plot(am~wt, pch=16); axis(4, at=0:1, c('a','m'), las=1)

logmod2=glm(am~wt, family=binomial) 

summary(logmod2)

allwt = seq(min(wt),max(wt),len=1000)
phats = predict(logmod2, newdata=data.frame(wt=allwt), type='response')
lines(phats ~ allwt, col=2, lwd=2)

abline(h=0.5, v=allwt[which(abs(phats-.5)==min(abs(phats-.5)))], lty=2, lwd=2, col=2)

exp(coef(logmod2)[2])
# If weight is increased by 1000 lbs, then the odds that 
#  the car is a manual are 0.018 times what they were before


### AM vs MPG & Weight ###

library(rgl)
plot3d(am~mpg+wt, size=8)

logmod3=glm(am~mpg+wt, family=binomial(link=logit))

grd = expand.grid(mpg=seq(from=min(mpg),to=max(mpg),length.out=50), 
                  wt=seq(from=min(wt),to=max(wt),length.out=50))
grd$pred = predict(logmod3, newdata=grd, type='response')
persp3d(x=unique(grd[[1]]), y=unique(grd[[2]]), 
              z=matrix(grd[[3]],50,50), add=TRUE, col=2, alpha=0.5)
# https://www.stat.purdue.edu/~keatont/mtcars3dlogit.html

summary(logmod3)

# MPG doesn't appear helpful (multicollinearity?)
# Let's check...

# Comparing the larger model to the smaller model

anova(logmod2, logmod3, test='Chisq')

# There is one df difference (because there is one more predictor in the larger model)
# The difference in the residual deviance is 1.99
# According to the p-value (from a Chi-Square test),
#   that extra deviance that is accounted for in the larger model isn't worth it.
# The added predictor (in this case, MPG) isn't adding a significant amount of predictive help

pchisq(1.9918, df=1, lower.tail=FALSE)
curve(dchisq(x,df=1), xlim=c(0,5), col=2, lwd=2); abline(h=0, v=0)
segments(x0=1.9918,y0=0,y1=dchisq(1.9918,df=1), col=4, lwd=3)

### A quick look at the Chi-Square distribution ###

# A Chi-Square R.V. with 1 df is the same as a squared standard normal R.V.
z=rnorm(10000)               # ~N(0,1)
hist(z, col='gray', freq=FALSE)

hist(z^2, freq=FALSE, col='gray', breaks=100)
curve(dchisq(x,df=1), col=2, lwd=2, add=T)

# A Chi-Square R.V. with q df is the same as a the sum of q independent squared standard normal R.V.s
hist(rnorm(10000)^2+rnorm(10000)^2+rnorm(10000)^2, freq=FALSE, col='gray', breaks=100)
curve(dchisq(x,df=3), col=2, lwd=2, add=T)


### Stepwise model selection ###

logmod4=glm(am~mpg+wt+disp+hp, family=binomial(link=logit))
summary(logmod4)

logstep=step(logmod4)
summary(logstep)

plot3d(am~wt+hp, size=8)

grd = expand.grid(wt=seq(from=min(wt),to=max(wt),length.out=50), 
                  hp=seq(from=min(hp),to=max(hp),length.out=50))
grd$pred = predict(logstep, newdata=grd, type='response')
persp3d(x=unique(grd[[1]]), y=unique(grd[[2]]), 
              z=matrix(grd[[3]],50,50), add=TRUE, col=2, alpha=0.5)
# https://www.stat.purdue.edu/~keatont/mtcars3dlogit2.html


### Alternative link: Probit model ###

curve(dnorm(x), xlim=c(-4,4), col=4, lwd=3, main='PDF of N(0,1)')
curve(pnorm(x), xlim=c(-4,4), col=4, lwd=3, main='CDF of N(0,1)')

plot(am~mpg, pch=16); axis(4, at=0:1, c('a','m'), las=1)

probit=glm(am~mpg, family=binomial(link=probit))
summary(probit)

allmpg = seq(min(mpg),max(mpg),len=1000)
phats = predict(probit, newdata=data.frame(mpg=allmpg), type='response')
lines(phats ~ allmpg, col=4, lwd=2)

legend('right', c('Probit','Logit'), lwd=2, col=c(4,2), lty=1:2)

phats = predict(logmod1, newdata=data.frame(mpg=allmpg), type='response')
lines(phats ~ allmpg, col=2, lwd=2, lty=2)


### Multinomial Regression with one predictor ###

cylcols=as.numeric(as.factor(cyl))+1  
            # 4 cyl=red, 6cyl=green, 8cyl=blue
plot(cyl~mpg,col=cylcols, pch=cylcols+13, cex=1.2)

as.factor(cyl)
# R will automatically select the lowest level 
#  (by default, earliest alphabetically) as the baseline
# If you want to change the ordering of the levels, check out this nice site:
#   http://www.cookbook-r.com/Manipulating_data/Changing_the_order_of_levels_of_a_factor/

# To fit the multinomial model, we need the 'nnet' package:
# install.packages('nnet')   # (if it's not already installed)
library(nnet)

multi1=multinom(cyl~mpg)
summary(multi1)

round(predict(multi1, data.frame(mpg=24), type='probs'),3)
round(predict(multi1, data.frame(mpg=10), type='probs'),3)
round(predict(multi1, data.frame(mpg=20), type='probs'),3)
round(predict(multi1, data.frame(mpg=21), type='probs'),3)


coef(multi1)[,2]
# the log(odds of cyl=6 vs cyl=4) decrease as mpg increases
# the log(odds of cyl=8 vs cyl=4) decrease even more as mpg increases

exp(coef(multi1)[,2])
# the odds of cyl=6 vs cyl=4 are multiplied by .110 if mpg increases by 1
# the odds of cyl=8 vs cyl=4 are multiplied by .028 if mpg increases by 1

p21=round(predict(multi1, data.frame(mpg=21), type='probs'),3)
p21
p21[2]/p21[1]   # odds of cyl=6 vs cyl=4 at 21 mpg
p21[3]/p21[1]   # odds of cyl=8 vs cyl=4 at 21 mpg

p22=round(predict(multi1, data.frame(mpg=22), type='probs'),3)
p22
p22[2]/p22[1]   # odds of cyl=6 vs cyl=4 at 22 mpg
p22[3]/p22[1]   # odds of cyl=8 vs cyl=4 at 22 mpg


### Multinomial Regression with more than one predictor ###

multi2=multinom(cyl~mpg+wt)
summary(multi2)

plot(wt~mpg, col=cylcols, pch=cylcols+13, cex=1.2)
legend('topright', levels(as.factor(cyl)), pch=sort(unique(cylcols))+13,
       title='cyl', cex=1.2, col=sort(unique(cylcols)))

points(x=30, y=3, pch=4, cex=2)
round(predict(multi2, data.frame(mpg=30, wt=3), type='probs'),3)
points(x=24, y=3, pch=4, cex=2)
round(predict(multi2, data.frame(mpg=24, wt=3), type='probs'),3)
points(x=24, y=5, pch=4, cex=2)
round(predict(multi2, data.frame(mpg=24, wt=5), type='probs'),3)
points(x=17, y=3.2, pch=4, cex=2)
round(predict(multi2, data.frame(mpg=17, wt=3.2), type='probs'),3)

grd = expand.grid(mpg=seq(from=min(mpg),to=max(mpg),length.out=100), 
                  wt=seq(from=min(wt),to=max(wt),length.out=100))
grd$pred = predict(multi2, newdata=grd, type='probs')
dev.new(width=8, height=8)
plot(grd$wt~grd$mpg, pch=15, col=rgb(grd$pred))


### Ordinal Logistic Regression: Proportional Odds ###

# In the last example, we were treating the different categories of cyl as nominal (unordered)
# However, they really are ordinal: 4 < 6 < 8
ordered(cyl)
# Should we perhaps use a model that can take advantage of this ordered structure?

# One possible model is known as the proportional odds model
# We won't go into much detail here, but there's a nice introductory blog post on the topic:
#   https://towardsdatascience.com/implementing-and-interpreting-ordinal-logistic-regression-1ee699274cf5

# To fit the proportional odds logistic regression (POLR) model, we need the 'MASS' package:
# install.packages('MASS')   # (if it's not already installed)
library(MASS)

plot(cyl~wt, col=cylcols, pch=cylcols+13, cex=1.2)
po=polr(ordered(cyl)~wt)
summary(po)

abline(v=2:4)
round(predict(po, data.frame(wt=2:4), type='probs'),3)


### A nonparametric approach to classification: K-Nearest Neighbors (KNN) ###

# To run KNN, we need the 'class' package:
# install.packages('class')   # (if it's not already installed)
library(class)
?knn

knn(train=data.frame(wt), test=data.frame(wt=2:4), cl=factor(cyl), k=1, prob=TRUE)
# The closest value to wt=2 had 4 cyl, so predict 4 cyl
# The closest value to wt=3 had 6 cyl, so predict 6 cyl
# The closest value to wt=4 had 8 cyl, so predict 8 cyl

knn(train=data.frame(wt), test=data.frame(wt=2:4), cl=factor(cyl), k=3, prob=TRUE)
# The 3 closest values to wt=2 all had 4 cyl, so predict 4 cyl
# The 3 closest values to wt=3 were 4, 6 and 8 cyls (all tied for majority), 
#   so predict one of them randomly with prob 1/3
#   cyl[order(abs(wt-3))][1:3]
# The 3 closest values to wt=4 all had 8 cyl, so predict 8 cyl

knn(train=data.frame(wt), test=data.frame(wt=2:4), cl=factor(cyl), k=10, prob=TRUE)
# Of the 10 closest values to wt=2, 8 (majority) of them had 4 cyl, so predict 4 cyl
#  cyl[order(abs(wt-2))][1:10]
# Of the 10 closest values to wt=3, 4 (majority) of them had 6 cyl, so predict 6 cyl
#  cyl[order(abs(wt-3))][1:10]
# Of the 12 closest values to wt=4, 9 (majority) of them had 8 cyl, so predict 8 cyl
#  cyl[order(abs(wt-4))][1:12]  (3-way tie for 10th, so 10th, 11th, 12th included)


### KNN for classification in two dimensions ###

plot(wt~mpg, col=cylcols, pch=cylcols+13, cex=1.2)
legend('topright', levels(as.factor(cyl)), pch=sort(unique(cylcols))+13,
       title='cyl', cex=1.2, col=sort(unique(cylcols)))

text(x=c(30,24,24,17), y=c(3,3,5,3.2), LETTERS[1:4])
knn(train=data.frame(mpg, wt), test=data.frame(mpg=c(30,24,24,17),wt=c(3,3,5,3.2)), cl=factor(cyl), k=1, prob=TRUE)

text(x=c(30,24,24,17), y=c(3,3,5,3.2), LETTERS[1:4])
knn(train=data.frame(mpg, wt), test=data.frame(mpg=c(30,24,24,17),wt=c(3,3,5,3.2)), cl=factor(cyl), k=10, prob=TRUE)


grd = expand.grid(mpg=seq(from=min(mpg),to=max(mpg),length.out=100), 
                  wt=seq(from=min(wt),to=max(wt),length.out=100))
grd$pred1 = knn(train=data.frame(mpg, wt), test=grd, cl=factor(cyl), k=1)
dev.new(width=8, height=8)
plot(grd$wt~grd$mpg, pch=15, col=as.numeric(grd$pred1)+1, main='K = 1 nearest neighbor')

grd$pred3 = knn(train=data.frame(mpg, wt), test=grd[,1:2], cl=factor(cyl), k=3)
dev.new(width=8, height=8)
plot(grd$wt~grd$mpg, pch=15, col=as.numeric(grd$pred3)+1, main='K = 3 nearest neighbors')

grd$pred5 = knn(train=data.frame(mpg, wt), test=grd[,1:2], cl=factor(cyl), k=5)
dev.new(width=8, height=8)
plot(grd$wt~grd$mpg, pch=15, col=as.numeric(grd$pred5)+1, main='K = 5 nearest neighbors')

grd$pred10 = knn(train=data.frame(mpg, wt), test=grd[,1:2], cl=factor(cyl), k=10)
dev.new(width=8, height=8)
plot(grd$wt~grd$mpg, pch=15, col=as.numeric(grd$pred10)+1, main='K = 10 nearest neighbors')

# Since the predictors here are on such different scales, 
#   maybe try standardizing both (scale(x)) and rerun the above code.
# What would be the advantages and disadvantages of doing so?


### You can use KNN for nonparametric regression too! ###

# To fit the KNN regression model, we need the 'FNN' package:
# install.packages('FNN')
library(FNN)
plot(mpg~hp, pch=16)

?knn.reg
allhp=seq(min(hp),max(hp),len=1000)
knnreg1=knn.reg(train=data.frame(hp), test=data.frame(allhp), y=mpg, k = 1)
lines(knnreg1$pred~allhp, col=2, lwd=2)

knnreg3=knn.reg(train=data.frame(hp), test=data.frame(allhp), y=mpg, k = 3)
lines(knnreg3$pred~allhp, col=3, lwd=2)

knnreg10=knn.reg(train=data.frame(hp), test=data.frame(allhp), y=mpg, k = 10)
lines(knnreg10$pred~allhp, col=4, lwd=2)

knnreg25=knn.reg(train=data.frame(hp), test=data.frame(allhp), y=mpg, k = 25)
lines(knnreg25$pred~allhp, col=5, lwd=2)

legend('topright', lty=1, lwd=2, col=2:5, paste('K =',c(1,3,10,25)))

