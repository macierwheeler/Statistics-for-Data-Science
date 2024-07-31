### One categorical predictor ###

attach(mtcars)

boxplot(mpg~cyl, col=heat.colors(3), xlab='Cylinder')

mod1=lm(mpg~as.factor(cyl))
anova(mod1)
summary(mod1)

aggregate(mpg,list(cyl),mean)
coef(mod1)
# 4-cylinder engines are being used as the baseline here


### One categorical & one numerical predictor (no int.) ###

plot(mpg~hp, col=2*(am+1), data=mtcars, pch=15+am)
legend('topright', col=c(2,4), legend=c('am=0 (automatic)','am=1 (manual)'), pch=15:16)

mod2=lm(mpg~hp+as.factor(am), data=mtcars)
summary(mod2)
anova(mod2)

abline(a=coef(mod2)[1], b=coef(mod2)[2], col=2, lwd=2)
abline(a=sum(coef(mod2)[c(1,3)]), b=coef(mod2)[2], col=4, lwd=2)


### One categorical & one numerical predictor (with int.) ###

plot(mpg~hp, col=2*(am+1), data=mtcars, pch=15+am)
legend('topright', col=c(2,4), legend=c('am=0 (automatic)','am=1 (manual)'), pch=15:16)

mod3=lm(mpg~hp*as.factor(am), data=mtcars)
summary(mod3)
anova(mod3)
# interaction isn't significant. Should probably stick with the no-int. model for this data

coef(mod3)
abline(a=coef(mod3)[1], b=coef(mod3)[2], col=2, lwd=2)
abline(a=sum(coef(mod3)[c(1,3)]), b=sum(coef(mod3)[c(2,4)]), col=4, lwd=2)


plot(mpg~hp, col=2*(vs+1), data=mtcars, pch=15+vs)
legend('topright', col=c(2,4), legend=c('vs=0 (V-shaped Engine)','vs=1 (Straight Engine)'), pch=15:16)

mod4=lm(mpg~hp*as.factor(vs), data=mtcars)
summary(mod4)
anova(mod4)
# interaction is definitely significant

coef(mod4)
abline(a=coef(mod4)[1], b=coef(mod4)[2], col=2, lwd=2)
abline(a=sum(coef(mod4)[c(1,3)]), b=sum(coef(mod4)[c(2,4)]), col=4, lwd=2)


### Multiple numerical predictors ###

mod5=lm(mpg~hp+wt, data=mtcars)
summary(mod5)
anova(mod5)

library(rgl)
plot3d(mpg~hp+wt, data=mtcars, size=8, col=4)
planes3d(coef(mod5)[2], coef(mod5)[3], -1, coef(mod5)[1], col=2, alpha=0.5)
# browseURL(paste("file://", writeWebGL(dir=file.path(tempdir(), "webGL"), width=1200), sep=""))
# https://www.stat.purdue.edu/~keatont/mtcars3d.html
# https://www.stat.purdue.edu/~keatont/mtcars3dplane.html 


mod6=lm(mpg~hp*wt, data=mtcars)
summary(mod6)
anova(mod6)

plot3d(mpg~hp+wt, data=mtcars, size=8, col=4)
grd = expand.grid(hp=seq(from=min(hp),to=max(hp),length.out=50), 
                  wt=seq(from=min(wt),to=max(wt),length.out=50))
grd$pred = predict(mod6, newdata=grd)
persp3d(x=unique(grd[[1]]), y=unique(grd[[2]]), 
              z=matrix(grd[[3]],50,50), add=TRUE, col=2, alpha=0.5)
# https://www.stat.purdue.edu/~keatont/mtcars3dint.html 


### All the predictors ###

mtcars2=mtcars
mtcars2$cyl=as.factor(mtcars2$cyl)
mtcars2$gear=as.factor(mtcars2$gear)

modallint=lm(mpg~.^2, data=mtcars2)
summary(modallint)
# Not big enough n to estimate all two-way interactions and main effects

modall=lm(mpg~., data=mtcars2)
summary(modall)
# Now none of the predictors are significant at alpha = .05


# One automated way to narrow down to a reasonable model is (Backwards) Stepwise model selection
step(modall)

modstep=lm(mpg ~ wt + qsec + am, data=mtcars2)
summary(modstep)

plot3d(mpg~wt+qsec, data=mtcars2, size=8, col=2*am+2)
planes3d(coef(modstep)[2], coef(modstep)[3], -1, coef(modstep)[1], col=2, alpha=0.5)
planes3d(coef(modstep)[2], coef(modstep)[3], -1, sum(coef(modstep)[c(1,4)]), col=4, alpha=0.5)

modstep2=lm(mpg ~ (wt + qsec + am)^2, data=mtcars2)
summary(modstep2)

modstep3=lm(mpg ~ wt + qsec + am + wt:am, data=mtcars2)
summary(modstep3)

plot3d(mpg~wt+qsec, data=mtcars2, size=8, col=2*am+2)
planes3d(coef(modstep3)[2], coef(modstep3)[3], -1, coef(modstep3)[1], col=2, alpha=0.5)
planes3d(sum(coef(modstep3)[c(2,5)]), coef(modstep3)[3], -1, sum(coef(modstep3)[c(1,4)]), col=4, alpha=0.5)


# Is this model with the wt:am interaction significantly better than without?
# Can compare their Adjusted R^2 values (use Adjusted since the models are different sizes: have different number of predictors)
# Or can do a Partial F-test to compare the two models:
anova(modstep, modstep3)
# Here H0: beta_4  =  0   (the beta corresponding to the interaction)
#   vs H1: beta_4 =/= 0
# Big F / low p-value means that there is a signficant difference in the fit of the two models:
#  the added predictor(s) (here, the wt:am interaction) are signficantly beneficial

# Should probably check the assumptions
plot(modstep3)

# Can do predictions (and basically everything else we talked about from before) too:
# Predict MPG for a manual 2500 lb car (wt is in 1000s) that can do a 1/4 mile in 17 seconds (qsec)
b=coef(modstep3); b
unname(b[1]+b[2]*2.5+b[3]*17+b[4]*1+b[5]*2.5*1)
pred=predict(modstep3, data.frame(wt=2.5, am=1, qsec=17), interval='prediction', level=.9)
pred
points3d(x=2.5, y=17, z=pred[1], color='purple', size=12)
segments3d(x=2.5, y=17, z=pred[2:3], color='purple', size=30)