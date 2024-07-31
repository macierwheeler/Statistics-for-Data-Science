### The F distribution ###

?pf

df1s=c(1,  1,3,10,10,50)
df2s=c(1,100,1, 1,10,50)
curve(df(x, df1=df1s[1], df2=df2s[1]),xlim=c(0,2.5), ylim=c(0,2), lwd=3, xlab=bquote(italic(x)), ylab='Density')
abline(v=0, h=0)
for (i in 2:6) curve(df(x, df=df1s[i], df2=df2s[i]), lwd=3, col=i, lty=i, add=T)
legend('topright', lwd=3, lty=1:6, col=1:6, legend=paste('F(df1 = ',df1s,', df2 = ',df2s,')',sep=''))


# T(df=q)^2 = F(df1=1,df2=q) 
par(mfrow=c(2,1))
randomsquaredt=rt(1000,df=50)^2
randomf=rf(1000,df1=1,df2=50)
b=seq(0,ceiling(max(randomsquaredt,randomf)),by=.5)
hist(randomsquaredt, breaks=b, col='gray', main='T(df=50)^2')
hist(randomf, breaks=b, col='gray', main='F(df1=1, df2=50)')


### ANOVA data ###

attach(mtcars)

boxplot(mpg~cyl, col=heat.colors(3), xlab='Cylinder')

mean(mpg[cyl==4])
mean(mpg[cyl==6])
mean(mpg[cyl==8])     # the sample means definitely look different

sd(mpg[cyl==4])
sd(mpg[cyl==6])
sd(mpg[cyl==8])        # pretty iffy, but we'll roll with it for now


### Type I Error Rates ###
alpha=.05
1-alpha
I=3
(1-alpha)^choose(I,2)


### ANOVA ###

unique(cyl)
I = length(unique(cyl))
I

# variation between groups

groupmeans=aggregate(mpg,by=list(cyl),mean)$x
groupmeans

groupn=aggregate(mpg,by=list(cyl),length)$x
groupn

mean(mpg)

SSG=sum(groupn*(groupmeans-mean(mpg))^2)
SSG

dfG=I-1
dfG

MSG=SSG/dfG
MSG

# variation within groups

groupvars=aggregate(mpg,by=list(cyl),var)$x
groupvars

SSE=sum((groupn-1)*groupvars)
SSE

dfE=length(mpg)-I
dfE

MSE=SSE/dfE
MSE

# comparing variations

SST=sum((mpg-mean(mpg))^2)
SST
SSG+SSE

F=MSG/MSE
F

pf(F, df1=dfG, df2=dfE, lower.tail=FALSE)

# IMPORTANT: make sure your grouping variable, if numeric, 
#   is a "factor" vector (can look at dfG to double-check: should be I-1, not 1)
mpganova=aov(mpg~as.factor(cyl))
mpganova
summary(mpganova)
names(mpganova)

# Alternative approach: doesn't give quite as much stuff to work with
anova(lm(mpg~as.factor(cyl)))


# Another example (null is true, but try it -- 1/20 times, the p-value should be < .05)

data=rnorm(100)
groups=rep(LETTERS[1:10],10)

boxplot(data~groups, col=rainbow(10))

summary(aov(data~groups))
anova(lm(data~groups))

# Another example (null is *mostly* true)
data=rnorm(100,0+2*(groups=='E'))
boxplot(data~groups, col=2+(groups=='E'))
summary(aov(data~groups))


### Pairwise comparisons ###

# Could do choose(3,2) = 3 individual t-tests
t.test(mpg[cyl==4],mpg[cyl==6])$p.value
t.test(mpg[cyl==4],mpg[cyl==8])$p.value
t.test(mpg[cyl==6],mpg[cyl==8])$p.value
# or
pairwise.t.test(mpg, cyl, p.adj='none', pool.sd=FALSE)
p.vals=pairwise.t.test(mpg, cyl, p.adj='none', pool.sd=FALSE)$p.value
# In these cases, compare to adjusted alpha = .05/3 = .0167
# All pairs are different (all p < .0167)

# Bonferroni for MPGs
pairwise.t.test(mpg, cyl, p.adj='bonferroni', pool.sd=FALSE)
# Compare here to family-wise error rate of alpha=.05.
# All pairs are different (all p < .05)
# R is adjusting the p-values here using what is essentially the inverse
#   of Bonferroni's correction (it is muliplying the p-values by 3)
round(p.vals*3,5)

# Tukey for MPGs
TukeyHSD(mpganova)
# All pairs are different, though the 8-cyl and 6-cyl are the closest
# Use the family-wise alpha = .05 here, as the adjustment is already
#   happening behind the scenes: "95% family-wise confidence level"


# Bonferroni for example with one different out of 10
choose(10,2)
pairwise.t.test(data, groups, p.adj='bonferroni')
round(pairwise.t.test(data, groups, p.adj='bonferroni')$p.value,3)
# Group E is significantly different from all the other groups.
# No other pairing is significantly different.
# (Your results may vary)

# Tukey for example with one different out of 10
TukeyHSD(aov(data~groups))
round(TukeyHSD(aov(data~groups))$groups,2)
# Same story
# (Your results may vary)


### Checking Normality of Residuals ###

# For MPGs
qqnorm(mpganova$residuals)
# Doesn't look too bad here, though the equal variances thing is still iffy

# For example with 1/10 different
qqnorm(aov(data~groups)$res)


### A "preview" of Two-Way ANOVA ###

# Two-way anova model for mpg based on cyl & gear 
mpgtwoway=aov(mpg~as.factor(cyl)*as.factor(gear))
summary(mpgtwoway)

# The third grouping variable which includes both cyl & gear is called the "interaction"
# Notice that the SS for cyl is the same as it was before.
# Now some of the SSE from before is included in the SS for gear and the cyl:gear interaction
# However, neither of those has a very large test statistic (its SS / SEE),
#   so neither gear, nor gear interacting with cyl, has a signficant effect on mpg

mpgtwowaynoint=aov(mpg~as.factor(cyl)+as.factor(gear))
summary(mpgtwowaynoint)
# Removing the insignificant interaction doesn't change much.
# Its SS gets absorbed back into SSE.
# SSE is a way to account for all the variation from things we aren't accounting for.


### Another Two-Way ANOVA Example ###

ToothGrowth
?ToothGrowth

boxplot(len~supp+as.factor(dose), col=c('orange','darkgreen'), data=ToothGrowth)
# It looks like higher doses => more growth
# For a dose of 2, OJ seems roughly equal to VC,
#   but for the lower doses, OJ seems more effective

summary(aov(len~supp*as.factor(dose), data=ToothGrowth))
# We have a significant interaction along with very significant
#   "main effects" (the individual factors, supp and dose)
# What does this significant interaction mean?

interaction.plot(x.factor=ToothGrowth$dose,
                 trace.factor=ToothGrowth$supp,
                 response=ToothGrowth$len,
                 col=c('orange','darkgreen'), lwd=3, type='b', pch=1)
# The effect of dose on len is pretty clear (both lines are increasing)
# The effect of supp on len is mostly clear (both lines are mostly separated)
# But the effect of supp depends on dose, or vice versa (the lines are not parallel)
