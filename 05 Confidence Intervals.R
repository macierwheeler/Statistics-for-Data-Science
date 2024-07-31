### Visualizing the interpretation of a confidence interval ###

set.seed(1)

nints=20
conf_level=95
CIs=matrix(NA,nrow=nints,ncol=2)

for (i in 1:nints) {
  CIs[i,]=t.test(rnorm(20), conf.level=conf_level/100)$conf.int
}

cols=3-(apply(CIs,1,prod)>0)
plot(y=1:nints, x=CIs[,1], pch='(', col=cols,
     xlim=range(CIs), yaxt='n', xaxt='n', ylab='', xlab='')
axis(1, at=0, bquote(mu), cex.axis=2)
abline(v=0)
title(bquote(.(conf_level)*'% Confidence Intervals for'~mu))
mtext(bquote(.(sum(cols==3))~'of'~.(nints)~'('*.(round(100*mean(cols==3)))*'%) include'~mu))
points(y=1:nints, x=CIs[,2], pch=')', col=cols)
segments(y0=1:nints, x0=CIs[,1], x1=CIs[,2], col=cols, lwd=2)


### Calculating a z-based CI ###

# 95% CI
alpha=.05
qnorm(c(alpha/2, 1-alpha/2))

# 99% CI
alpha=.01
qnorm(c(alpha/2, 1-alpha/2))

# 90% CI
alpha=.10
qnorm(c(alpha/2, 1-alpha/2))



### Calculating a t-based CI for mean ###

# 95% CI with df=3
alpha=.05
qt(c(alpha/2, 1-alpha/2), df=3)

# 90% CI with df=3
alpha=.10
qt(c(alpha/2, 1-alpha/2), df=3)

# 90% CI with df=30
alpha=.10
qt(c(alpha/2, 1-alpha/2), df=30)

# 90% CI with df=300
alpha=.10
qt(c(alpha/2, 1-alpha/2), df=300)


mpg=mtcars$mpg
mean(mpg)
sd(mpg)
length(mpg)

# 95% CI for mean MPG
alpha=.05
mean(mpg)+c(-1,1)*qt(1-alpha/2,df=length(mpg)-1)*sd(mpg)/sqrt(length(mpg))
t.test(x=mpg, conf.level=.95)$conf.int

# "We are 95% confident that the true average MPG for the population of cars 
#  that this sample represents is between 17.92 and 22.26 miles per gallon."


### Calculating a Paired-t CI for mean difference ###

head(ChickWeight)

chicks=ChickWeight[ChickWeight$Diet==1 & ChickWeight$Time%in%c(0,2),-4]
chicks

weight_before=chicks$weight[chicks$Time==0]
weight_after=chicks$weight[chicks$Time==2]

data.frame(weight_before, weight_after)

par(mfrow=c(1,3))
hist(weight_before, col='gray', main='Before', breaks=seq(30,60,2.5))
hist(weight_after, col='gray', main='After', breaks=seq(30,60,2.5))
plot(weight_after~weight_before, xlim=range(chicks$weight), ylim=range(chicks$weight))
abline(a=0,b=1)

diffs=weight_after-weight_before

mean(diffs)    # d-bar
sd(diffs)      # s_d
length(diffs)  # n

alpha=.05
mean(diffs)+qt(c(alpha/2, 1-alpha/2), df=length(diffs)-1)*sd(diffs)/sqrt(length(diffs))

t.test(x=weight_after, y=weight_before, paired=TRUE, conf.level=1-alpha)$conf.int
t.test(x=diffs, conf.level=1-alpha)$conf.int
# "We are 95% confident that the average weight increase  
#  from before to after is between 3.98 and 7.72 gms.
#  Since the interval does not include 0, we can be 95%
#  confident that the weight increases on average."

### Calculating a 2-independent-samples t-based CI for difference of means ###

chicks=ChickWeight[ChickWeight$Diet %in% c(1,2) & ChickWeight$Time==21,]
dim(chicks)
weight_diet1=chicks$weight[chicks$Diet==1]
weight_diet2=chicks$weight[chicks$Diet==2]

mean(weight_diet1)    # x-bar_1
sd(weight_diet1)      # s_1
length(weight_diet1)  # n_1

mean(weight_diet2)    # x-bar_2
sd(weight_diet2)      # s_2
length(weight_diet2)  # n_2

df_approx=length(weight_diet1)+length(weight_diet2)-2   # roughly
alpha=.05
mean(weight_diet1)-mean(weight_diet2) + 
  qt(c(alpha/2,1-alpha/2),df=df_approx) *
    sqrt(var(weight_diet1)/length(weight_diet1)+var(weight_diet2)/length(weight_diet2))

t.test(x=weight_diet1, y=weight_diet2, conf.level=1-alpha)$conf.int
# "We are 95% confident that diet 1 produces chicks that weigh
#  on average between 96.26 gm less than and 22.36 gm more than 
#  chicks being fed diet 2. Since the interval includes 0, we 
#  cannot be 95% confident that there is a difference."


### One sample z-based CI for a proportion ###

heads=53
flips=100

phat=heads/flips
phat

flips*phat         # >5
flips*(1-phat)     # >5

alpha=.05
phat + qnorm(c(alpha/2,1-alpha/2))*sqrt(phat*(1-phat)/flips)

# The prop.test function calculates a very similar CI,
# but it uses a slightly different formulation:
prop.test(x=heads, n=flips, conf.level=1-alpha)$conf.int

# "We are 95% confident that the true proportion of heads is between
#   about 0.43 and 0.63. Since that includes 0.5, we cannot be
#   95% confident that the coin is unfair."

heads=38:62
plot(x=heads, y=dbinom(heads, size=flips, prob=.5), type='h', 
     ylab='Prob', lwd=3, col=1+(heads==53))


### Two independent samples z-based CI for a difference in proportions ###

UCBAdmissions    # a 3-dimensional matrix for admissions to UC Berkeley

E_Admitted=sum(UCBAdmissions[1,,5])
E_Total=sum(UCBAdmissions[,,5])
phat1=E_Admitted/E_Total
phat1

F_Admitted=sum(UCBAdmissions[1,,6])
F_Total=sum(UCBAdmissions[,,6])
phat2=F_Admitted/F_Total
phat2

phat=(E_Admitted+F_Admitted)/(E_Total+F_Total)

alpha=.05
phat1-phat2+qnorm(c(alpha/2,1-alpha/2))*sqrt((1/E_Total+1/F_Total)*phat*(1-phat))

# Again, R is doing something slightly different here, 
# but it is similar enough for our purposes
prop.test(x=c(E_Admitted, F_Admitted), 
          n=c(E_Total, F_Total),
          conf.level=1-alpha)$conf.int

# "We are 95% confident that Dept. E's true admission rate is between
#   0.15 and 0.23 higher than Dept. F's. Since that interval does not
#   include 0, we are 95% confident that there is a difference."

