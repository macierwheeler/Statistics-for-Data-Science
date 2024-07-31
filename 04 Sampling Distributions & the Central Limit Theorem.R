### Normal Approximation to Binomial ###

n=40; p=0.5   # Binomial distribution parameters

# Symmetric enough?
n*p           # > 5
n*(1-p)       # > 5

# Approximate P(X=18) is P(17.5 < X* < 18.5))
diff(pnorm(c(17.5, 18.5), mean=n*p, sd=sqrt(n*p*(1-p))))

# Actual P(X=18)
dbinom(18, size=n, prob=p)

# Approximate P(X>24) is P(X* > 24.5))
1-pnorm(24.5, mean=n*p, sd=sqrt(n*p*(1-p)))

# Actual P(X>24)
sum(dbinom(25:n, size=n, prob=p))


### Sampling Distributions ###

par(mfrow=c(2,2))

# A single die roll

n=10000
plot(table(sample(1:6,n,replace=T))/n, type='h', lwd=3, xlim=c(1,6),
     main='1 Roll', xlab='', ylab='Prob')

# Average of two die rolls
twos=c(1.5,3,5.5,4.5,3,1,3,3,2.5,5.5,2,2.5,6,4,5.5,2.5,3.5)
twos=(sample(1:6,n,replace=T)+sample(1:6,n,replace=T))/2
plot(table(twos)/length(twos), type='h', lwd=3, xlim=c(1,6),
     main='Average of 2 Rolls', xlab='', ylab='Prob')

# Average of four die rolls
fours=c(1.5,3.75,4,3.75,4,2.5,3.75,2.5,4.5,3.25,2,3.5)
fours=colMeans(matrix(sample(1:6,n*4,replace=T),nrow=4))f
plot(table(fours)/length(fours), type='h', lwd=3, xlim=c(1,6),
     main='Average of 4 Rolls', xlab='', ylab='Prob')

# Average of twelve die rolls
twelves=c()
twelves=colMeans(matrix(sample(1:6,n*12,replace=T),nrow=12))
plot(table(twelves)/length(twelves), type='h', lwd=3, xlim=c(1,6),
     main='Average of 12 Rolls', xlab='', ylab='Prob')


### Normal distribution: Sampling dist. of mean ###

par(mfrow=c(2,2))
normprob=function(m=0, s=1, from=-1, to=1, xlim=m+c(-3.5,3.5)*s){
  # modified code from https://www.statmethods.net/advgraphs/probability.html
  x = m + seq(-4,4,length=10000)*s; hx = dnorm(x, m, s); plot(x, hx, type="n", xlab="x", ylab="Density", bty='n', xlim=xlim)
  i = (x >= from) & (x <= to); lines(x, hx); polygon(c(from,x[i],to), c(0,hx[i],0), col='darkgreen'); abline(h=0)
  area <- pnorm(to, m, s) - pnorm(from, m, s); result <- paste("P(",from,"< X <",to,") =", signif(area, digits=4)); mtext(result,3)
}

# Probability one person's IQ is above 110
1-pnorm(110, mean=100, sd=15)
normprob(100, 15, 110, Inf, xlim=c(60,140)); title('n = 1')

# Probability average of two people's IQs is above 110
1-pnorm(110, mean=100, sd=15/sqrt(2))
normprob(100, 15/sqrt(2), 110, Inf, xlim=c(60,140)); title('n = 2')

# Probability average of ten people's IQs is above 110
1-pnorm(110, mean=100, sd=15/sqrt(10))
normprob(100, 15/sqrt(10), 110, Inf, xlim=c(60,140)); title('n = 10')

# Probability average of fifty people's IQs is above 110
1-pnorm(110, mean=100, sd=15/sqrt(50))
normprob(100, 15/sqrt(50), 110, Inf, xlim=c(60,140)); title('n = 50')

# "A person is smart. People are dumb..." ~ Agent K, MiB


### Central Limit Theorem ###

par(mfrow=c(1,3))
curve(dexp(x, rate=1), xlim=c(0,6), lwd=3, main='Exponential Distribution (rate=1)')
# An Exponential distribution with rate 1 should have a mean=1 and sd=1

# By CLT, if we take a sample of size 100, the sample mean should have a Normal
# distribution with mean=1 and sd=1/sqrt(100)=0.1. Let's check.

sample_means=rowMeans(matrix(rexp(100*100000, rate=1),ncol=100))
mean(sample_means)
sd(sample_means)
hist(sample_means, freq=F, breaks=40)
curve(dnorm(x, mean=1, sd=0.1), lwd=3, col='blue', add=T)
qqnorm(sample_means)

# Probability a sample mean is below 0.85
pnorm(0.85, mean=1, sd=0.1)   # actual
mean(sample_means<0.85)       # approximate via simulation

# Probability a sample mean is within 0.2 (2 standard errors) of 1 (the mean)
diff(pnorm(c(0.8, 1.2), mean=1, sd=0.1))         # actual
mean(sample_means > 0.8 & sample_means < 1.2)    # approx


### Bootstrapping ###

# Estimating the population median

exp_sample=rexp(100, rate=1)     # sample of 100 values from that Exponential distribution

par(mfrow=c(1,2))
hist(exp_sample, col='gray', main='Sample from Exponential(rate=1)')
median(exp_sample)
abline(v=median(exp_sample), col='red', lwd=3)    # Sample Median
abline(v=qexp(.5, rate=1), col='green', lwd=3, lty=2)   # True Median (would be unknown in reality)
legend('topright', c('Sample Median','True Median'), lty=1:2, lwd=3, col=c('red','green'))

bootsize=10000
medians=rep(NA,bootsize)

for (i in 1:bootsize) {
  resample=sample(exp_sample,replace=T)
  medians[i]=median(resample)
}

mean(medians)
sd(medians)

# A bootstrapped estimate of the sampling distribution of the sample median
hist(medians, col='gray', freq=F, main='Bootstrapped Sampling Distribution of Median')
abline(v=median(exp_sample), col='red', lwd=3)    # Sample Median
abline(v=qexp(.5, rate=1), col='green', lwd=3, lty=2)   # True Median
abline(v=quantile(medians, c(.025,.975)), lwd=3, lty=3, col='purple')
    # A bootstrapped 95% confidence interval for the population median
legend('topright', c('Sample Median','True Median', 'Middle 95% of\nSample Medians'), lty=1:3, 
       lwd=3, col=c('red','green', 'purple'))

# Estimating (poorly?) the population 99th percentile

hist(exp_sample, col='gray', main='Sample from Exponential(rate=1)')
quantile(exp_sample, .99)
abline(v=quantile(exp_sample, .99), col='red', lwd=3)    # Sample 99th Percentile
abline(v=qexp(.99, rate=1), col='green', lwd=3, lty=2)   # True 99th Percentile
legend('top', c('Sample 99th Pct.','True 99th Pct.'), lty=1:2, lwd=3, col=c('red','green'))

bootsize=10000
p99s=rep(NA,bootsize)

for (i in 1:bootsize) {
  resample=sample(exp_sample,replace=T)
  p99s[i]=quantile(resample, .99)
}

mean(p99s)
sd(p99s)

# A bootstrapped estimate of the sampling distribution of the sample 99th percentile
hist(p99s, col='gray', freq=F, main='Bootstrapped Sampling Distribution of 99th Pct.')
abline(v=quantile(exp_sample, .99), col='red', lwd=3)    # Sample 99th Percentile
abline(v=qexp(.99, rate=1), col='green', lwd=3, lty=2)   # True 99th Percentile
abline(v=quantile(p99s, c(.025,.975)), lwd=3, lty=3, col='purple')
    # A bootstrapped 95% confidence interval for the population 99th percentile
legend('topleft', c('Sample 99th Pct.','True 99th Pct.', 'Middle 95% of\nSample 99th Pct.s'), lty=1:3, 
       lwd=3, col=c('red','green', 'purple'))