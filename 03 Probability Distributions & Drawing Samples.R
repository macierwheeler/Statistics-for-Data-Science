### Probability ###

# Number of rolls to get first "1"
rolls=c(1, 21, 2, 7, 1, 1, 2, 12, 16, 10, 6, 23)
plot(table(rolls),type='h')

# P(23 rolls to get first "1")
most=23
(5/6)^(most-1)*(1/6)
 
# Distribution of rolls to get first "1"
x=1:30
plot(x,dgeom(x-1,1/6),type='h',lwd=3,col=(x==most)+1, 
     ylab='Probability', xlab='Number of Rolls to Get First "1"')
text(x=most, y=dgeom(most-1,1/6)+.01, round(dgeom(most-1,1/6),4), col=2)
axis(1,most,col.axis=2)


# A coin flip
sample(x=c('H','T'), size=1, prob=c(.5,.5))


# 100 coin flips
flips=sample(c('H','T'), 100, replace=T)
flips
table(flips)
barplot(table(flips),col=1+rank(table(flips)),ylim=c(0,70), cex.names=2)


# 100 coin flips, 100 times

for(i in 1:100){
  flips=sample(c('H','T'), 100, replace=T)
  barplot(table(flips),col=1+rank(table(flips)),ylim=c(0,70), cex.names=2)
  Sys.sleep(0.2)
}


### Binomial Distributions ###

# 3 coin flips, "infinite" times

choose(3,0)*(0.5)^0*(0.5)^3      # P(0 Heads)
dbinom(x=0, size=3, prob=0.5)    # P(0 Heads)

probs=dbinom(x=0:3, size=3, prob=0.5)
probs
sum(probs)
plot(y=probs, x=0:3, 
     type='h',        # try also type='b' 
     lwd=4,
     ylim=c(0,max(probs)),
     xlab='Number of Heads',
     ylab='Probability')

# Binomial sample: 3 coin flips, 1000 sampled values
sample=rbinom(n=1000, size=3, prob=0.5)
hist(sample, breaks=seq(-0.25,3.25,0.5), col='gray', freq=F)

# Binomial sample: 1000 coin flips, 10000 sampled values
par(mfrow=c(2,2))
set.seed(42)
sample=rbinom(10000,size=1000, prob=0.5)
for(b in c(6,12,24,100)) {
  hist(sample,breaks=b,
       col='gray', xlab='x', freq=F,
       main=paste(b,'Bins'))
}
curve(dnorm(x,mean=1000*.5,sd=sqrt(1000*.5*.5)), col=4, lwd=3,add=T)
par(mfrow=c(1,1))


### Normal Distributions ###

mu=10; sigma=15

curve(dnorm(x,mean=mu,sd=sigma),xlim=mu+c(-5,5)*sigma)

normprob=function(m=0, s=1, from=-1, to=1, xlim=m+c(-3.5,3.5)*s){
  # modified code from https://www.statmethods.net/advgraphs/probability.html
  x = m + seq(-4,4,length=10000)*s
  hx = dnorm(x, m, s)
  plot(x, hx, type="n", xlab="x", ylab="Density", bty='n', xlim=xlim)
  i = (x >= from) & (x <= to)
  lines(x, hx)
  polygon(c(from,x[i],to), c(0,hx[i],0), col='darkgreen')
  abline(h=0)
  area <- pnorm(to, m, s) - pnorm(from, m, s)
  result <- paste("P(",from,"< X <",to,") =", signif(area, digits=4))
  mtext(result,3)
}

pnorm(mu, mean=mu, sd=sigma)
normprob(mu, sigma, -Inf, mu)

pnorm(90, mean=mu, sd=sigma)
normprob(mu, sigma, -Inf, 90)

pnorm(110, mean=mu, sd=sigma)
normprob(mu, sigma, -Inf, 110)

1-pnorm(110, mean=mu, sd=sigma)
pnorm(110, mean=mu, sd=sigma, lower.tail=F)
normprob(mu, sigma, 110, Inf)


pnorm(mu+sigma, mean=mu, sd=sigma) - 
  pnorm(mu-sigma, mean=mu, sd=sigma)
diff(pnorm(c(mu-sigma,mu+sigma), mean=mu, sd=sigma))
normprob(mu, sigma, mu-sigma, mu+sigma)

pnorm(mu+2*sigma, mean=mu, sd=sigma) - 
  pnorm(mu-2*sigma, mean=mu, sd=sigma)
normprob(mu, sigma, mu-2*sigma, mu+2*sigma)

pnorm(mu+3*sigma, mean=mu, sd=sigma) - 
  pnorm(mu-3*sigma, mean=mu, sd=sigma)
normprob(mu, sigma, mu-3*sigma, mu+3*sigma)


# Normal percentiles
qnorm(.90, mean=mu, sd=sigma)
normprob(mu, sigma, -Inf, 119.2233)

qnorm(.15, mean=mu, sd=sigma)
normprob(mu, sigma, -Inf, 84.4535)


# Sampling from a Normal Distribution
sample=rnorm(1000, mean=mu, sd=sigma)
sample[1:10]
length(sample)
length(unique(sample))
hist(sample, col='gray', freq=F, breaks=20)
curve(dnorm(x, mu, sigma), add=T, col=4, lwd=3)
mean(sample)
sd(sample)

sample=rnorm(1000000, mean=mu, sd=sigma)
sample[1:10]
length(sample)
length(unique(sample))
hist(sample, col='gray', freq=F, breaks=40)
curve(dnorm(x, mu, sigma), add=T, col=4, lwd=3)
mean(sample)
sd(sample)


# Adding Normal Distributions
x1=rnorm(1000, mean=60, sd=20)
x2=rnorm(1000, mean=40, sd=10)
hist(x1+x2)
x3=rnorm(1000, mean=60+40, sd=sqrt(20^2+10^2))
hist(x3)


# Normal Q-Q Plots

par(mfrow=c(1,2))

hist(x1+x2)
qqnorm(x1+x2)
qqplot(x1+x2)
qqline(x1+x2, col='red', lwd=3, lty=2)

uniform_sample=runif(1000, min=0, max=1)
hist(uniform_sample)
qqnorm(uniform_sample)

exponential_sample=rexp(1000, rate=1)
hist(exponential_sample)
qqnorm(exponential_sample)

beta_sample=rbeta(1000, shape1=19, shape2=1)
hist(beta_sample)
qqnorm(beta_sample)

hist(mtcars$mpg)
qqnorm(mtcars$mpg)