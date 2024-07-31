### One-sample t-test for mu ###

# Two-sided test
# H0: mu  =  17
# H1: mu =/= 17

mu0=17

mpg=mtcars$mpg
xbar=mean(mpg)
s=sd(mpg)
n=length(mpg)

# test statistic
t=(xbar-mu0)/(s/sqrt(n))
t


2*pt(abs(t),df=n-1,lower.tail=FALSE)

t.test(x=mpg, alternative = 'two.sided', mu = mu0)

# p = 0.0068 < alpha = .05
# "Reject H0. We have significant evidence (at alpha=.05) that the
#  true population mean MPG is not 17."

alpha=.05
CV=qt(c(alpha/2, 1-alpha/2), df=n-1)
CV
t
# t is more extreme than CV, so reject H0.


# One-sided (greater than) test
# H0: mu = 17
# H1: mu > 17

t

pt(t,df=n-1,lower.tail=FALSE)

t.test(x=mpg, alternative = 'greater', mu = mu0)

# p = 0.0034 < alpha = .05
# "Reject H0. We have significant evidence (at alpha=.05) that the
#  true population mean MPG is greater than 17."

alpha=.05
CV=qt(1-alpha, df=n-1)
CV
t
# t is more extreme (greater than) than CV, so reject H0.

######################################################################
# Try both of the above with mu0 of 18 instead of 17. What do you see?
######################################################################

# One-sided (less than) test
# H0: mu = 17
# H1: mu < 17

t

pt(t,df=n-1,lower.tail=TRUE)

t.test(x=mpg, alternative = 'less', mu = mu0)

# p = 0.9966 > alpha = .05
# "Fail to reject H0. We do not have significant evidence (at alpha=.05) that the
#  true population mean MPG is less than 17. In fact, we have 'anti-evidence'."

alpha=.05
CV=qt(alpha, df=n-1)
CV
t
# t is not more extreme (less than) than CV, so fail to reject H0.


### Paired t-test ###

chicks=ChickWeight[ChickWeight$Diet==1 & ChickWeight$Time%in%c(0,2),-4]
weight_before=chicks$weight[chicks$Time==0]
weight_after=chicks$weight[chicks$Time==2]
data.frame(weight_before, weight_after)

# H0: mu_{after-before} = 0
# H1: mu_{after-before} > 0

diffs=weight_after-weight_before

dbar=mean(diffs)    # d-bar
s=sd(diffs)      # s_d
n=length(diffs)  # n

t=dbar/(s/sqrt(n))
t

pt(t, df=n-1, lower.tail=FALSE)

alpha=.05
qt(alpha, df=n-1, lower.tail=FALSE)

t.test(x=weight_after, y=weight_before, paired=TRUE, alternative='greater')
t.test(x=diffs, alternative='greater')

# p-value is less than alpha (or t > CV). Reject H0.
# We have significant evidence (at alpha=.05) that the chicks have 
# increased in weight on average.


### Two-Sample t-test ###

chicks=ChickWeight[ChickWeight$Diet %in% c(1,2) & ChickWeight$Time==21,]
weight_diet1=chicks$weight[chicks$Diet==1]
weight_diet2=chicks$weight[chicks$Diet==2]

# H0: mu_diet1  =  mu_diet2  (or mu_diet1-mu_diet2  =  0)
# H1: mu_diet1 =/= mu_diet2  (or mu_diet1-mu_diet2 =/= 0)

mean(weight_diet1)    # x-bar_1
sd(weight_diet1)      # s_1
length(weight_diet1)  # n_1

mean(weight_diet2)    # x-bar_2
sd(weight_diet2)      # s_2
length(weight_diet2)  # n_2

df_approx=length(weight_diet1)+length(weight_diet2)-2   # roughly

t=(mean(weight_diet1)-mean(weight_diet2))/
   sqrt(var(weight_diet1)/length(weight_diet1)+var(weight_diet2)/length(weight_diet2))
t

2*pt(abs(t), df=df_approx, lower.tail=FALSE)

alpha=.05
qt(c(alpha/2, 1-alpha/2), df=df_approx) 

t.test(x=weight_diet1, y=weight_diet2, alternative='two.sided')
# Again, R is calculating the DF in a slightly different (and much
# more complicated) way, so the p-values aren't exactly the same.
# In either case, p-value > alpha (and t is not more extreme than CVs),
# so fail to reject H0. We do not have sufficient evidence that the two
# diets produce different average weights.


### One-proportion z-test ###

heads=53
flips=100

# H0: p  =  0.5 (fair coin)
# H1: p =/= 0.5 (unfair)
p0=0.5

phat=heads/flips

flips*p0; flips*(1-p0)     # >5

z=(phat-p0)/sqrt(p0*(1-p0)/flips)
z
2*pnorm(abs(z), lower.tail=FALSE)

alpha=.05
qnorm(c(alpha/2, 1-alpha/2))

prop.test(x=heads, n=flips, p=0.5, alternative='two.sided', correct=FALSE)
# p-value > alpha (and z is not more extreme than critical values), so 
# fail to reject H0. We do not have sufficient evidence (at alpha=.05)
# that the die is unfair.

# Note: Could use the continuity correction here (correct=TRUE), but we
# don't need to worry about it. With that, you would add or subtract 0.5  
# from the number of successes when calculating the z test statistic.
# You can play around with it if you are curious.


### Two-sample proportion z-test ###

E_Admitted=sum(UCBAdmissions[1,,5])
E_Total=sum(UCBAdmissions[,,5])
phat1=E_Admitted/E_Total
phat1

F_Admitted=sum(UCBAdmissions[1,,6])
F_Total=sum(UCBAdmissions[,,6])
phat2=F_Admitted/F_Total
phat2

phat=(E_Admitted+F_Admitted)/(E_Total+F_Total)
phat

# H0: p1  =  p2  (Depts E & F have same admission rates)
# H1: p1 =/= p2  (Depts E & F have different admission rates)

z = (phat1-phat2)/sqrt((1/E_Total+1/F_Total)*phat*(1-phat))
z

2*pnorm(abs(z), lower.tail=FALSE)

alpha=.05
qnorm(c(alpha/2, 1-alpha/2))

# Again, R is doing something slightly different here, 
# but it is similar enough for our purposes
prop.test(x=c(E_Admitted, F_Admitted), 
          n=c(E_Total, F_Total),
          alternative='two.sided')

# p-value is way less than alpha (and z is definitely more 
# extreme than critical values), so definitely reject H0.
# Sufficient evidence (at alpha=.05) that the departments
# have different admission rates.


### Wilcoxon Signed Rank Test (1-sample) ###

# Two-sided test
# H0: mpg distribution located at 17
# H1: mpg distribution not located at 17

mu0=17

mpg=mtcars$mpg

diffs=mpg-mu0
ranks=rank(abs(diffs))
signed.ranks=sign(diffs)*ranks
sum(signed.ranks[signed.ranks>0])   # test stat V

# For this and the other Wilcoxon tests, we'll let R handle the p-value calculation
wilcox.test(diffs)
wilcox.test(mpg, mu=mu0)
# p-value < alpha=.05. Reject H0. We have significant evidence that the distribution
# of MPGs is not located at 17.


### Wilcoxon Signed Rank Test (Paired Samples) ###

chicks=ChickWeight[ChickWeight$Diet==1 & ChickWeight$Time%in%c(0,2),-4]
weight_before=chicks$weight[chicks$Time==0]
weight_after=chicks$weight[chicks$Time==2]
data.frame(weight_before, weight_after)

# H0: Weights have the same distribution at both times
# H1: Weights are systematically higher at Time 2 than at Time 0

diffs=weight_after-weight_before

ranks=rank(abs(diffs))
signed.ranks=sign(diffs)*ranks
sum(signed.ranks[signed.ranks>0])   # test stat V

wilcox.test(diffs, alternative='greater')
wilcox.test(weight_after, weight_before, paired=TRUE, alternative='greater')
# p-value < alpha=.05. Reject H0. We have significant evidence that weights
# are systematically higher at Time 2 than at Time 0.


### Wilcoxon Rank Sum Test (two independent samples) ###

chicks=ChickWeight[ChickWeight$Diet %in% c(1,2) & ChickWeight$Time==21,]
chicks
weight_diet1=chicks$weight[chicks$Diet==1]
weight_diet2=chicks$weight[chicks$Diet==2]

# H0: no difference in weight distributions between the two diets
# H1: is a difference in weight distributions between the two diets

n_total=nrow(chicks)
n_total

n_diet1=length(weight_diet1)
n_diet1
n_diet2=length(weight_diet2)
n_diet2

ranks=rank(chicks$weight)
sum(ranks[chicks$Diet==1]) - n_diet1*(n_diet1+1)/2  # test stat W

wilcox.test(weight_diet1, weight_diet2)
# or wilcox.test(chicks$weight ~ chicks$Diet)
# p-value > alpha = .05. Fail to reject H0. We do not have significant evidence that
# there is a difference in weights between the two diets.

# Switching the group we are focusing on changes the test stat,
# but not the p-value or conclusion
sum(ranks[chicks$Diet==2]) - n_diet2*(n_diet2+1)/2
wilcox.test(weight_diet2, weight_diet1)