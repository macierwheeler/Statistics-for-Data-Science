### Looking at data & statistics ###

head(mtcars)

str(mtcars)
str(iris)

summary(mtcars)

mtcars$mpg
attach(mtcars)
mpg

mean(mpg)    # sum(mpg)/length(mpg)
var(mpg)     # sum((mpg-mean(mpg))^2)/(length(mpg)-1)
sd(mpg)      # sqrt(var(mpg))

median(mpg)

quarts=quantile(mpg,c(.25,.75))

range(mpg)
diff(range(mpg))
iqr=IQR(mpg)

quarts+c(-1.5,1.5)*iqr

x1=x2=1:5
x2[5]=50

mean(x1)
mean(x2)

median(x1)
median(x2)

IQR(x1)
IQR(x2)

sd(x1)
sd(x2)


### Graphics ###

boxplot(mpg)
boxplot(mpg~cyl,col=2:4)

hist(mpg)
hist(mpg,breaks=100)

plot(mpg~wt)
pairs(mtcars[,c(1,3:7)],pch=16)


cyl_cat=factor(cyl)
cyl_cat

tcyl=table(cyl_cat)
tcyl

pie(tcyl)

barplot(tcyl)


par(mfrow=c(3,1))
cyls=levels(cyl_cat)
for(i in 1:length(cyls)) {
  hist(mpg[cyl==cyls[i]],
       main=paste(cyls[i],'Cylinders'),
       xlab='MPG',
       xlim=range(mpg),
       col=i+1,
       breaks=seq(10,34,2))
}
par(mfrow=c(1,1))


# See also https://www.statmethods.net/graphs/dot.html
#   for a dotchart example

