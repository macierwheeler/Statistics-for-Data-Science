### Commenting ###

# This is a comment
# 2 + 2


### Basic calculation ###

2 + 2
10 - 6
2 * 2
8 / 2
2^2      # or 2**2
2*1+1
2*(1+1)


### Creating & using variables ###

x <- 2
x
X

3 -> y
y

y = 4
y

x + y
z = x + y
z

this_is_my.NEW.variable123 = z
this_is_my.NEW.variable123

b = "buffalo"    # or b = 'buffalo'
b

truth = TRUE
truth
truth + 1

missing = NA
missing
missing + 1


### Multiple lines of code on one line ###

t=9000; t = t + 1; t


### Logical operators ###

t > 9000

2 < 1
2 < 2
2 <= 2

z == 5
z != 5
z = 5

t > 9000 | 2 < 1

t > 9000 & 2 < 1


### Vectors ###

x = c(1,2,3,4,5,6,7,8)    # or x = 1:8
x

x + 1
x * 2
x^2

y = 1:2
y

x * y

x[1]
x[5]
x[2:4]
x[c(3,8)]

x>6
x[x>6]
which(x>6)

x==y
x[x==y]

all.equal(y,1:2)


### Functions ###

sum(x)
length(x)
sum(x)/length(x)
mean(x)

seq(5, 40, 3)
seq(from=5, to=40, by=3)
seq(by=3, to=40, from=5)
seq(b=3,t=40,f=5)
seq(to=40, by=3)

sentence=rep(b, 8)   # or rep(x=b, times=8)
sentence

# Getting help when you know the name of the function
?mean
help(mean)

# Getting help when you maybe don't know the name of the function
??mean


### Matrices ###
# Two (or higher) dimensional groupings of vectors, 
#   which must all be the same length & type

mat1=cbind(x,y)
mat1
mat1[2,]
mat1[,2]
mat1[2,2]

mat2=rbind(x,y)
mat2

mat3=matrix(1:9,nrow=3)
mat3

mat4=matrix(1:9,ncol=3,byrow=T)
mat4

t(mat3)

### Data Frames ###
# Two dimensional groupings of vectors, 
#   which must all be the same length 
#   (but not necessarily the same type)

dat1=data.frame(x,sentence)
dat1
dat1[2,]
dat1[,2]
dat1[2,2]
dat1$x
dat1$sentence
dat1[,'sentence']

is.data.frame(mtcars)
mtcars
dim(mtcars)
head(mtcars)

mtcars[mtcars$mpg>30,1:3]

mtcars$kpl=round(mtcars$mpg/2.352,2)
mtcars[,c('mpg','kpl')]
mtcars[,-(2:11)]                 # excluding certain columns

rownames(mtcars)


### Lists ###
# Groupings of vectors (and/or matrices and/or lists),
#   which do not need to have the same length or type

list1=list(first=x,second=y,third=b,fourth=sentence)
list1

list1[[1]]
list1$first
list1$first[3]
list1$third


### Graphics Intro ###

demo(graphics)

hist(mtcars$mpg, col='gray')

plot(y=mtcars$mpg, x=mtcars$hp)
plot(y=mtcars$mpg, x=mtcars$hp, pch=17, xlab='Horse Power', ylab='Miles per Gallon',cex=1.5, main='Plot comparing HP & MPG')


### Packages ###

search()
library()
install.packages('ISwR')
library('ISwR')


### Making New Functions ###

add=function(a,b) {
  a+b
}
add(1,2)

add=function(a=5,b=6) {
  a+b
}
add(1,2)
add(1)
add()
add(b=9)

test=function(x){
  if (is.character(x)) {
    return(x)
  } else {
    return(x+5)
  }
}

addone=function(x) {
  return(x+1)
}
addone(3)
addone(c(3,7,14,15,22,41))
addone(matrix(1:9,ncol=3))

'%+-%'=function(a,b) {
  a + c(-1,+1)*b
}
5%+-%2


### Loops ###

big=2
while (big < 10000)
  big=big^2
big

big=2
repeat {
  big=big^2
  if (big > 10000) break
}
big

testmat=1:10

fib=c(0,1)
for (i in testmat) {
  fib=c(fib,
        fib[length(fib)]+fib[length(fib)-1])
}
fib

testmat=3:100

fib=c(0,1)
for (i in testmat) {
  fib[i]=fib[i-2]+fib[i-1]
}
fib

testmat=matrix(3:14,nrow=3)    # for loops can cycle through matrices as well (try it above)


for (day in c('M','T','W','R','F')) {
  print(day)
}

count=0
for (i in fib) {
  count=count+1
  if (i %% 2 == 0) {
    string=cat(count,': Fibonacci number',i,'is Even\n')
  } else {
    string=cat(count,': Fibonacci number',i,'is Odd\n')
  }
  string
}

for (i in 1:5) {
  print(i)
  for (j in 6:7) {
    print(j)
  }
}


### Saving Things ###

save(fib, file='\\\\nas01.itap.purdue.edu\\puhome\\pu.data\\desktop\\fib.Rdata')
save.image(file='\\\\nas01.itap.purdue.edu\\puhome\\pu.data\\desktop\\all.Rdata')

# R objects are saved as .Rdata files
# R scripts are saved as .R files
# R histories are saved as .Rhistory

load(file='\\\\nas01.itap.purdue.edu\\puhome\\pu.data\\desktop\\fib.Rdata')
fib
load(file='\\\\nas01.itap.purdue.edu\\puhome\\pu.data\\desktop\\all.Rdata')
day
