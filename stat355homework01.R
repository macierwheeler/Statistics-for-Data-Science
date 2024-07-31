# Macie Rose Wheeler
# Homework 1

### Question 1

dim(iris)
#  There are 150 rows and 5 columns in iris.

### Question 2

irisdata <- rbind(iris[1, ], iris[2, ], iris[3, ], iris[4, ], iris[5, ],
                  iris[6, ], iris[nrow(iris), ])
sub1 <- data.frame(irisdata)
sub1

### Question 3

columnNames <- c("Sepal.Length", "Sepal.Width", "Species")
correctlength <- c(iris[iris$Sepal.Length < 4.7, columnNames])
sub2 <- data.frame(correctlength)
sub2

### Question 4

Versicolor.Is.The.Best <- c(rep(0, sum(iris$Species == "setosa")),
                            rep(10, sum(iris$Species == "versicolor")),
                            rep(0, sum(iris$Species == "virginica")))
Versicolor.Is.The.Best

### Question 5

pw <- c(iris$Petal.Width)
mean(pw)
median(pw)
max(pw)
min(pw)

### Question 6

sum <- 0
count <- 0

while (sum <= 40) {
  count <- count + 1
  sum <- sum + pw[count]
}

#  The sum is:
sum

#  The loop executed 71 times.
count

### Question 7

centimetertoinches <- function(x) {
  return(x / 2.54)
}

pw_in = 1:length(pw)

for (i in pw_in) {
  pw_in[i] <- centimetertoinches(pw[i])
}

pw_in[1:10]
  
### Question 8

plot(x = iris$Sepal.Length, y = iris$Petal.Length,
     main = 'iris Data Frame: Sepal Length Compared to Petal Length',
     xlab = 'Sepal Length', ylab = 'Petal Length', col = 'purple', pch = 8)