# Macie Rose Wheeler
# Worksheet 1

### Question 2

# Find the mean of the numbers in the Ozone vector in the airquality data set.
# What issue do you run into, and why? Resolve that issue.

mean(airquality$Ozone, na.rm = TRUE)

### Question 3

# airquality: What was the lowest Wind speed in July(Month7)?

min(airquality$Wind[airquality$Month == 7])

### Question 4

# airquality: What was the Ozone reading on the day with the highest Temp?

plot(y = airquality$Ozone, x = airquality$Temp)

#  or
#  attach(airquality)
#  plot(y = Ozone, x = Temp)
#  can then just call Ozone and Temp to reference those columns

airquality$Ozone[which.max(airquality$Temp)]

#  airquality[which.max(airquality$Temp),]
#  airquality$Ozone[airquality$Temp == max(airquality$Temp)]

### Question 5

# Make a plot with the Temp vector of airquality on the y-axis.
# The x-axis should be the numbers 1, 2, 3, ..., n(the total number of days/rows in airquality).
# In your plot, use the type="l" option (that’s a lowercase “L”); what does that do?

plot(y = airquality$Temp, x = 1:length(airquality$Temp), type='l')

### Question 6

# Add a new column to airquality named MonthName that is, well, the name of
# the month. That is, when Month is 5, MonthName should be "May" and so on.

airquality$MonthName[airquality$Month == 1] = 'January'
airquality$MonthName[airquality$Month == 2] = 'February'
airquality$MonthName[airquality$Month == 3] = 'March'
airquality$MonthName[airquality$Month == 4] = 'April'
airquality$MonthName[airquality$Month == 5] = 'May'
airquality$MonthName[airquality$Month == 6] = 'June'
airquality$MonthName[airquality$Month == 7] = 'July'
airquality$MonthName[airquality$Month == 8] = 'August'
airquality$MonthName[airquality$Month == 9] = 'September'
airquality$MonthName[airquality$Month == 10] = 'October'
airquality$MonthName[airquality$Month == 11] = 'November'
airquality$MonthName[airquality$Month == 12] = 'December'

airquality

### Question 7

# Create a function that converts degrees Fahrenheit to
# Celsius(°C= (°F − 32)× 5/9). Use this function to create a new column in
# airquality named TempC based on Temp.

FtoC <- function(f) {
  ((f-32) * 5/9)
}

airquality$TempC = FtoC(airquality$Temp)
airquality[, c('Temp', 'TempC')]

### Question 8

# Revert airquality back to its original form by removing the extra columns
# I made you add.

airquality <- airquality[, 1:6]
airquality
