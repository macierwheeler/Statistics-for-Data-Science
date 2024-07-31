# Macie Rose Wheeler
# Worksheet 2

### Question 1

# Run the summary function for airquality. For each variable, describe its
# type and scale. Based on these statistics, describe the shape of each of the
# quantitative variables.

summary(airquality)

# Ozone, Solar.R, Wind, and Temp are qualitative variables. Month and Day are
# quantitative variables on a ordinal scale.
# The month and the Day are relatively symmetric.
# Ozone looks skewed right. Solar.R looks skewed left. Wind and Temp look
# relatively symmetric.

### Question 2

# Make a graph of Wind. Is its shape consistent with what you said in #1?

boxplot(airquality$Wind)

hist(airquality$Wind)
hist(airquality$Wind, probability = TRUE)
hist(airquality$Wind, breaks = 20)
hist(airquality$Wind, breaks = c(0, 3, 6, 9, 15, 18, 21), freq = TRUE)
hist(airquality$Wind, breaks = c(0, 3, 6, 9, 15, 18, 21), col = 'blue', freq = TRUE)
hist(airquality$Wind, breaks = c(0, 3, 6, 9, 15, 18, 21), col = 1:7, freq = TRUE)

### Question 3

# Make a graph of Solar.R. Is its shape consistent with what you said in #1?

boxplot(airquality$Solar.R)
hist(airquality$Solar.R)

### Question 4

# Find the mean Temp and the standard deviation of Temp for each Month in
# airquality. You can do these tediously, or you can check out the aggregate
# function.

means = aggregate(airquality$Temp, by = list(airquality$Month), FUN = mean)
sds = aggregate(airquality$Temp, by = list(airquality$Month), FUN = sd)

### Question 5

# Make boxplots of Temp across each Month. Make them colorful. If someone asked
# you what typical temperatures were in these months, would you tell them about
# the means or about the medians? Why?

boxplot(airquality$Temp ~ airquality$Month, col = 'yellow')
means$x
points(y = means$x, x = 1:5, pch = 16, cex = 2)

# I would tell them about the means because they seem more accurate with the
# actual data given the boxplots.

### Question 6

# Are there any outliers in the Ozone vector? If so, identify the day(s) with
# the outlier(s).

boxplot(airquality$Ozone)
quantile(airquality$Ozone, .25, na.rm = TRUE) - 1.5 * IQR(airquality$Ozone, na.rm = TRUE)
quantile(airquality$Ozone, .75, na.rm = TRUE) + 1.5 * IQR(airquality$Ozone, na.rm = TRUE)
abline(h = 131.125)
sort(airquality$Ozone)

# There are outliers above the value 131.125, which would be the values 135 and
# 168 in Ozone.

# Ozone%in%OzoneOutliers : allows you to look for both values in OzoneOutliers
# throughout the data

### Question 7

# Plot Ozone versus Solar.R. Does it appear that the two are related? How so?

plot(airquality$Ozone ~ airquality$Solar.R)

### Question 8

# Make a barplot of Month. What does this tell you (It should be nothing
# shocking)? If you are disappointed in how boring this last question was,
# then try playing around with the axis labels of the graph to make
# them a bit more descriptive.

barplot(table(airquality$Month), ylab = 'Number of Days', names.arg = month.name[5:9])
