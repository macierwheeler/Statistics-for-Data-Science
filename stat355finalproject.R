# Macie Wheeler
# Final Project

### Part 1

# reading in the data set, needs to be read in with the file path on your computer
data <- read.csv('/Users/maciewheeler/Downloads/archive/data.csv')

# getting the dimensions of the data set
dim(data)

# removing the columns of the data that are not of interest for my analysis
variables_of_interest <- c('acousticness', 'danceability', 'duration_ms', 'energy',
                           'explicit', 'instrumentalness', 'key', 'liveness', 
                           'loudness', 'mode', 'popularity', 'speechiness', 'tempo',
                           'valence', 'year')
project_data <- data[variables_of_interest]

# getting dimensions of the project data
dim(project_data)

# getting the summary statistics of the attributes of the project data
summary(project_data)

# plots for popularity

boxplot(project_data$popularity, col = 'lightblue', xlab = 'popularity')
hist(project_data$popularity, col = 'lightblue', xlab = 'popularity',
     main = 'Histogram of popularity')

# plots for tempo

boxplot(project_data$tempo, col = 'yellow', xlab = 'tempo (BPM)')
hist(project_data$tempo, col = 'yellow', xlab = 'tempo (BPM)',
     main = 'Histogram of tempo (BPM)')

# plots for mode

hist(project_data$mode, col = 'purple', xlab = 'mode',
     main = 'Histogram of mode (0 = minor, 1 = major)')

# plots for duration_ms

boxplot(project_data$duration_ms, col = 'lightgreen', xlab = 'duration_ms')
hist(project_data$duration_ms, col = 'lightgreen', xlab = 'duration_ms',
     main = 'Histogram of duration_ms', breaks = 60)

# plots for year

boxplot(project_data$year, col = 'red', xlab = 'year')
hist(project_data$year, col = 'red', xlab = 'year',
     main = 'Histogram of year')

# plots for acousticness

boxplot(project_data$acousticness, col = 'cyan', xlab = 'acousticness')
hist(project_data$acousticness, col = 'cyan', xlab = 'acousticness',
     main = 'Histogram of acousticness')

# plots for danceability

boxplot(project_data$danceability, col = 'orange', xlab = 'danceability')
hist(project_data$danceability, col = 'orange', xlab = 'danceability',
     main = 'Histogram of danceability')

# plots for energy

boxplot(project_data$energy, col = 'darkseagreen', xlab = 'energy')
hist(project_data$energy, col = 'darkseagreen', xlab = 'energy',
     main = 'Histogram of energy')

# plots for instrumentalness

boxplot(project_data$instrumentalness, col = 'coral', xlab = 'instrumentalness')
hist(project_data$instrumentalness, col = 'coral', xlab = 'instrumentalness',
     main = 'Histogram of instrumentalness')

# plots for valence

boxplot(project_data$valence, col = 'lightpink', xlab = 'valence')
hist(project_data$valence, col = 'lightpink', xlab = 'valence',
     main = 'Histogram of valence')

# plots for liveness

boxplot(project_data$liveness, col = 'slateblue', xlab = 'liveness')
hist(project_data$liveness, col = 'slateblue', xlab = 'liveness',
     main = 'Histogram of liveness')

# plots for loudness

boxplot(project_data$loudness, col = 'khaki', xlab = 'loudness')
hist(project_data$loudness, col = 'khaki', xlab = 'loudness',
     main = 'Histogram of loudness')

# plots for speechiness

boxplot(project_data$speechiness, col = 'tomato', xlab = 'speechiness')
hist(project_data$speechiness, col = 'tomato', xlab = 'speechiness',
     main = 'Histogram of speechiness')

# plots for explicit

hist(project_data$explicit, col = 'thistle', xlab = 'thistle',
     main = 'Histogram of explicit (0 = no explicit content, 1 = explicit content')

# plots for key

boxplot(project_data$key, col = 'chocolate', xlab = 'key')
hist(project_data$key, col = 'chocolate', xlab = 'key',
     main = 'Histogram of key')

### Part 2

## Question 1

# getting the multiple regression model with all predictors
popmodall <- lm(project_data$popularity ~., data = project_data)

# getting the summary of this model
summary(popmodall)
# getting the multiple regression F-test of this model
anova(popmodall)

# backwards stepwise method for all predictors
step(popmodall)
steppopmod <- lm(project_data$popularity ~ acousticness + danceability + energy + explicit
                    + instrumentalness + key + liveness + loudness + mode +
                      speechiness + tempo + valence + year, data = project_data)

# getting the summary of the backwards stepwise model
summary(steppopmod)
# getting the multiple regression F-test of this model
anova(steppopmod)

# a partial F-test between the model with all predictors and the model with
# all the predictors created with the backwards stepwise method
anova(popmodall, steppopmod)

# checking assumptions for first model with all predictors
plot(popmodall)

# checking assumptions for backwards stepwise model
plot(steppopmod)

## Question 2

# subsetting project data to get the tempos where the mode equals 0 (the mode is minor)
minor_project_data <- project_data$tempo[project_data$mode == 0]
# subsetting project data to get the tempos where the mode equals 1 (the mode is major)
major_project_data <- project_data$tempo[project_data$mode == 1]

# checking whether the length of the major tempo and minor tempo data are the same
length(minor_project_data)
length(major_project_data)

# histogram of the tempos for minor tracks
hist(minor_project_data, col = 'darkblue', xlab = 'Tempo (BPM)', main = 'Histogram
     of Tempo (BPM) for minor tracks')

# histogram of the tempos for major tracks
hist(major_project_data, col = 'violet', xlab = 'Tempo (BPM)', main = 'Histogram
     of Tempo (BPM) for major tracks')

# running the two-sample unpaired t-test
t.test(x = minor_project_data, y = major_project_data, alternative = 'two.sided')

## Question 3

# running the ANOVA test 
durationanova <- aov(project_data$duration_ms ~ as.factor(project_data$year))

# getting the ANOVA table
summary(durationanova)

# pairwise comparisons
TukeyHSD(durationanova)

# checking the assumptions
plot(durationanova)
