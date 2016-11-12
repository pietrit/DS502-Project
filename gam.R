# Generalized Additive Model (GAM)


# Create dataframe
setwd("~/Documents/ds502-project")
dataframe <- read.csv('data/hour.csv')


# Pre-process data
attach(dataframe)
dataframe = subset(dataframe, select = -c(instant))
season = factor(season)
holiday = factor(holiday)
workingday = factor(workingday)
weekday = factor(weekday)
weathersit = factor(weathersit)
mnth = factor(mnth)
yr = factor(yr)


# Create training & testing data
set.seed(1)
train_percent = 0.8

library(caret)
train_index = createDataPartition(cnt, p = train_percent, list = FALSE)
train = dataframe[train_index, ]
test = dataframe[-train_index, ]

train_data = subset(train, select = -c(casual, registered, cnt))
test_data  = subset(test, select  = -c(casual, registered, cnt))

train_casual_data = subset(train, select = -c(registered, cnt))
train_registered_data = subset(train, select = -c(casual, cnt))
train_count_data = subset(train, select = -c(casual, registered))


# GAMs
library(gam)

# CASUAL
# Natural splines
gam_natural_splines = lm(casual ~ ns(temp, 4) + ns(atemp, 5) + ns(hum, 6) + ns(windspeed, 7) + season + holiday + workingday + weekday + weathersit + mnth + yr, data=train_casual_data)
par(mfrow=c(2,6))
plot.gam(gam_natural_splines, se=TRUE, col="red")

# Smoothing splines
gam_smoothing_splines = gam(casual ~ s(temp, 4) + s(atemp, 5) + s(hum, 6) + s(windspeed, 7) + season + holiday + workingday + weekday + weathersit + mnth + yr, data=train_casual_data)
par(mfrow=c(2,6))
plot.gam(gam_smoothing_splines, se=TRUE, col="blue")

# REGISTERED

# COUNT

# Cross Validation
# K-Fold Cross Validation, k=10


