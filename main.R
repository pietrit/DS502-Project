## set your working directory
setwd("C:/Users/denni/Desktop/DS 502/Project/Bike-Sharing-Dataset")

#---------------

## getting datasets

## read in bike-sharing datasets
bks_hour <- read.csv('hour.csv')
bks_day  <- read.csv('day.csv')

## set dataset you wish to do future modeling over
# (NOTE: hour is just a more refined version of day)
data = bks_hour
#data = bks_day

#---------------

## preprocessing before train/test split (can comment all to do no preprocessing)

# remove instance attribute (is the first attribute)
data = subset(data, select = -c(instant))

# dteday variable is almost entirely redundant as year and month are captured
#  the only semi-new information is the day, for now remove
data = subset(data, select = -c(dteday))

# Uncommenting line below shows that r sees some variables as integers
#  rather than categories or factors, should switch the ones we know
# str(data)
data$season     = factor(data$season)
data$holiday    = factor(data$holiday)
data$workingday = factor(data$workingday)
data$weathersit = factor(data$weathersit)

# unsure if other time variables should be considered ordered factors
# data$yr      = factor(data$yr, ordered = is.ordered(data$yr))
# data$mnth    = factor(data$mnth, ordered = is.ordered(data$mnth))
#  NOTE: hr is only in hr dataset, so make sure comment out if using day
# data$hr      = factor(data$hr, ordered = is.ordered(data$hr))
# data$weekday = factor(data$weekday, ordered = is.ordered(data$weekday))


#---------------

## visualizations of data

## correlation matrix
library(corrplot)
# create numeric variable dataframe
#  NOTE: temp & atemp are obviously correlated so kept only temp. Casual
#        and registered make up the count, thus they are removed from 
#        the correlation matrix too
data_numeric = subset(data, select = c(yr, mnth, hr, weekday, temp,
                                       hum, windspeed, cnt))
cor_mat = cor(data_numeric)
corrplot.mixed(cor_mat, order ="AOE")

# basic histograms of registered and casual counts
library(ggplot2)
library(gridExtra)
p1 = ggplot(data, aes(x = casual)) + geom_histogram()
p2 = ggplot(data, aes(x = registered)) + geom_histogram()
grid.arrange(p1,p2)

## 3-dimensional scatter plot
# set values to any variables in data (or make newones)
x_values = "temp"
y_values   = "cnt"
color    = "workingday"
ggplot(data, aes_string(x = x_values, y = y_values,
                         color = color)) + geom_point()

## box plots
# set values to any variables in data (or make newones)
x_values = "hr"
y_values = "cnt"
group    = "hr"
ggplot(data, aes_string(x = x_values, y = y_values, 
                         group = group)) + geom_boxplot()


#---------------

## set train and test data

# set random seed
set.seed(1)
# set % of data to be in train set
train_percent = 0.80
# load caret library
# (NOTE: has some helpful functions, mostly used here
#        to do stratified sampling)
library(caret)
# get training indices in stratified fashion
train_index = createDataPartition(data$cnt, p = train_percent,
                                   list = FALSE)
# create train/testing datasets
train = data[train_index, ]
test  = data[-train_index, ]

# create train/test data (w/o labels)
train_data = subset(train, select = -c(casual, registered, cnt))
test_data  = subset(test, select  = -c(casual, registered, cnt))

# create train data for casual, registered, and count (w/ labels)
#   NOTE: Needed since any two of these variables would give the third
train_cas_data = subset(train, select = -c(registered, cnt))
train_reg_data = subset(train, select = -c(casual, cnt))
train_cnt_data = subset(train, select = -c(casual, registered))

#---------------

## MLR

# casual
lm_cas.fit = lm(casual ~ ., data = train_cas_data)
lm_cas.pred = predict(lm_cas.fit, test)
lm_cas.rmse = sqrt(mean((test$casual - lm_cas.pred)^2))
lm_cas.rmse

# registered
lm_reg.fit = lm(registered ~ ., data = train_reg_data)
lm_reg.pred = predict(lm_reg.fit, test)
lm_reg.rmse = sqrt(mean((test$registered - lm_cas.pred)^2))
lm_reg.rmse

# count
lm_cnt.fit = lm(cnt ~ ., data = train_cnt_data)
lm_cnt.pred = predict(lm_cnt.fit, test)
lm_reg.rmse = sqrt(mean((test$cnt - lm_cas.pred)^2))
lm_reg.rmse

# compare count model to summation of casual and registered models
lm_sum.rmse = sqrt(mean((test$cnt - (lm_cas.pred + lm_reg.pred))^2))
lm_sum.rmse

#---------------

## Other regression models
## LASSO
## PCR
## PLS
## etc...
