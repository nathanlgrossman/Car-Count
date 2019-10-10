
print("BEGIN car_count")

library(zoo)
library(xts)
library(forecast)
library(lubridate)
library(forcats)
setwd("C:/Users/natha/OneDrive/Nathan/RStudioProjects/orbital_insight")
rm(list=ls())
options(max.print=150)

# Read data into dataframe
dataDF = read.csv("data.csv")

# Cast date column of dataframe as date data type
dataDF$date = as.Date(dataDF$date, format='%Y-%m-%d')

# Create XTS time series object, for plotting and later use
countXTS = xts(dataDF$car.count, dataDF$date)
plot(countXTS)

###########################################################################
# Decompose car count time series assuming an additive model with a trend #
# component and a seasonal component, of the form                         #
#     y(t) = b(t) + s(t) + e(t)                                           #
# where b(t) is the trend, s(t) is the seasonality and e(t) is the error  #
###########################################################################

# Assume the dominant seasonality (cyclicality) is weekly, so there are
# 7 (daily) observations per season (cycle)

# Create time series object under weekly seasonality assumption
countTS7 = ts(dataDF$car.count, freq=7)

# Decompose time series into trend and weekly seasonal components
countComps7 = decompose(countTS7, type="additive")
plot(countComps7)

# Assume the dominant seasonality (cyclicality) is annual, so there are
# 365.25 (daily) observations per season (cycle)

# Create time series object under annual seasonality assumption
countTS365 = ts(dataDF$car.count, freq=365.25)

# Decompose time series into trend and annual seasonal components
countComps365 = decompose(countTS365, type="additive")
plot(countComps365)

###########################################################################
# Decompose car count time series assuming an additive model with a trend #
# component and two seasonal components, using a TBATS state space model  #
# of the form                                                             #
#     y(t) = l(t-1) + phi * b(t-1) + s1(t) + s2(t) + e(t)                 #
# where l(t) is the level, phi is a damping parameter, b(t) is the trend, #
#       s1(t) and s2(t) are the seasonalities, and e(t) is the error      #
###########################################################################

# Create multi-seasonal time series object under weekly and annual
# seasonality assumption
countMSTS = msts(countXTS, seasonal.periods=c(7,365.25))

# Create TBATS state space model with trend, weekly and annual components
countTBATS = tbats(countMSTS)

# Extract trend, weekly and annual seasonal components
countCompsTBATS = tbats.components(countTBATS)
plot(countCompsTBATS)

###########################################################################
# Use the TBATS state space model to predict car counts as a function of  #
# trend, weekly and annual seasonal components                            #
###########################################################################

# Predict future values of car count with TBATS model, with trend, weekly
# and annual seasonal components
countFcastTBATS = forecast(countTBATS)
print("#################################################")
print("# TBATS TREND, WEEKLY AND ANNUAL SEASONAL MODEL #")
print("#################################################")
print(summary(countFcastTBATS))
plot(countFcastTBATS)

# Predict future values of car count TBATS model with trend and weekly
# seasonal components
# countSTS7 = msts(countXTS, seasonal.periods=c(7))
countSTS7 = ts(countXTS, freq=7)
countTBATS7 = tbats(countSTS7)
countFcastTBATS7 = forecast(countTBATS7)
print("#########################################")
print("# TBATS TREND AND WEEKLY SEASONAL MODEL #")
print("#########################################")
print(summary(countFcastTBATS7))
# plot(countFcastTBATS7)

# Predict future values of car count TBATS model with trend and annual
# seasonal components
# countSTS365 = msts(countXTS, seasonal.periods=c(365))
countSTS365 = ts(countXTS, freq=365)
countTBATS365 = tbats(countSTS365)
countFcastTBATS365 = forecast(countTBATS365)
print("#########################################")
print("# TBATS TREND AND ANNUAL SEASONAL MODEL #")
print("#########################################")
print(summary(countFcastTBATS365))
# plot(countFcastTBATS365)

###########################################################################
# Create a linear regression model to predict car count as a function of  #
# year, month, day of week and cloud indicator, of the form               #
#     y(t) = x1(t) + x2(t) + x3(t) + x4(t) + e(t)                         #
# where x1(t), x2(t), x3(t) and x(4) are categorical predictor variables  #
#       representing the year, month, day of week and cloud indicator,    #
#       and e(t) is the error                                             #
###########################################################################

# Create data frame comprising car count as a numerical variable, and
# year, month, day of week and cloud indicator as categorical variables
dataDF2 = data.frame(count=dataDF$car.count,
                     year = factor(year(dataDF$date)),
                     month = factor(month(dataDF$date)),
                     day = factor(wday(dataDF$date)),
                     cloud = factor(dataDF$cloud.indicator))

# Create linear regression model
linearModel = lm(count ~ year + month + day + cloud, data=dataDF2)
print("###################################")
print("# LINEAR REGRESSION MODEL SUMMARY #")
print("###################################")
print(summary(linearModel))

print("END car_count")
