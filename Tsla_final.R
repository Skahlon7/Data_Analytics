#This program applies the Holt-Winters forecast and fits a logistic growth curve to public Tesla unit sales data
###############################################
# Written by: Sandeep Kahlon and Zain Mirza   #
# Date: 11/23/2021                            #
###############################################
getwd()
setwd("/Users/sandeepk/Desktop/DS604")
library(forecast)
library(zoo)
library(ggplot2)
library(readxl)
library(growthcurver)

#Read in Data
tsla = read_xlsx("Tesla2.xlsx")

#View top and bottom of Data
head(tsla)
tail(tsla)

#Create time series
tsla.ts = ts(tsla$`Units sold`,start = c(2013,1), end = c(2020,4), freq = 4)
tsla.ts

#Visualize Data
autoplot(tsla.ts, xlab = "Year", ylab = "Units Sold", main = "TESLA")
plot(tsla$Time, tsla$`Units sold`, main = "Time Series Scatter Plot",
     xlab = "Quarterly Report Since Public Offering (2013)",  
     ylab = "Units Sold")
ggplot(tsla, aes(x = Date, y = `Units sold`, fill = Date)) + 
  geom_point(shape = 21, size = 4, colour = "red") + ggtitle("Scatter Plot")
tsla$Date = format(as.Date(tsla$Date, format = "%d/%m/%Y"), "%Y")
ggplot(tsla, aes(x = Date, y = `Units sold`, fill = Date)) + geom_point(shape = 21, size = 4, colour = "red") 

#Logistic Curve
model.wt = SummarizeGrowth(tsla$Time, tsla$`Units sold`) 
model.wt$vals
predict(model.wt$model)
plot(model.wt, main = "Logistic Growth Curve", xlab = "Quarterly Report Since Public Offering (2013)",
     ylab = "Units Sold")

#Holt-Winters forecast
tslahw = HoltWinters(tsla.ts, seasonal = "multiplicative",)
tslahw
tslahw$fitted

#Plot Holt-Winters vs Actual
plot(tslahw)

#Forecast out for year 2022
tsla.pred = forecast(tslahw, h = 8, prediction.interval = T)
tsla.pred

#Performance Measures of Holt-Winters
accuracy(tsla.pred)

#Plot Holt-Winters Forecast
autoplot(tsla.ts) + 
  autolayer(tsla.pred, series="Holt-Winters Forecast", colour = "blue") +
  xlab("Years") + ylab("Units Sold(In hundred thousands)") + ylim(0,400000) +
  ggtitle("Holt-Winters Forecast of Tesla Unit Sales") 

