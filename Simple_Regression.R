getwd()
setwd("/Users/sandeepk/Desktop/DS604")
library(forecast)

#Read the Data
Sales.data = read.csv("Alex_sales.csv")

#View top and bottom of Data
head(Sales.data)
tail(Sales.data)

#Create time series
Sales.ts = ts(Sales.data$Sales, start = c(2012,1), end = c(2016,4), freq = 4)

#Plot the data
plot(Sales.ts)

#Perform linear regression
Sales.lm = tslm(Sales.ts~trend)

#Report Results
summary(Sales.lm)

#Forecast
Sales.lm.pred = forecast(Sales.lm, h=4)
Sales.lm.pred

#Write to csv file
write.csv(Sales.lm.pred,"Alex_forecast.csv")