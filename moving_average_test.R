getwd()
setwd("/Users/sandeepk/Desktop/DS604")
library(forecast)
library(zoo)
#Read the Data
exchange.data = read.csv("YenvsdollarExchangeRate.csv")

#View top and bottom of data
head(exchange.data)
tail(exchange.data)

#Create Time Series
exchange.ts = ts(exchange.data$ExchangeRate, start = c(1980,1), end = c(2017,1), freq = 4) 

#Moving Average Performance
ma.4 = rollmean(exchange.ts, k = 4, align = "right")
ma.4
ma.8 = rollmean(exchange.ts, k = 8, align = "right" )
ma.8

#Write to a CSV file
write.csv(ma.4, "Exchange_ma4.csv")