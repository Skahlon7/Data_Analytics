getwd()
setwd("/Users/sandeepk/Desktop/DS604")

library(forecast)

#Read Data
NewCarSales.Data = read.csv("Newcarsalesdata.csv")
NewCarSales.Data = as.data.frame(NewCarSales.Data)

#View Top and Bottom of Data
head(NewCarSales.Data)
tail(NewCarSales.Data)

#Perform Multiple Regression
NewCarSales.lm = lm(Sales~DPIPC + UR + PR + UMICS, NewCarSales.Data)

#Report Results ANOVA
summary(NewCarSales.lm)

#Create data frame for Feb 2017 data 
dataFeb2017 = data.frame(DPIPC=44141, UR=4.67, PR=3.79, UMICS=97.23 )

#Forecast Feb 2017 
predict(NewCarSales.lm,dataFeb2017)
