getwd()
setwd("/Users/sandeepk/Desktop/DS604")
#Get forecast package
library(forecast)
#read the data
gap.data = read.csv("Gap-Sales-Data.csv")
#View top and bottom of data
head(gap.data)
tail(gap.data)
#Create Time series
gapsales.ts = ts(gap.data$SalesM,start=c(06,1),end=c(16,4), freq=4)
gapsales.ts
#plot data
plot(gapsales.ts,xlab="Time",ylab="Sales$M", ylim=c(2000,5000))
#Naive forecast           
snaive.pred = snaive(gapsales.ts,h=8,level=95)
snaive.pred
#Performance Metrics
accuracy(snaive.pred)
