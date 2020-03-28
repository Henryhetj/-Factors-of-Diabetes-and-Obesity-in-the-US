library(readxl)
Population <- read_excel("Downloads/dataset/Population.xlsx", 
                           +     sheet = "Transpose")
library("zoo")
library('forecast')
population1<-Population[3]
x1<- ts(population1,start=1,frequency=1)
plot.ts(x1)
auto.arima(x1,trace=T)
AL1 <- arima(x1,order=c(0,2,0),seasonal=list(order=c(0,2,0),period=1),method="ML")
AL1
ALforecast <- forecast(AL1,h=5,level=c(99.5))
ALforecast

