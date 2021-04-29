#Dominiks
#Yuyu Fan#
library(tidyverse)
library(ggplot2)
library(forecast)
#step 1#
a<-arima.sim(n = 63, list(ar = c(0.8897, -0.4858), ma = c(-0.2279, 0.2488)),
          sd = sqrt(0.1796))

#step 2#
acf(a, lag.max = 48, plot = T) #q=2
pacf(a, lag.max = 48, plot = T) #p=2

#ARIMA model#
#create ts#
bakert_008<-ts (store_008$BAKERY, frequency = 7, start = -88)
bakert_008
decomposedRes <- decompose(bakert_008, type="mult") # use type = "additive" for additive components
plot (decomposedRes) # see plot below

#ACF & PACF#
acf(bakert_008, lag.max = 48, plot = T) #q=2
pacf(bakert_008, lag.max = 48, plot = T)
bakert_008 %>% diff() %>% ggtsdisplay(main="")

#de-seasonal#
ts.stl <- stl(bakert_008,"periodic")  # decompose the TS
ts.sa <- seasadj(ts.stl)  # de-seasonalize
plot(bakert_008, type="l")  # original series
plot(ts.sa, type="l")  # seasonal adjusted
seasonplot(ts.sa, 7, col=rainbow(7), year.labels=TRUE, main="Seasonal plot: Bakery")

#stational#
library(tseries)
adf.test(bakert_008) # p-value = 0.01 indicates the TS is stationary
kpss.test(bakert_008)

#seasonally differenced#
nsdiffs(bakert_008)  # number for seasonal differencing needed
#> 1
bakert_008_seasdiff <- diff(bakert_008, lag=frequency(bakert_008), differences=1)  # seasonal differencing
plot(bakert_008_seasdiff, type="l", main="Seasonally Differenced")  

bakert_008_seasdiff %>% diff() %>% ggtsdisplay(main="")
acf(bakert_008_seasdiff, lag.max = 48, plot = T) #q=1
pacf(bakert_008_seasdiff, lag.max = 48, plot = T) #p=6 
#ARIMA(6,7,1)