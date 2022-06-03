library(quantmod)
library(PerformanceAnalytics)
getSymbols(`BTC-INR`, src='yahoo')




#TRADE 1 - SIMPLE BUY-SELL FILTER

price <-  `BTC-INR`[,4]

r <- price/Lag(price) - 1
delta<-0.005
signal <-c(NA) # first signal is NA

for (i in 2: length(Cl(`BTC-INR`))){
  if (r[i] > delta){
    signal[i]<- 1
  } else if (r[i]< -delta){
    signal[i]<- -1
  } else
    signal[i]<- 0
}
signal<-reclass(signal,Cl(`BTC-INR`))

trade1 <- Lag(signal)
ret1<-dailyReturn(`BTC-INR`)*trade1
names(ret1) <- 'Naive'
charts.PerformanceSummary(ret1)

#TRADE 2 - RSI

day <-14


price <-  `BTC-INR`[,4]

signal <- c()                    #initialize vector
rsi <- RSI(price, day)     #rsi is the lag of RSI
signal [1:day+1] <- 0            #0 because no signal until day+1

for (i in (day+1): length(price)){
  if (rsi[i] < 30){             #buy if rsi < 30
    signal[i] <- 1
  }else {                       #no trade all if rsi > 30
    signal[i] <- 0
  }
}
signal<-reclass(signal,Cl(`BTC-INR`))
trade2 <- Lag(signal)

#construct a new variable ret1
ret1 <- dailyReturn(`BTC-INR`)*trade1
names(ret1) <- 'Naive'
# construct a new variable ret2
ret2 <- dailyReturn(`BTC-INR`)*trade2
names(ret2) <- 'RSI'

retall <- cbind(ret1, ret2)
charts.PerformanceSummary(retall, 
                          main="Naive v.s. RSI")

















