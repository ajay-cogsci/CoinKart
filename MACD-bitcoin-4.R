library("quantmod")
library("PerformanceAnalytics")
plot.macd = TRUE
ndrawdowns = 3
short.pos = 0 # size of short position. Set to 0 to disallow shorting


sym = "BTC-INR" # symbol to be tested -- can be changed


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


all.data = getSymbols(sym,auto.assign=FALSE)
data=all.data[,6] # column 6 to use distribution-adjusted closing prices
macd = MACD(data, nFast=12, nSlow=26,nSig=9,maType=SMA,percent = FALSE)
if (plot.macd) {
  chartSeries(all.data, TA=NULL)
  chartSeries(data, TA="addMACD()")
}
signal = Lag(ifelse(macd$macd < macd$signal, short.pos, 1))
returns = ROC(data)*signal
portfolio = exp(cumsum(returns))
table.Drawdowns(returns, top=ndrawdowns)
table.DownsideRisk(returns)
charts.PerformanceSummary(returns)




retall <- cbind(ret1, returns)
charts.PerformanceSummary(retall, 
                          main="Naive v.s. MACD")

