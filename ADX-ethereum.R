


getSymbols("ETH-INR")



price <- Cl(`ETH-INR`)
r <- price/Lag(price) - 1
delta<-0.005
signal <-c(NA) # first signal is NA
for (i in 2: length(Cl(`ETH-INR`))){
  if (r[i] > delta){
    signal[i]<- 1
  } else if (r[i]< -delta){
    signal[i]<- -1
  } else
    signal[i]<- 0
}
signal<-reclass(signal,Cl(`ETH-INR`))
trade1 <- Lag(signal)
ret1<-dailyReturn(`ETH-INR`)*trade1
names(ret1) <- 'Naive'
charts.PerformanceSummary(ret1)

adx = ADX(`ETH-INR`[,2:4])
signal <- Lag(ifelse(adx$DIp < adx$DIn, -1, 1))
ret2<-dailyReturn(`ETH-INR`)*signal
names(ret2) <- 'ADX'
retall <- cbind(ret1, ret2)
charts.PerformanceSummary(retall,
                          main="Naive v.s. ADX for ETH-INR")

