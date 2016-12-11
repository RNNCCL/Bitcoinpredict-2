##################################################################
################## Loading packages ##############################
##################################################################
require(quantmod)
library(xts)
library(forecast)
library(ggplot2)
library("TTR")
library(stats)
require(Quandl)



##################################################################
################## Feature creation ##############################
##################################################################
################# loading data into R #######################

getFX(c("BTC/USD"), source="google", from="2010-01-01", to = Sys.Date())
head(BTCUSD)



################# variable creation ##################

#log function
#BTClog <- log(BTCUSD)
BTClog <- BTCUSD

#head(rbind(BTClog, ts(as.Date("2016-11-20"),751.837)))

#BTClog[1828] <- "751.234"

#BTClog <- c(BTClog, xts(as.numeric(734.07), Sys.Date()))
BTClog <- c(BTClog, xts(as.numeric(752.82), as.Date("2016-12-05")))

#lags:
BTCdiff1 <- diff(BTClog, lag=1) /lag(BTClog,1)    
BTCdiff2 <- diff(BTClog, lag=2) /lag(BTClog,2) 
BTCdiff3 <- diff(BTClog, lag=3) /lag(BTClog,3) 
BTCdiff4 <- diff(BTClog, lag=4) /lag(BTClog,4) 
BTCdiff5 <- diff(BTClog, lag=5) /lag(BTClog,5) 
BTCdiff10 <- diff(BTClog, lag=10) /lag(BTClog,10) 
BTCdiff20 <-diff(BTClog, lag=20) /lag(BTClog,20) 

BTCdiff600 <-diff(BTClog, lag=600) /lag(BTClog,600)
plot(BTCdiff600)



#Bollinger bands
BTCbb <- BBands(BTClog[,1], n=20, SMA, sd = 2)
tail(BTCbb)

BTCbb40 <- BBands(BTClog[,1], n=40, SMA, sd = 2)
BTCbb60 <- BBands(BTClog[,1], n=60, SMA, sd = 2)
BTCbb100 <- BBands(BTClog[,1], n=100, SMA, sd = 2)

BTClog$lowerBB20 <- BTCbb$dn
BTClog$upperBB20 <- BTCbb$up
BTClog$sma20 <- BTCbb$mavg

BTClog$lowerBB40 <- BTCbb40$dn
BTClog$upperBB40 <- BTCbb40$up
BTClog$sma40 <- BTCbb40$mavg


BTClog$lowerBB60 <- BTCbb60$dn
BTClog$upperBB60 <- BTCbb60$up
BTClog$sma60 <- BTCbb60$mavg


BTClog$lowerBB100 <- BTCbb100$dn
BTClog$upperBB100 <- BTCbb100$up
BTClog$sma100 <- BTCbb100$mavg

BTClog$BTCvsBB20up <- BTClog$BTC.USD/BTCbb$up
BTClog$BTCvsBB20low <- BTClog$BTC.USD/BTCbb$dn
BTClog$BTCvsBB40up <- BTClog$BTC.USD/BTCbb40$up
BTClog$BTCvsBB40low <- BTClog$BTC.USD/BTCbb40$dn

BTClog$BTCvsBB60up <- BTClog$BTC.USD/BTCbb60$up
BTClog$BTCvsBB60low <- BTClog$BTC.USD/BTCbb60$dn

BTClog$BTCvsBB100up <- BTClog$BTC.USD/BTCbb100$up
BTClog$BTCvsBB100low <- BTClog$BTC.USD/BTCbb100$dn


#merging features:
my_data <- as.data.frame(merge(BTClog, BTCdiff1,BTCdiff2,BTCdiff3,BTCdiff4,BTCdiff5,BTCdiff10,BTCdiff20))

names(my_data)

dim(my_data)


extreme <- as.data.frame(ifelse(BTCdiff1 > abs(0.02), 1, 0))

more_extreme <- as.data.frame(ifelse(BTCdiff1 > abs(0.05), 1, 0))

colnames(extreme) <- "extreme"  

colnames(more_extreme) <- "more_extreme"

#RSI:
BTCrsi <- as.data.frame(RSI(BTClog[,1]))
plot(BTCrsi[,1])
names(BTCrsi)
class(BTCrsi)
my_data <- as.data.frame(cbind(my_data,BTCrsi, extreme, more_extreme))
names(my_data)
dim(my_data)

#MACD
macd <- as.data.frame(MACD(BTClog[,1], 12, 26, 9, maType="EMA"))
head(macd)

my_data <- as.data.frame(cbind(my_data,macd))

names(my_data)
dim(my_data)

plot(my_data$BTC.USD.5,my_data$EMA)

#Vertical Horizontal Filter
vhf10 <- as.data.frame(VHF(BTClog[,1],10))
vhf20 <- as.data.frame(VHF(BTClog[,1],20))
vhf40 <- as.data.frame(VHF(BTClog[,1],40))
vhf60 <- as.data.frame(VHF(BTClog[,1],60))
vhf100 <- as.data.frame(VHF(BTClog[,1],100))

colnames(vhf10)[1] <- "vhf10"
colnames(vhf20)[1] <- "vhf20"
colnames(vhf40)[1] <- "vhf40"
colnames(vhf60)[1] <- "vhf60"
colnames(vhf100)[1] <- "vhf100"


my_data <- as.data.frame(cbind(my_data, vhf10,vhf20,vhf40,vhf60,vhf100))

names(my_data)
dim(my_data)
#Commodity Channel Index

cci10 <- as.data.frame(CCI(BTClog[,1], n=10))
cci20 <- as.data.frame(CCI(BTClog[,1], n=20))
cci40 <- as.data.frame(CCI(BTClog[,1], n=40))
cci60 <- as.data.frame(CCI(BTClog[,1], n=60))
cci100 <- as.data.frame(CCI(BTClog[,1], n=100))

names(cci10)

colnames(cci10) <- "cci10"
colnames(cci20) <- "cci20"
colnames(cci40) <- "cci40"
colnames(cci60) <- "cci60"
colnames(cci100) <- "cci100"

my_data <- as.data.frame(cbind(my_data,cci10,cci20,cci40,cci60,cci100))
dim(my_data)

names(my_data)

#Aroon

aroon10 <- as.data.frame(aroon(BTClog[,1],10))
aroon20 <- as.data.frame(aroon(BTClog[,1],20))
aroon40 <- as.data.frame(aroon(BTClog[,1],40))
aroon60 <- as.data.frame(aroon(BTClog[,1],600))
aroon100 <- as.data.frame(aroon(BTClog[,1],100))

names(aroon100)

colnames(aroon10) <- c("aroonup10", "arroondn10", "aroonoscillator10")
colnames(aroon20) <- c("aroonup20", "arroondn20", "aroonoscillator20")
colnames(aroon40) <- c("aroonup40", "arroondn40", "aroonoscillator40")
colnames(aroon60) <- c("aroonup60", "arroondn60", "aroonoscillator60")
colnames(aroon100) <- c("aroonup100", "arroondn100", "aroonoscillator100")


my_data <- as.data.frame(cbind(my_data, aroon10,aroon20,aroon40,aroon60,aroon100))
dim(my_data)

#Chande Momentum Oscillator

cmo10 <- as.data.frame(CMO(BTClog[,1],n=10))
cmo20 <- as.data.frame(CMO(BTClog[,1],n=20))
cmo40 <- as.data.frame(CMO(BTClog[,1],n=40))
cmo60 <- as.data.frame(CMO(BTClog[,1],n=60))
cmo100 <- as.data.frame(CMO(BTClog[,1],n=100))

names(cmo20)

colnames(cmo10) <- "cm10"
colnames(cmo20) <- "cm20"
colnames(cmo40) <- "cm40"
colnames(cmo60) <- "cm60"
colnames(cmo100) <- "cm100"


my_data <- as.data.frame(cbind(my_data,cmo10,cmo20,cmo40,cmo60,cmo100))
dim(my_data)

#De-Trended Price Oscillator
dpo10 <- DPO(BTClog[,1],10) 
dpo20 <- DPO(BTClog[,1],20) 
dpo40 <- DPO(BTClog[,1],40) 
dpo60 <- DPO(BTClog[,1],60) 
dpo100 <- DPO(BTClog[,1],100) 
dpo5 <- DPO(BTClog[,1],5) 

cor(dpo40[1400:1800],BTCdiff5[1405:1805])

tail(my_data)
tail(dpo5)

#length(dpo5)
l#ength(BTCdiff1)

plot(dpo100)



#De-Trended Price Oscillator
#mirar



# Guppy Multiple Moving Averages
#bullshit


#Know Sure Thing
kst <- as.data.frame(KST(BTClog[,1]))
names(kst)

colnames(kst) <- c("kst", "kst_signal")
my_data <- as.data.frame(cbind(my_data,kst))

names(my_data)

dim(my_data)





# Running Percent Rank
PR5 <- as.data.frame(runPercentRank(BTClog[,1], n = 5))
PR10 <- as.data.frame(runPercentRank(BTClog[,1], n = 10))
PR20 <- as.data.frame(runPercentRank(BTClog[,1], n = 20))
PR40 <- as.data.frame(runPercentRank(BTClog[,1], n = 40))
PR60 <- as.data.frame(runPercentRank(BTClog[,1], n = 60))
PR100 <- as.data.frame(runPercentRank(BTClog[,1], n = 100))

colnames(PR5) <- "PR5"
colnames(PR10) <- "PR10"
colnames(PR20) <- "PR20"
colnames(PR40) <- "PR40"
colnames(PR60) <- "PR60"
colnames(PR100) <- "PR100"

my_data <- as.data.frame(cbind(my_data,PR5,PR10,PR20,PR40,PR60,PR100))


dim(my_data)
#hasta aqui todo ok



# Running Max
RM5 <- as.data.frame(runMax(BTClog[,1],5))
RM10 <- as.data.frame(runMax(BTClog[,1],10))
RM20 <- as.data.frame(runMax(BTClog[,1],20))
RM40 <- as.data.frame(runMax(BTClog[,1],40))
RM60 <- as.data.frame(runMax(BTClog[,1],60))
RM100 <- as.data.frame(runMax(BTClog[,1],100))


colnames(RM5) <- "RM5"
colnames(RM10) <- "RM10"
colnames(RM20) <- "RM20"
colnames(RM40) <- "RM40"
colnames(RM60) <- "RM60"
colnames(RM100) <- "RM100"



my_data <- as.data.frame(cbind(my_data,RM5, RM10,RM20, RM40, RM60, RM100))


dim(my_data)

# Running Min
RMin5 <- as.data.frame(runMin(BTClog[,1],5))
RMin10 <- as.data.frame(runMin(BTClog[,1],10))
RMin20 <- as.data.frame(runMin(BTClog[,1],20))
RMin40 <- as.data.frame(runMin(BTClog[,1],40))
RMin60 <- as.data.frame(runMin(BTClog[,1],60))
RMin100 <- as.data.frame(runMin(BTClog[,1],100))


colnames(RMin5) <- "RMin5"
colnames(RMin10) <- "RMin10"
colnames(RMin20) <- "RMin20"
colnames(RMin40) <- "RMin40"
colnames(RMin60) <- "RMin60"
colnames(RMin100) <- "RMin100"


my_data <- as.data.frame(cbind(my_data, RMin5, RMin10, RMin20, RMin40, RMin60, RMin100))

dim(my_data)



# Running SD of lags

SDlag15 <- as.data.frame(runSD(BTCdiff1, 5))  
SDlag110 <- as.data.frame(runSD(BTCdiff1, 10))  
SDlag120 <- as.data.frame(runSD(BTCdiff1, 20))  
SDlag140 <- as.data.frame(runSD(BTCdiff1, 40))  
SDlag160 <- as.data.frame(runSD(BTCdiff1, 60))  
SDlag1100 <- as.data.frame(runSD(BTCdiff1, 100))  


colnames(SDlag15) <- "sdlag15"
colnames(SDlag110) <- "sdlag110"
colnames(SDlag120) <- "sdlag120"
colnames(SDlag140) <- "sdlag140"
colnames(SDlag160) <- "sdlag160"
colnames(SDlag1100) <- "sdlag1100"


my_data <- as.data.frame(cbind(my_data, SDlag15, SDlag110, SDlag120, SDlag140, SDlag160, SDlag1100))

dim(my_data)
dim(SDlag1100)

#hasta aqui ok


SDlag25 <- as.data.frame(runSD(BTCdiff2, 5))  
SDlag210 <- as.data.frame(runSD(BTCdiff2, 10))  
SDlag220 <- as.data.frame(runSD(BTCdiff2, 20))  
SDlag240 <- as.data.frame(runSD(BTCdiff2, 40))  
SDlag260 <- as.data.frame(runSD(BTCdiff2, 60))  
SDlag2100 <- as.data.frame(runSD(BTCdiff2, 100))  


colnames(SDlag25) <- "sdlag25"
colnames(SDlag210) <- "sdlag210"
colnames(SDlag220) <- "sdlag220"
colnames(SDlag240) <- "sdlag240"
colnames(SDlag260) <- "sdlag260"
colnames(SDlag2100) <- "sdlag2100"

my_data <- as.data.frame(cbind(my_data, SDlag25, SDlag210, SDlag220, SDlag240, SDlag260, SDlag2100))


#names(my_data)

dim(my_data)




SDlag35 <- runSD(BTCdiff3, 5)  
SDlag310 <- runSD(BTCdiff3, 10)  
SDlag320 <- runSD(BTCdiff3, 20)  
SDlag340 <- runSD(BTCdiff3, 40)  
SDlag360 <- runSD(BTCdiff3, 60)  
SDlag3100 <- runSD(BTCdiff3, 100)  



colnames(SDlag35) <- "sdlag35"
colnames(SDlag310) <- "sdlag310"
colnames(SDlag320) <- "sdlag320"
colnames(SDlag340) <- "sdlag340"
colnames(SDlag360) <- "sdlag360"
colnames(SDlag3100) <- "sdlag3100"

dim(my_data)
str(my_data)

my_data <- as.data.frame(cbind(my_data, as.data.frame(cbind( SDlag35, SDlag310, SDlag320, SDlag340, SDlag360, SDlag3100))))

dim(my_data)
dim(SDlag35)

#dim(cbind(my_data,as.data.frame(SDlag310[,1]))



#### !!!! aqui esta el problema


SDlag45 <- runSD(BTCdiff4, 5)  
SDlag410 <- runSD(BTCdiff4, 10)  
SDlag420 <- runSD(BTCdiff4, 20)  
SDlag440 <- runSD(BTCdiff4, 40)  
SDlag460 <- runSD(BTCdiff4, 60)  
SDlag4100 <- runSD(BTCdiff4, 100)  



colnames(SDlag45) <- "sdlag45"
colnames(SDlag410) <- "sdlag410"
colnames(SDlag420) <- "sdlag420"
colnames(SDlag440) <- "sdlag440"
colnames(SDlag460) <- "sdlag460"
colnames(SDlag4100) <- "sdlag4100"

dim(my_data)
str(my_data)

my_data <- as.data.frame(cbind(my_data, as.data.frame(cbind( SDlag45, SDlag410, SDlag420, SDlag440, SDlag460, SDlag4100))))


dim(my_data)



SDlag55 <- runSD(BTCdiff5, 5)  
SDlag510 <- runSD(BTCdiff5, 10)  
SDlag520 <- runSD(BTCdiff5, 20)  
SDlag540 <- runSD(BTCdiff5, 40)  
SDlag560 <- runSD(BTCdiff5, 60)  
SDlag5100 <- runSD(BTCdiff5, 100)  



colnames(SDlag55) <- "sdlag55"
colnames(SDlag510) <- "sdlag510"
colnames(SDlag520) <- "sdlag520"
colnames(SDlag540) <- "sdlag540"
colnames(SDlag560) <- "sdlag560"
colnames(SDlag5100) <- "sdlag5100"

dim(my_data)
str(my_data)

my_data <- as.data.frame(cbind(my_data, as.data.frame(cbind( SDlag55, SDlag510, SDlag520, SDlag540, SDlag560, SDlag5100))))

dim(my_data)



SDlag105 <- runSD(BTCdiff10, 5)  
SDlag1010 <- runSD(BTCdiff10, 10)  
SDlag1020 <- runSD(BTCdiff10, 20)  
SDlag1040 <- runSD(BTCdiff10, 40)  
SDlag1060 <- runSD(BTCdiff10, 60)  
SDlag10100 <- runSD(BTCdiff10, 100)  




colnames(SDlag105) <- "sdlag105"
colnames(SDlag1010) <- "sdlag1010"
colnames(SDlag1020) <- "sdlag1020"
colnames(SDlag1040) <- "sdlag1040"
colnames(SDlag1060) <- "sdlag1060"
colnames(SDlag10100) <- "sdlag10100"

dim(my_data)
str(my_data)

my_data <- as.data.frame(cbind(my_data, as.data.frame(cbind( SDlag105, SDlag1010, SDlag1020, SDlag1040, SDlag1060, SDlag10100))))

dim(my_data)








SDlag205 <- runSD(BTCdiff20, 5)  
SDlag2010 <- runSD(BTCdiff20, 10)  
SDlag2020 <- runSD(BTCdiff20, 20)  
SDlag2040 <- runSD(BTCdiff20, 40)  
SDlag2060 <- runSD(BTCdiff20, 60)  
SDlag20100 <- runSD(BTCdiff20, 100)  




colnames(SDlag205) <- "sdlag205"
colnames(SDlag2010) <- "sdlag2010"
colnames(SDlag2020) <- "sdlag2020"
colnames(SDlag2040) <- "sdlag2040"
colnames(SDlag2060) <- "sdlag2060"
colnames(SDlag20100) <- "sdlag20100"

dim(my_data)
str(my_data)

my_data <- as.data.frame(cbind(my_data, as.data.frame(cbind( SDlag205, SDlag2010, SDlag2020, SDlag2040, SDlag2060, SDlag20100))))

dim(my_data)







# Running Median of lags (differences = returns)
Mdlag15 <- runMedian(BTCdiff1, 5)  
Mdlag110 <- runMedian(BTCdiff1, 10)  
Mdlag120 <- runMedian(BTCdiff1, 20)  
Mdlag140 <- runMedian(BTCdiff1, 40)  
Mdlag160 <- runMedian(BTCdiff1, 60)  
Mdlag1100 <- runMedian(BTCdiff1, 100)  




colnames(Mdlag15) <- "Mdlag15"
colnames(Mdlag110) <- "Mdlag110"
colnames(Mdlag120) <- "Mdlag120"
colnames(Mdlag140) <- "Mdlag140"
colnames(Mdlag160) <- "Mdlag160"
colnames(Mdlag1100) <- "Mdlag1100"

dim(my_data)
str(my_data)

my_data <- as.data.frame(cbind(my_data, as.data.frame(cbind( Mdlag15, Mdlag110, Mdlag120, Mdlag140, Mdlag160, Mdlag1100))))

dim(my_data)





Mdlag25 <- runMedian(BTCdiff2, 5)  
Mdlag210 <- runMedian(BTCdiff2, 10)  
Mdlag220 <- runMedian(BTCdiff2, 20)  
Mdlag240 <- runMedian(BTCdiff2, 40)  
Mdlag260 <- runMedian(BTCdiff2, 60)  
Mdlag2100 <- runMedian(BTCdiff2, 100)  



colnames(Mdlag25) <- "Mdlag25"
colnames(Mdlag210) <- "Mdlag210"
colnames(Mdlag220) <- "Mdlag220"
colnames(Mdlag240) <- "Mdlag240"
colnames(Mdlag260) <- "Mdlag260"
colnames(Mdlag2100) <- "Mdlag2100"

dim(my_data)
str(my_data)

my_data <- as.data.frame(cbind(my_data, as.data.frame(cbind( Mdlag25, Mdlag210, Mdlag220, Mdlag240, Mdlag260, Mdlag2100))))

dim(my_data)






Mdlag35 <- runMedian(BTCdiff3, 5)  
Mdlag310 <- runMedian(BTCdiff3, 10)  
Mdlag320 <- runMedian(BTCdiff3, 20)  
Mdlag340 <- runMedian(BTCdiff3, 40)  
Mdlag360 <- runMedian(BTCdiff3, 60)  
Mdlag3100 <- runMedian(BTCdiff3, 100)  




colnames(Mdlag35) <- "Mdlag35"
colnames(Mdlag310) <- "Mdlag310"
colnames(Mdlag320) <- "Mdlag320"
colnames(Mdlag340) <- "Mdlag340"
colnames(Mdlag360) <- "Mdlag360"
colnames(Mdlag3100) <- "Mdlag3100"

dim(my_data)
str(my_data)

my_data <- as.data.frame(cbind(my_data, as.data.frame(cbind( Mdlag35, Mdlag310, Mdlag320, Mdlag340, Mdlag360, Mdlag3100))))

dim(my_data)




Mdlag45 <- runMedian(BTCdiff4, 5)  
Mdlag410 <- runMedian(BTCdiff4, 10)  
Mdlag420 <- runMedian(BTCdiff4, 20)  
Mdlag440 <- runMedian(BTCdiff4, 40)  
Mdlag460 <- runMedian(BTCdiff4, 60)  
Mdlag4100 <- runMedian(BTCdiff4, 100)  


colnames(Mdlag45) <- "Mdlag45"
colnames(Mdlag410) <- "Mdlag410"
colnames(Mdlag420) <- "Mdlag420"
colnames(Mdlag440) <- "Mdlag440"
colnames(Mdlag460) <- "Mdlag460"
colnames(Mdlag4100) <- "Mdlag4100"

dim(my_data)
str(my_data)

my_data <- as.data.frame(cbind(my_data, as.data.frame(cbind( Mdlag45, Mdlag410, Mdlag420, Mdlag440, Mdlag460, Mdlag4100))))

dim(my_data)





Mdlag55 <- runMedian(BTCdiff5, 5)  
Mdlag510 <- runMedian(BTCdiff5, 10)  
Mdlag520 <- runMedian(BTCdiff5, 20)  
Mdlag540 <- runMedian(BTCdiff5, 40)  
Mdlag560 <- runMedian(BTCdiff5, 60)  
Mdlag5100 <- runMedian(BTCdiff5, 100)  




colnames(Mdlag55) <- "Mdlag55"
colnames(Mdlag510) <- "Mdlag510"
colnames(Mdlag520) <- "Mdlag520"
colnames(Mdlag540) <- "Mdlag540"
colnames(Mdlag560) <- "Mdlag560"
colnames(Mdlag5100) <- "Mdlag5100"

dim(my_data)
str(my_data)

my_data <- as.data.frame(cbind(my_data, as.data.frame(cbind( Mdlag55, Mdlag510, Mdlag520, Mdlag540, Mdlag560, Mdlag5100))))





Mdlag105 <- runMedian(BTCdiff10, 5)  
Mdlag1010 <- runMedian(BTCdiff10, 10)  
Mdlag1020 <- runMedian(BTCdiff10, 20)  
Mdlag1040 <- runMedian(BTCdiff10, 40)  
Mdlag1060 <- runMedian(BTCdiff10, 60)  
Mdlag10100 <- runMedian(BTCdiff10, 100) 



colnames(Mdlag105) <- "Mdlag105"
colnames(Mdlag1010) <- "Mdlag1010"
colnames(Mdlag1020) <- "Mdlag1020"
colnames(Mdlag1040) <- "Mdlag1040"
colnames(Mdlag1060) <- "Mdlag1060"
colnames(Mdlag10100) <- "Mdlag10100"

dim(my_data)
str(my_data)

my_data <- as.data.frame(cbind(my_data, as.data.frame(cbind( Mdlag105, Mdlag1010, Mdlag1020, Mdlag1040, Mdlag1060, Mdlag10100))))




Mdlag205 <- runMedian(BTCdiff20, 5)  
Mdlag2010 <- runMedian(BTCdiff20, 10)  
Mdlag2020 <- runMedian(BTCdiff20, 20)  
Mdlag2040 <- runMedian(BTCdiff20, 40)  
Mdlag2060 <- runMedian(BTCdiff20, 60)  
Mdlag20100 <- runMedian(BTCdiff20, 100) 


colnames(Mdlag205) <- "Mdlag205"
colnames(Mdlag2010) <- "Mdlag2010"
colnames(Mdlag2020) <- "Mdlag2020"
colnames(Mdlag2040) <- "Mdlag2040"
colnames(Mdlag2060) <- "Mdlag2060"
colnames(Mdlag20100) <- "Mdlag20100"


str(my_data)

my_data <- as.data.frame(cbind(my_data, as.data.frame(cbind( Mdlag205, Mdlag2010, Mdlag2020, Mdlag2040, Mdlag2060, Mdlag20100))))

dim(my_data)


#plot(Mdlag20100)


# Moving averages of differences

sma15 <- SMA(BTCdiff1, 5)
sma110 <- SMA(BTCdiff1, 10)
sma120 <- SMA(BTCdiff1, 20)
sma140 <- SMA(BTCdiff1, 40)
sma160 <- SMA(BTCdiff1, 60)
sma1100 <- SMA(BTCdiff1, 100)





colnames(sma15) <- "sma15"
colnames(sma110) <- "sma110"
colnames(sma120) <- "sma120"
colnames(sma140) <- "sma140"
colnames(sma160) <- "sma160"
colnames(sma1100) <- "sma1100"


str(my_data)

my_data <- as.data.frame(cbind(my_data, as.data.frame(cbind( sma15, sma110, sma120, sma140, sma160, sma1100))))

dim(my_data)



sma105 <- SMA(BTCdiff10, 5)
sma1010 <- SMA(BTCdiff10, 10)
sma1020 <- SMA(BTCdiff10, 20)
sma1040 <- SMA(BTCdiff10, 40)
sma1060 <- SMA(BTCdiff10, 60)
sma10100 <- SMA(BTCdiff10, 100)



colnames(sma105) <- "sma105"
colnames(sma1010) <- "sma1010"
colnames(sma1020) <- "sma1020"
colnames(sma1040) <- "sma1040"
colnames(sma1060) <- "sma1060"
colnames(sma10100) <- "sma10100"



my_data <- as.data.frame(cbind(my_data, as.data.frame(cbind( sma105, sma1010, sma1020, sma1040, sma1060, sma10100))))

dim(my_data)


###adding quandl:

#number of bitcoins created
volume_total <- Quandl("BCHAIN/NTRAT", start_date = "2011-12-05", order="asc") 

#volume_total <- read.csv("n-transactions-total", header = F)

names(volume_total) <- c("dates", "Value")

volume <- as.data.frame(tail(volume_total$Value,1828))

colnames(volume) <- "volume"

#revenue
revenue_total <- Quandl("BCHAIN/MIREV", start_date = "2011-12-01", order="asc") 

#revenue_total <- read.csv("miners-revenue", header = F)

names(revenue_total) <- c("dates", "Value")

revenue <- as.data.frame(tail(revenue_total$Value,1828))

colnames(revenue) <- "revenue"


#number of transactions per day
transactions_total <- Quandl("BCHAIN/NTRAN", start_date = "2011-12-01", order="asc") 

#transactions_total <- read.csv("n-transactions", header = F)

names(transactions_total) <- c("dates", "Value")

transactions <- as.data.frame(tail(revenue_total$Value,1828))

colnames(transactions) <- "transactions"



my_data <- as.data.frame(cbind(my_data[1:1827,], volume, revenue, transactions))




#playing with volume


# MFI
mfi5 <- MFI(BTClog$BTC.USD, transactions$transactions, n = 5)

mfi10 <- MFI(BTClog$BTC.USD, transactions$transactions, n = 10)

mfi20 <- MFI(BTClog$BTC.USD, transactions$transactions, n = 20)

mfi40 <- MFI(BTClog$BTC.USD, transactions$transactions, n = 40)

mfi60 <- MFI(BTClog$BTC.USD, transactions$transactions, n = 60)

mfi100 <- MFI(BTClog$BTC.USD, transactions$transactions, n = 100)

#OBV
OBV_BTC <- OBV(BTClog$BTC.USD, transactions$transactions)




my_data <- as.data.frame(cbind(my_data, OBV_BTC[1:1828], mfi5[1:1828,], mfi10[1:1828,], mfi20[1:1828,], mfi40[1:1828,], mfi60[1:1828,], mfi100[1:1828,]))



### offsetting by one day (or more)

predict_value <- BTCUSD[8:1827]

colnames(predict_value) <- "BTC_next_day"

total_data <- cbind(my_data[1:1820,], as.data.frame(predict_value))


##################################################################
################## Model building # ##############################
##################################################################









set.seed(3)

#drops <- c("BTC.USD", "aroonup60", "arroondn60", "aroonoscillator60")

drops <- c( "aroonup60", "arroondn60", "aroonoscillator60")

load_data <- total_data[120:nrow(total_data),]

matrix_data <- load_data[,!(names(load_data) %in% drops)]




#simple LM model


modelv1 <- lm( BTC_next_day ~. -BTC.USD , data = matrix_data )


summary(modelv1)




#################### Random Forest #####################

require(quantregForest)


#training and test set:

train = sample(1:nrow(load_data), size = round(0.6*nrow(load_data)), replace=FALSE)

train_set <- matrix_data[train,]
test_set <- matrix_data[-train,]



################# Feature selection with Boruta ##################3

'''
### feature selection with Boruta:
require(Boruta)  
set.seed(4)

boruta.train <- Boruta(train_set$BTC_next_day ~. 
, data = train_set[,1:186]
, doTrace = 2
, maxRuns = 40)

plot(boruta.train)  


relevant_features <- getSelectedAttributes(boruta.train)'''


qf1 <- quantregForest(y = train_set$BTC_next_day 
                      , x = train_set[,!(names(train_set) %in% c("BTC_next_day"))]
                      , ntree = 5000
                      #, mtry = 40
                      , corr.bias = T
                      , do.trace = T
                      , keep.inbag = TRUE
)


oobQuantiles <- predict(qf1, what=c(0.01,0.05, 0.5, 0.95,0.99) )
oobQuantiles
head(qf1$inbag)


#mse
qf1$mse

#y values
qf1$y

#plotting oob results
plot(qf1$y,type="p",col="red", pch=3, cex=0.1)
points(oobQuantiles[,1],col="green", cex=0.1)
points(oobQuantiles[,5],col="blue", cex=0.1)

#checking how often OOB results above 5% quantile:
sum(ifelse(qf1$y>oobQuantiles[,2],1,0))/length(qf1$y) #97%-99%


#checking how often OOB results above 1% quantile:
sum(ifelse(qf1$y>oobQuantiles[,1],1,0))/length(qf1$y) #100%


#checking how often OOB results below median:
sum(ifelse(qf1$y>oobQuantiles[,3],1,0))/length(qf1$y) #32%

#checking how often OOB results below 95% quantile:
sum(ifelse(qf1$y<oobQuantiles[,4],1,0))/length(qf1$y) #99%


#checking how often OOB results below 99% quantile:
sum(ifelse(qf1$y<oobQuantiles[,5],1,0))/length(qf1$y) #99%


##################################################################
################## Cross validation ##############################
##################################################################


#Cross validation:
testy <- predict (qf1, newdata= test_set, what=c(0.01, 0.05, 0.5, 0.95,0.99))
head(testy)

meany <- predict (qf1, newdata= test_set, what=mean)

#checking how often OOB results above 5% quantile:
sum(ifelse(test_set$BTC_next_day>testy[,2],1,0))/length(test_set$BTC_next_day) #97%-99%


#checking how often OOB results above 1% quantile:
sum(ifelse(test_set$BTC_next_day>testy[,1],1,0))/length(test_set$BTC_next_day) #100%


#checking how often OOB results below median:
sum(ifelse(test_set$BTC_next_day<testy[,3],1,0))/length(test_set$BTC_next_day) #32%

#checking how often OOB results below 95% quantile:
sum(ifelse(test_set$BTC_next_day<testy[,4],1,0))/length(test_set$BTC_next_day) #99%


#checking how often OOB results below 99% quantile:
sum(ifelse(test_set$BTC_next_day<testy[,5],1,0))/length(test_set$BTC_next_day) #99%




#plotting oob results
plot(qf1$y,type="p",col="red", pch=3, cex=0.1)
points(oobQuantiles[,1],col="green", cex=0.1)
points(oobQuantiles[,5],col="blue", cex=0.1)


plot( test_set$BTC_next_day,test_set$BTC_next_day , pch=3, cex=0.5 )
#plot(testy[,2], test_set$BTC_next_day, pch=3, cex=0.5 )
points(test_set$BTC_next_day,testy[,5],col="green", cex=0.5)
points(test_set$BTC_next_day,testy[,1],col="blue", cex=0.5)
points(test_set$BTC_next_day,meany,col="yellow", cex=0.5)
points(test_set$BTC_next_day,testy[,3],col="red", cex=0.5)



#### assessing model performance - mean prediction:

results <- as.data.frame(cbind(meany, test_set$BTC_next_day))


colnames(results) <- c("prediction", "real_value") #, "error", "error_total_rounded")

results$error <- results$prediction - results$real_value
results$relative_error <- results$error / results$real_value



plot(results$prediction,results$error)
plot(results$prediction,results$relative_error)
plot(results$prediction,results$real_value)


mean(results$error)
sd(results$error)
mad(results$error)
t.test(results$error)

# mean absolute deviation for losses
mean(results$error[results$error>0])
mean(results$relative_error[results$relative_error>0])


results[which.max(results$error),]
results[which.min(results$error),]


hist(results$error,1000)

boxplot(results$error)


require(stats)
shapiro.test(results$relative_error)
qqnorm(results$relative_error)
qqline(results$relative_error)



#### assessing model performance - median prediction:

results <- as.data.frame(cbind(testy[,3], test_set$BTC_next_day))


colnames(results) <- c("prediction", "real_value") #, "error", "error_total_rounded")

results$error <- results$prediction - results$real_value
results$relative_error <- results$error / results$real_value



plot(results$prediction,results$error)
plot(results$prediction,results$relative_error)
plot(results$prediction,results$real_value)


mean(results$error)
sd(results$error)
mad(results$error)
t.test(results$error)



results[which.max(results$error),]
results[which.min(results$error),]


hist(results$error,1000)

boxplot(results$error)


require(stats)
shapiro.test(results$relative_error)
qqnorm(results$relative_error)
qqline(results$relative_error)




##################################################################
################## Building final model###########################
##################################################################


qf_final <- quantregForest(y = matrix_data$BTC_next_day 
                           , x = matrix_data[,!(names(matrix_data) %in% c("BTC_next_day"))]
                           , ntree = 5000
                           #, mtry = 40
                           , corr.bias = T
                           , do.trace = T
                           , keep.inbag = TRUE
)



oobQuantiles <- predict(qf1, what=c(0.01,0.05, 0.5, 0.95,0.99) )
oobQuantiles




#plotting oob results
plot(qf1$y,type="p",col="red", pch=3, cex=0.1)
points(oobQuantiles[,1],col="green", cex=0.1)
points(oobQuantiles[,5],col="blue", cex=0.1)

#checking how often OOB results above 5% quantile:
sum(ifelse(qf1$y>oobQuantiles[,2],1,0))/length(qf1$y) #97%-99%


#checking how often OOB results above 1% quantile:
sum(ifelse(qf1$y>oobQuantiles[,1],1,0))/length(qf1$y) #100%


#checking how often OOB results below median:
sum(ifelse(qf1$y>oobQuantiles[,3],1,0))/length(qf1$y) #32%

#checking how often OOB results below 95% quantile:
sum(ifelse(qf1$y<oobQuantiles[,4],1,0))/length(qf1$y) #99%


#checking how often OOB results below 99% quantile:
sum(ifelse(qf1$y<oobQuantiles[,5],1,0))/length(qf1$y) #99%


#making final prediction:

pred_sevenday_quant <- predict(qf_final, newdata = my_data[1828,], what = c(0.01, 0.025, 0.1, 0.25, 0.5,0.75, 0.95, 0.975, 0.99))
pred_sevenday_mean <- predict(qf_final, newdata = my_data[1828,], what = mean)


pred_sevenday_quant
pred_sevenday_mean

pred_seven_day_cdf <- predict(qf_final, newdata = my_data[1828,], what = seq(0.01, 1, 0.001))
plot(seq(0.01, 1, 0.001),pred_seven_day_cdf )

hist(pred_seven_day_cdf, 200 )


