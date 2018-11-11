library(xml2)
library(rvest)
library(dplyr)
library(BatchGetSymbols)

first.date <- Sys.Date()-365
last.date <- Sys.Date()

df.SP500 <- GetSP500Stocks()
tickers <- df.SP500$tickers

l.out <- BatchGetSymbols(tickers = tickers,first.date = first.date,last.date = last.date)
HistoricalSP500 <- l.out
load("C:/Users/Pooya/Desktop/Core/Library/CityFalcon/Data/HistoricalSP500.rda")


load("C:/Users/Pooya/Desktop/Core/Library/CityFalcon/Data/Cleaned1.rda")
Cleaned1 <- Cleaned
load("C:/Users/Pooya/Desktop/Core/Library/CityFalcon/Data/Cleaned2.rda")
Cleaned2 <- Cleaned
load("C:/Users/Pooya/Desktop/Core/Library/CityFalcon/Data/Cleaned3.rda")
Cleaned3 <- Cleaned


Cleaned <- rbind(Cleaned1,Cleaned2,Cleaned3)
Cl <- Cleaned[!duplicated(Cleaned),]
TotalCleaned <- Cl
save(TotalCleaned,file = "C:/Users/Pooya/Desktop/Core/Library/CityFalcon/Data/TotalCleaned.rda")




load("C:/Users/Pooya/Desktop/Core/Library/CityFalcon/Data/HistoricalSP500.rda")
load("C:/Users/Pooya/Desktop/Core/Library/CityFalcon/Data/TotalCleaned.rda")

Story <- paste(TotalCleaned$ticker,TotalCleaned$titles,TotalCleaned$descritions,sep = "-")
pub.date <- substr(TotalCleaned$publishDate,1,10)

library(zoo)
library(xts)
library(TTR)
library(quantmod)

tar <- as.Date(pub.date)
newz <- xts(data.frame(Story,TotalCleaned$ticker),order.by = tar)
load("C:/Users/Pooya/Desktop/Core/Library/CityFalcon/Data/newz.rda")

tickerName <- as.character(HistoricalSP500$df.control$ticker)


validtickers <- tickerName[tickerName %in% SP500$Symbol]
n <- length(validtickers)

b <- HistoricalSP500$df.tickers$ticker
HistXTS <- list()
for (i in 1:n) {
  d <- which(b == validtickers[i])
  Open <- HistoricalSP500$df.tickers$price.open[d]
  High <- HistoricalSP500$df.tickers$price.high[d]
  Low <- HistoricalSP500$df.tickers$price.low[d]
  Close <- HistoricalSP500$df.tickers$price.close[d]
  Volume <- HistoricalSP500$df.tickers$volume[d]
  Adjusted <- HistoricalSP500$df.tickers$price.adjusted[d]
  HistXTS[[validtickers[i]]] <- xts(data.frame(Open=Open,High=High,Low=Low,Close=Close,Volume=Volume,Adjusted=Adjusted),order.by = HistoricalSP500$df.tickers$ref.date[d])
}

save(HistXTS,file = "C:/Users/Pooya/Desktop/Core/Library/CityFalcon/Data/HistXTS.rda")
load("C:/Users/Pooya/Desktop/Core/Library/CityFalcon/Data/HistXTS.rda")


newz <- newz[index(newz) < "2018-10-25",]
n <- length(HistXTS)
Sentiment <- list()
for(i in 1:n){
  bnewz <- newz[newz[,2] == names(HistXTS)[i],]
  temphist <- na.locf(HistXTS[[i]])
  if(nrow(bnewz) > 0){
    a <- ATR(temphist[,2:4],5)
    TRtoATR <- a$tr/a$atr
    meanTrtAtr <- mean(TRtoATR, na.rm = T)
    StanDev <- sd(TRtoATR,na.rm = T)
    Limp <- meanTrtAtr + StanDev
    Limn <- meanTrtAtr - StanDev
    smV <- SMA(temphist[,5],5)
    meanV <- mean(smV, na.rm = T)
    StanDevV <- sd(smV,na.rm = T)
    LimpV <- meanV + StanDevV
    LimnV <- meanV - StanDevV
    sentimentATR <- vector()
    sentimentVol <- vector()
    m <- nrow(bnewz)
    for (j in 1:m) {
      tar <- index(bnewz)[j]
      tempAtr <- TRtoATR[index(TRtoATR) >= tar,]
      
      if(as.numeric(tempAtr[1,1]) > Limp){
        sentimentATR[j] <- 1
      }else if(as.numeric(tempAtr[1,1]) < Limn){
        sentimentATR[j] <- -1
      }else {
        sentimentATR[j] <- 0
      }
      tempsmV <- smV[index(smV) >= tar,]
      if(as.numeric(tempsmV[1,1]) > LimpV){
        sentimentVol[j] <- 1
      }else if(as.numeric(tempsmV[1,1]) < LimnV){
        sentimentVol[j] <- -1
      }else {
        sentimentVol[j] <- 0
      }
    }
    df <- data.frame(Story = bnewz$Story, Ticker = bnewz$TotalCleaned.ticker, sentimentVol = sentimentVol, sentimentATR = sentimentATR)
    bs <- xts(df,order.by = index(bnewz))
    Sentiment[[names(HistXTS)[i]]] <- bs
  }
   
}
save(Sentiment,file = "C:/Users/Pooya/Desktop/Core/Library/CityFalcon/Data/Sentiment.rda")
load("C:/Users/Pooya/Desktop/Core/Library/CityFalcon/Data/Sentiment.rda")









