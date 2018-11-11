options(stringsAsFactors = F)

load("C:/Users/Pooya/Desktop/Core/Library/CityFalcon/Data/SP500.rda")
load("C:/Users/Pooya/Desktop/Core/Library/CityFalcon/Data/CRYPTLIST.rda")

library(jsonlite)

token <- "829a9487bcab27b449d44912f339c4a7852ed31093392505519b0dbebf59e81d"
urlHardcode <- "http://api.cityfalcon.com"
versionCF <- "v0.2"
ServiceCF <- "stories?"
Link <- paste(urlHardcode,versionCF,ServiceCF, sep = "/")
identifier_type <- "identifier_type=tickers"
identifiersraw <- "identifiers="
categories <- "categories=mp"
min_cityfalcon_score <- "min_cityfalcon_score=0"
order_by <- "order_by=top"
time_filter <- "time_filter=mth1"
languages <- "languages=en"
all_languages <- "all_languages=false"
limit <- "limit=500"
offsets <- "offset=0"
access_token <- "access_token="
access_token <- paste(access_token,token,sep = "")

NEWS <- list()
n <- nrow(SP500)
for(i in 1:n){
  identifiersName <- SP500[i,1]
  identifiers <- paste(identifiersraw,identifiersName,sep = "")
  SubLink <- paste(identifier_type,identifiers,categories,min_cityfalcon_score,order_by,time_filter,languages,all_languages,limit,offsets,access_token,sep = "&")
  FinalLink <- paste(Link,SubLink,sep = "")
  fres <- fromJSON(FinalLink)
  NEWS[[i]] <- fres
}
save(NEWS,file = "C:/Users/Pooya/Desktop/Core/Library/CityFalcon/Data/NEWS3.rda")

load("C:/Users/Pooya/Desktop/Core/Library/CityFalcon/Data/NEWS2.rda")

links <- vector()
descrits <- vector()
titles <- vector()
ticker <- vector()
publishDate <- vector()
k <- 1
for (i in 1:n) {
  t <- length(NEWS[[i]]$stories$title)
  if(t > 0) {
    for (j in 1:t) {
      links[k] <- NEWS[[i]]$stories$url[j]
      descrits[k] <- NEWS[[i]]$stories$description[j]
      titles[k] <- NEWS[[i]]$stories$title[j]
      publishDate[k] <- NEWS[[i]]$stories$publishTime[j]
      ticker[k] <- SP500[i,1]
      k <- k + 1
    } 
  }
}
Cleaned <- data.frame(ticker = ticker, titles = titles,links = links, descritions = descrits, publishDate = publishDate)
save(Cleaned,file = "C:/Users/Pooya/Desktop/Core/Library/CityFalcon/Data/Cleaned3.rda")

load("C:/Users/Pooya/Desktop/Core/Library/CityFalcon/Data/Cleaned.rda")
load("C:/Users/Pooya/Desktop/Core/Library/CityFalcon/Data/CORPUS.rda")
library(RSelenium)
library(xml2)
library(rvest)
rd <- rsDriver(browser = "chrome")
rem_dr <- rd[["client"]]
n <- nrow(Cleaned)
i <- length(CORPUS)
l <- i + 1
for (i in l:n) {
  url <- Cleaned$links[i]
  rem_dr$navigate(url)
  page <- read_html(rem_dr$getPageSource()[[1]])
  CORPUS[[i]] <- page %>% html_nodes("p") %>% html_text()
}
