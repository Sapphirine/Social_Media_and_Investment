library(XML)
library(RColorBrewer)
library(plyr)
library(quantmod)
library(httr)
#Reference http://blog.datapunks.com/2011/10/sp-500-components-heatmap-in-r/

tables <- GET("http://en.wikipedia.org/wiki/List_of_S%26P_500_companies")
tables <- readHTMLTable(rawToChar(tables$content))
t=as.data.frame(tables[1])
ticker=as.vector(t$NULL.Ticker.symbol)
play_ticker=ticker[1:200]
play_tickers=ticker[202:454]
play_ticker_3=ticker[456:491]
play_ticker_4=ticker[493:505]
ticker=ticker[c(-201,-455,-492)]

#Ten years data
x=getSymbols(ticker,from='2006-01-01',to='2016-01-01')

colname = c('O','H','L','C','V','AC')
dat = c()
for (i in 1:length(x)){
  yy= get(x[i])
  a=as.data.frame(yy)
  colnames(a) = colname
  dat = rbind(dat,a)
}

write.csv(dat,file='/Users/pingyuanwang/Desktop/dat.csv',sep=',',row.names=FALSE,col.names=FALSE)

dat$ZD=dat[,4]-dat[,1]
dat$Y=NA
for (i in 1:nrow(dat)){
  if (dat[i,7]<=0){
    dat[i,8]=0
  }else{
    dat[i,8]=1
  }
}
write.csv(dat,file='/Users/pingyuanwang/Desktop/dat.csv',sep=',',row.names=FALSE,col.names=FALSE)

b1=dat[4:nrow(dat),8]
c1=dat[3:(nrow(dat)-1),7]
d1=dat[2:(nrow(dat)-2),7]
e1=dat[1:(nrow(dat)-3),7]
f1=as.data.frame(cbind(b1,c1,d1,e1))
f1[,1]=as.factor(f1[,1])
f1[,2]=as.numeric(f1[,2])
f1[,3]=as.numeric(f1[,3])
f1[,4]=as.numeric(f1[,4])
write.csv(f1,file='/Users/pingyuanwang/Desktop/f1.csv')

