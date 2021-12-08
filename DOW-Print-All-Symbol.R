library(quantmod)

dfname     <- read.table(file='tickers2.csv', header = T, sep=',', quote = "")
tickers <- as.character(dfname$key)
#tickers = c("AAPL","IBM","CSCO","C","MGM","BA","BAC","WFC")
start <- as.Date("2015-01-01")
end   <- Sys.Date()

#getSymbols(tickers, src="yahoo") #), from = start, to = end)

length(tickers)

###   Print the length of each stock series.

head(tickers)
for (t in tickers) {
  a = get(noquote(t))[,1]
  print(c(t,length(a)))
} 

####### Convert closing adjusted prices of all stocks into individual data.frames.

df = list()
j = 0
for (t in tickers) {
  j = j + 1
  a = noquote(t)
  b = data.frame(get(a)[,6])
  b$dt = row.names(b)
  df[[j]] = b
}

###### Make a single data frame

stock_table = df[[1]]
for (j in 2:length(df)) {
  stock_table = merge(stock_table,df[[j]],by="dt")
}

###### Plot the stock series

PDFPath = paste("dow7_",Sys.Date(),".pdf",sep="")
pdf(file=PDFPath, width = 12)         

       for (j in 1:length(tickers)) {
         tryCatch({   
          chartSeries(tickers[i], type="line",subset='last 60 months', TA="addSMA(200,col='orange');addSMA(65,col='red')")
          #SID       <-  dfname$key[a] 
          #plot(as.Date(stock_table[,1]),stock_table[,j+1], type="l", ylab=tickers[j],xlab="date")
          #mtext(SID, adj = 1, side = 3, col = "gray50", cex.axis = 0.7) 
         }, error=function(e){})
      }
dev.off()
