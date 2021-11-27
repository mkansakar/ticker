library(quantmod)
tickers <- read.csv("500SANDP.csv", header=T,  sep = ',', stringsAsFactors = FALSE)

x <- list()
length(tickers)
start <- as.Date("2021-06-01")
end   <- Sys.Date()

tickers[tickers$Symbol=="MMM",2]

s= tickers[,1]


s <- unlist(s)

getSymbols(s[0], src="yahoo", from = start, to = end, auto.assign=FALSE, return.class="xts") 


for (i in s) 
{ 
  tryCatch({
    x[[i]] <- getSymbols(i, src="yahoo", from = start, to = end, auto.assign=FALSE, return.class="xts") 
    
  }, error=function(e){})
}

for (i in x)  
  { 
  n1 <- unlist(strsplit(colnames(i)[1], ".", fixed = TRUE))[1]  
  p1 <- tickers[tickers$Symbol==n1,2]
  s1 <- tickers[tickers$Symbol==n1,3]
  h1 <- tickers[tickers$Symbol==n1,4]
  m1 <- paste(p1, s1,  h1, sep = " -> ")
  jpeg(paste('.\\S_And_P\\500SP_', n1,".jpeg",sep=""), width = 1280, height = 800)
  chartSeries(x = i, name = m1, show.grid = TRUE, theme = chartTheme("white"), bar.type = "ohlc", up.col = "blue" , dn.col = "red") 
  addSMA(n = 3, on = 1, with.col = Cl, overlay = TRUE, col = "pink")
  addSMA(n = 8, on = 1, with.col = Cl, overlay = TRUE, col = "blue")

  dev.off()
}   

