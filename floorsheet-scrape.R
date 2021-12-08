library(XML)
library(dplyr)
library(lubridate)

dfname <- read.table(file='nepse.csv',header=T,sep=',', quote = "")
dfname %>% mutate_if(is.factor, as.character) -> dfname
sdate <- as.Date("2017-03-01")
edate <- as.Date("2017-03-31")


fl <- paste("2017-Mar-floorsheet.csv",sep="")
n<-nrow(dfname)
for (a in 1:n) { 
  tryCatch({
    nlim <- sample(2500:3000, 1)
    #stime <- sample(1:3,1)
    
    print(dfname$key[a]) 
    url <- paste('http://nepalstock.com.np/company/transactions/',dfname$value[a],'/2/seller/asc/YTozOntzOjk6InN0YXJ0RGF0ZSI7czoxMDoiMjAxNC0wMS0wMSI7czo3OiJlbmREYXRlIjtzOjEwOiIyMDE0LTAyLTAxIjtzOjY6Il9saW1pdCI7czozOiI1MDAiO30?startDate=',sdate,'&endDate=',edate,'&_limit=',nlim,sep="")
    print(url)
    doc = htmlParse(url)
    tableNodes = getNodeSet(doc, "//table")
    tb = readHTMLTable(tableNodes[[1]])
    tb<-(tb[-1,])
    tb<-(tb[-nrow(tb),])
    write.table(tb, fl ,col.names = F,append = T, row.names=FALSE, na="NA",quote = F, sep = ",")  
    Sys.sleep(2)
  }, error=function(e){})
}