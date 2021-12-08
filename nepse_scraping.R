library(XML)
library(dplyr)
dfname <- read.table(file='.//data//test1.txt',header=T,sep=',', quote = "")
dfname %>% mutate_if(is.factor, as.character) -> dfname
sdate <- as.Date("2021-04-02")
edate <- as.Date("2021-12-07")

fl <- paste("nepalstock_",Sys.Date(),".csv",sep="")
n<-nrow(dfname)
for (a in 1:n) { 
  tryCatch({
    dfname$value[a]
    #print(dfname$value[a])
    print(dfname$key[a])
    #url <- paste('http://nepalstock.com/main/stockwiseprices/index/2/Date/Desc/YTo0OntzOjk6InN0YXJ0RGF0ZSI7czoxMDoiMjAxMi0wMS0wMSI7czo3OiJlbmREYXRlIjtzOjEwOiIyMDE3LTAyLTIzIjtzOjEyOiJzdG9jay1zeW1ib2wiO3M6MzoiMzg1IjtzOjY6Il9saW1pdCI7czozOiI1MDAiO30?startDate=',sdate,'&endDate=',edate,'&stock-symbol=',dfname$value[a],'&_limit=30',sep="")
    url <- paste('http://nepalstock.com.np/main/stockwiseprices/index/1/Date/asc/YTo0OntzOjk6InN0YXJ0RGF0ZSI7czoxMDoiMjAxOC0wNC0wMiI7czo3OiJlbmREYXRlIjtzOjEwOiIyMDE4LTA0LTE1IjtzOjEyOiJzdG9jay1zeW1ib2wiO3M6MzoiMzk3IjtzOjY6Il9saW1pdCI7czoyOiI1MCI7fQ?startDate=',sdate,'&endDate=',edate,'&stock-symbol=',dfname$value[a],'&_limit=200',sep="")
    print(url)
    doc = htmlParse(url)
    tableNodes = getNodeSet(doc, "//table")
    tb = readHTMLTable(tableNodes[[1]])
    
    tb<-(tb[-c(1,2),])
    tb<-(tb[-nrow(tb),])
    
    tb$ID<-dfname$key[a]
    write.table(tb, fl ,col.names = F,append = T, row.names=FALSE, na="NA",quote = F, sep = ",")  
    
    Sys.sleep(1)
  }, error=function(e){})
}

