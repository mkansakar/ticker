library(quantmod)
library(zoo)
library("dplyr")
library("plyr")
library("readr")

s <- read.csv("CommonStocks.csv", header=F)
s <- unlist(s)
x <- list()
length(s)

start <- as.Date("2020-08-01")
end   <- Sys.Date()

for (i in s) 
{ 
  tryCatch({
    x[[i]] <- getSymbols(i, src="yahoo", from = start, to = end, auto.assign=FALSE, return.class="xts")
    
  }, error=function(e){})
}

for (i in x)  
{
  n1 <- unlist(strsplit(colnames(i)[1], ".", fixed = TRUE))[1]
  fl1 <- paste(".\\dow\\", n1,".csv",sep="")
  write.zoo(i,file=fl1,sep=",",header = FALSE)
}

#file_names <- dir("~/dow") 
#df <- do.call(rbind, lapply(file_names, function(x) cbind(read.csv(x), name=strsplit(x,'\\.')[[1]][1])))

#data_all <- list.files(path = "~/rstudio/dow", pattern = "*.csv", full.names = TRUE) %>% lapply(read_csv) %>%  bind_rows                                                       
#data_all  
