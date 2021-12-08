library(dplyr)

df = read.csv(".\\data\\nepalstock.csv",header=T,sep=',', quote = "",stringsAsFactors = FALSE)
df[df==0] <- NA
df$date1<-as.Date(df$date1)
#str(dt1)
SID <- "NLIC"
dt1 <- df %>%
  filter(df$Name == SID) %>%	
  select(date1, Close, Amnt) %>%
  subset(date1 > "2015-01-01" & date1 < "2016-01-01") %>%
  na.omit() %>%
  arrange(date1)
#Standard deviation
sd(dt1$Close)
#Variance
var(dt1$Close)
#range -> min and max
range(dt1$Close)
#quantiles at 10%, 25%, 50%, 75%, 90%
quantile(dt1$Close,c(0.1, 0.25, 0.5, 0.75, 0.9))
#coffiecent of variance
sd(dt1$Close)/mean(dt1$Close)
#inter quantile range
IQR(dt1$Close)
#histogram
hist(dt1$Close, col="orange")
