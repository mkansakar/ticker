library(dplyr)
library(xts)
library(TTR)
dt         <- read.csv("dow.csv", header = T, sep=',',  quote = "", stringsAsFactors = FALSE)
#dt         <- read.csv("dow-1985-2020.csv", header = T, sep=',',  quote = "", stringsAsFactors = FALSE)
dt$date1   <- as.Date(dt$date1, format="%m/%d/%Y")
SID        <- "DJI"


#vol <- read.csv("vol-index.csv", header = T, sep = ",", quote = "", stringsAsFactors = F)
#vol$date1 <- as.Date(vol$date1, format = "%Y-%m-%d")

#barplot(tail(vol$Quantity,380), ylab = NULL)


rsi1    <- RSI(dt$Index, maType = "SMA")
macd1   <- MACD(dt$Index, nFast = 12, nSlow = 26, nSig = 9, maType = SMA, percent = T)
sma10   <- SMA(dt$Index, n = 10)
sma30   <- SMA(dt$Index, n = 30)
bband   <- BBands(dt$Index, sd = 2)

dt1    <- data.frame(dt, sma10, sma30, rsi1, macd1, bband)

#summary(dt1)

#dt1[dt1$date1 >= "1999-01-01" & dt1$date1 <= "2016-06-27",1]

d_cal        <- 100#max(dt$SN)
#d_cal <- dt1[dt1$date1 >= "1999-01-01" & dt1$date1 <= "2016-06-27",1]
alldt        <- tail(dt1, d_cal)
#alldt        <- list(d_cal)
alldt$id     <- seq.int(d_cal)

above1    <- alldt$macd > alldt$signal
crss.sgnl <- which(diff(above1) != 0)
above2    <- alldt$sma10 > alldt$sma30
crss.sma  <- which(diff(above2) != 0)

max_date    <- max(alldt$date1)
min_date    <- min(alldt$date1)
t_date      <- paste(min_date,"to",max_date, sep=" ")

max_date    <- max(alldt$date1)
min_date    <- min(alldt$date1)

p1          <- seq(min(alldt$id), max(alldt$id), 20)
d1          <- format(subset(alldt, id %in% p1)$date1, "%Y %b")
idx         <- round(seq(round(min(alldt$Index), 0), round(max(alldt$Index), 0), length.out = 10),0)

#plot three graphs stacked one top of another.
op<-par(no.readonly=TRUE)
par(mfrow = c(3, 1), mgp=c(2, .5, 0), las = 0)
nf <- layout(matrix(c(1,2,3),ncol=1), widths=c(7, 7, 7), heights=c(4,1,1.75), TRUE) 

#plot1
par(mar = c(0, 2.5, 4, 2.5))
min_lim     <- min(alldt$Index) - (min(alldt$Index)*5)/100 
max_lim     <- max(alldt$Index) + (max(alldt$Index)*5)/100
plot(alldt$id, alldt$Index, type = "l", lwd = 2.5, col = "red", main = "", tck = 0.01, cex.axis = 1.0, las = 1, yaxt = "n", xaxt="n", xlab="", ylab="", ylim = c(min_lim, max_lim))
lines(alldt$id, alldt$sma10, col='blue', lty = 1, lwd = 0.15)
lines(alldt$id, alldt$sma30, col='brown', lty = 1, lwd = 0.15)
abline(h = c(idx), lwd = 0.5, col = "gray50", lty = 3)
abline(v = c(p1), lwd = 0.5, col = "gray50", lty = 3)
#abline(v = c(crss.sgnl), lwd = 0.5, col = "red")
#abline(v = c(crss.sma), lwd = 1, col = "blue")
legend("topleft", legend = c("Closing", "SMA 10", "SMA 30"), lty = c(1, 1, 1), col = c("red", "blue", "brown"), bty = "n", lwd = c(2.5, 0.15, 0.15), cex = 1, x.intersp = 0.5, y.intersp = .25)
mtext("Moving Avg", side = 4, col = "gray50", line = .5, cex = 0.75)
mtext(SID, adj = 1, side = 3, col = "gray50", cex.axis=0.7)
mtext(t_date, adj = 0, side = 3, col = "gray50", cex.lab = 0.25)
axis(2, at = c(idx), tck=0.01, cex.axis = .85, las = 2, labels = c(idx))

#plot2
par(mar = c(0, 2.5, 0, 2.5))
rmax <- max(alldt$rsi1, na.rm = T) + (max(alldt$rsi1, na.rm = T)*5)/100
rmin <- min(alldt$rsi1, na.rm = T) - (max(alldt$rsi1, na.rm = T)*5)/100
plot(alldt$id, alldt$rsi, lwd = 2, col = "blue", type = "l", main = "", tck = 0.02, cex.axis = 1.0, las = 1, yaxt = "n" , xaxt="n", xlab="", ylab="", ylim = c(rmin, rmax))
abline(v = c(p1), lwd = 0.5, col = "gray50", lty = 3)
abline(h = c(30, 50, 70), b = 0.1, lwd = .25, col = "blue", lty = "dotted")
#abline(v = c(crss.sgnl), lwd = 0.5, col = "red")
#abline(v = c(crss.sma), lwd = 1, col = "blue")
axis(2, at = c(30, 50, 70), tck=0.01, cex.axis = .85, line = 0, yaxs="i", col = "blue", las = 2, labels = c(30, 50, 70))
mtext("RSI", side = 4, col = "gray50", line = .5, cex = 0.75)


#plot3
par(mar = c(4, 2.5, 0, 2.5))
ymax <- max(alldt$macd, na.rm=TRUE) + (max(alldt$macd, na.rm=TRUE)*5)/100
ymin <- min(alldt$macd, na.rm=TRUE) - (min(alldt$macd, na.rm=TRUE)*5)/100
plot(alldt$id, alldt$macd, lwd = 2, col = "red", type = "l", main = "", tck = 0.02, cex.axis = 1.0, las = 1, xaxt="n", xlab="", ylab="", ylim=c(ymin, ymax))
lines(alldt$id, alldt$signal, col='blue', type = "l", lwd = 0.45)
abline(v = c(p1), lwd = 0.5, col = "gray50", lty = 3)
#abline(v = c(crss.sgnl), lwd = 0.5, col = "red")
#abline(v = c(crss.sma), lwd = 1, col = "blue")
abline(h = c(0))
legend("topleft", legend = c("macd", "signal"), lty = c(1, 1), col = c("red", "blue"), bty = "n", lwd = c(2, 0.45), cex = 1, x.intersp = 0.5, y.intersp = .25)
mtext("MACD", side = 4, col = "gray50", line = .5, cex = 0.75)
axis(1, at = c(p1), tck=0.01, cex.axis = .85, line = 0, yaxs="i", col = "blue", las = 2, labels = c(d1))

