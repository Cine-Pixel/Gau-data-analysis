x = c(2, 6, 14, 1, 10, 3, 0, 26, 37, 47, 4, 0, 6, 21, 3, 6, 
      6, 51, 8, 11, 0, 0, 4, 2, 5, 9, 4, 6, 24, 4, 1, 18, 63, 
      1, 5, 6, 4, 7, 12, 5, 3, 4, 3, 15, 46, 0, 5, 0, 26, 5, 17, 1, 17, 1, 58, 9)
x=sort(x)

intervals = 13

step = ceiling((min(x) + max(x))/intervals)
bins = seq(min(x), max(x), by=step)
bins = c(bins, bins[length(bins)]+step)

par(mfrow=c(2,2))

hist(x, breaks = bins, xaxt='n', lty="blank", main="Histogram", col=3)
axis(1, at = bins)

tbl = transform(table(cut(x, breaks=bins, right=F)))
tbl$Percents = round(100*tbl$Freq/sum(tbl$Freq), 1)
tbl$CumulativePercents = cumsum(tbl$Percents)
tbl$mid = (bins[-length(bins)] + bins[-1])/2
tbl

hist(x, breaks = bins, xaxt='n', lty="blank", col="white", main="Frequency")
axis(1, at = bins)
lines(tbl$mid, tbl$Freq, lty=1, col = 12)
cp = c(0, tbl$CumulativePercents)

plot(bins, cp, pch=16, xlab="Sample Size", ylab="Percent" , main = "Ogive", xaxt = "n", col = 3)
axis(1, at=bins, labels = bins)
lines(bins,cp, col = 12)
text(bins-1, cp+3, labels=paste(cp, "%", sep=""))
