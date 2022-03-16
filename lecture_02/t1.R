x = c(1, 2, 9, 6, 1, 1, 13, 20, 4, 0, 1, 0, 1, 31, 9, 10, 2, 7, 3, 1)
x=sort(x)

intervals = 5

step = ceiling((min(x) + max(x))/intervals)
bins = seq(min(x), max(x), by=step)
bins = c(bins, bins[length(bins)]+step)

par(mfrow=c(2,2))

hist(x, breaks = bins, xaxt='n', lty="blank", main="Histogram")
axis(1, at = bins)

cut(x, breaks = bins, right=F)
tbl = transform(table(cut(x, breaks=bins, right=F)))
tbl$Percents = 100*tbl$Freq/sum(tbl$Freq)
tbl$CumulativePercents = cumsum(tbl$Percents)

mid_points = c()
for(i in (1:length(bins)-1)) {
  mid = (bins[i] + bins[i+1])/2
  mid_points = c(mid_points, mid)
}

tbl$mid = mid_points
tbl

hist(x, breaks = bins, xaxt='n', lty="blank", col="white", main="Frequency")
axis(1, at = bins)
lines(tbl$mid, tbl$Freq, lty=1)
bins
cp = c(0, tbl$CumulativePercents)
cp

plot(bins, cp, pch=1, xlab="Sample Size", ylab="Percent" , main = "Ogive", xaxt = "n")
axis(1, at=bins, labels = bins)
lines(bins,cp, col = "gray")
text(bins, cp-4, labels=paste(cp, "%", sep=""))
