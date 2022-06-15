survey = read.csv("lecture_07-quiz2/fl_student_survey.csv", header = T, sep=",", dec=".")

# 1
confidence = 95
alpha = 1 - confidence / 100
non_republican = survey[survey$political_affiliation != 'r',]
no_life_after_death = non_republican[non_republican$life_after_death != 'y',]
p_hat = nrow(no_life_after_death)/nrow(non_republican)
l = abs(qnorm(alpha/2)) * sqrt((p_hat*(1-p_hat)) / nrow(non_republican))
l*2
    

# 2
lower = 27.8
upper = 32.06
mean(survey$age) - 27.8
l = (upper - lower)/2
n = nrow(survey)
s = sd(survey$age)
t_alpha_half = l / (s / sqrt(n))
1 - (1 - pt(t_alpha_half, n))*2


# 3
confidence = 95
alpha = 1 - confidence / 100
n = nrow(survey[survey$religiosity == 1,])
m = mean(survey[survey$religiosity == 1,]$political_ideology)
std = sd(survey[survey$religiosity == 1,]$political_ideology)
m - (abs(qnorm(alpha/2)) * std) / sqrt(n)


# 4
data = c(51, 54, 47, 53, 59, 46, 50, 50, 56, 46, 48, 50, 45, 49, 52, 55, 42, 57, 45, 51,
         53, 55, 51, 47, 53, 53, 49, 51, 43, 48, 44, 48, 54, 46, 49, 51, 52, 50, 55, 51,
         50, 53, 45, 49, 57, 54, 53, 49, 46, 48, 52, 48, 50, 52, 47, 50, 44, 46, 47, 49,
         49, 51, 57, 49, 51, 42, 49, 53, 44, 52, 53, 55, 48, 52, 44, 46, 54, 54, 57, 55,
         48, 50, 50, 55, 52, 48, 47, 52, 55, 50, 59, 52, 47, 46, 56, 54, 51, 56, 54, 55)
  
data = sort(data)

intervals = 7

step = ceiling((min(data) + max(data))/intervals)
bins = seq(min(data), max(data), by=step)
bins = c(bins, bins[length(bins)]+step)
bins

par(mfrow=c(2,2))

hist(data, breaks = bins, xaxt='n', lty="blank", main="Histogram", col=3)
axis(1, at = bins)

tbl = transform(table(cut(data, breaks=bins, right=F)))
tbl$Percents = round(100*tbl$Freq/sum(tbl$Freq), 1)
tbl$CumulativePercents = cumsum(tbl$Percents)
tbl$mid = (bins[-length(bins)] + bins[-1])/2
tbl

hist(data, breaks = bins, xaxt='n', lty="blank", col="white", main="Frequency")
axis(1, at = bins)
lines(tbl$mid, tbl$Freq, lty=1, col = 12)
cp = c(0, tbl$CumulativePercents)

plot(bins, cp, pch=16, xlab="Sample Size", ylab="Percent" , main = "Ogive", xaxt = "n", col = 3)
axis(1, at=bins, labels = bins)
lines(bins,cp, col = 12)
text(bins-1, cp+3, labels=paste(cp, "%", sep=""))