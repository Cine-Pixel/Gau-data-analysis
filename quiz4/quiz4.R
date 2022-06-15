subject = c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L")
before = c(200, 174, 198, 170, 179, 182, 193, 209, 185, 155, 169, 210)
after = c(191, 170, 177, 167, 159, 151, 176, 183, 159, 145, 146, 177)
quarter = c(rep(1, 3), rep(2, 3), rep(3, 3), rep(4, 3))

data = data.frame(subject, before, after, quarter)


colors = numeric(4)
colors[quarter == "1"] = "red"
colors[quarter == "2"] = "blue"
colors[quarter == "3"] = "green"
colors[quarter == "4"] = "orange"

dotchart(data$after, labels = data$subject, pch = 21, bg = "green",
         xlim = range(data$before, data$after) + c(-2, 2),
         pt.cex = 1.5)
points(data$before, 1:nrow(data), col = "red", pch = 19, cex = 1.5)

invisible(sapply(1:nrow(data), function(i) {
  segments(min(data$after[i], data$before[i]), i,
           max(data$after[i], data$before[i]), i, lwd = 2)
  text(min(data$after[i], data$before[i]) - 1.5, i,
       labels = min(data$after[i], data$before[i]))
  text(max(data$after[i], data$before[i]) + 1.5, i,
       labels = max(data$after[i], data$before[i]))
}))

points(data$before, 1:nrow(data), col = "red", pch = 19, cex = 1.5)
points(data$after, 1:nrow(data), col = "red", pch = 21, bg = "green", cex = 1.5)



t.test(before, after, paired = T, alternative ="two.sided")


data$di = data$before-data$after
data$di_squared = data$di*data$di

D_hat = mean(data$di)
S_d = sqrt((sum(data$di_squared) - (sum(data$di^2)/nrow(data))) / nrow(data)-1)

l = S_d / sqrt(nrow(data))
CI = D_hat + abs(l) * c(-1, 1)
CI

t = D_hat / (S_d/sqrt(nrow(data)))
alpha = 0.05
t_alpha = qt(alpha/2, nrow(data)-1)

FTR_interval = abs(t_alpha) * c(-1, 1)
if(t < FTR_interval[1] | t > FTR_interval[2]){
  print("Rejected")
} else {
  print("Failed to Reject")
}
