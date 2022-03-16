set.seed(50000)
asaki = 22 + rexp(50000, 1)
asaki = round(asaki) #damrgvaleba

# set.seed(50000)
# sqesi=sample(c(0,1),50000,replace=T,prob=c(0.7,0.3))



sim = function(size) {
  all_means = c()
  loops = 100
  for(i in 1:loops) {
    sp = sample(asaki, size, replace = F)
    all_means = c(all_means, mean(sp))
  }
  
  print(mean(all_means))
  print(mean(asaki))
  
  print(sd(asaki)/sqrt(size))
  print(sd(all_means))
  
  return(all_means)
}

n = c(100, 500, 1000, 2000)
colors = rainbow(length(n), alpha = .7)
plot(density(sim(n[1])), col = colors[1], xlab = "", ylab = "", ylim=c(0, 30), probability=T)
for(current_n in 2:length(n)) {
  print("-----------------------------")
  # sims(current_n)
  lines(density(sim(n[current_n])), col = colors[current_n])
}

abline(v = 22.96577, lwd=1)
legend('topright', col = colors, lty = 1, legend = n)