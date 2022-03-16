p = 0.05
population_size = 10000
x = sample(c(rep(1, population_size*p), rep(0, population_size - population_size*p)))

sim = function(n) {
  total_props = c()
  for(i in 1:1000) {
    sp = sample(x, size=n, replace=F)
    total_props = c(total_props, sum(sp)/n) 
  }
  return(total_props)
}


sample_sizes = c(20, 40, 100, 200)
for(s in sample_sizes) {
  sim(s) 
}

hist(sim(200), breaks = 5)
