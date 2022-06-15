game = function() {
  doors = sample(c(1, 0, 0))
  choice = sample(1:3, 1)
  
  if(doors[choice] == 1) outcome = "stay"
  else outcome = "change"
  
  return(outcome)
}

change = 0
stick = 0
n = 100000
for(i in 1:n) {
  if(game() == "change") change = change + 1
  else stick = stick + 1
}

change / n
stick / n


# 1   0   0
# 0   1   0
# 0   0   1

