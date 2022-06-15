# 10.18
x = c(10.1, 9.3, 9.2, 10.2, 9.3, 9.6, 9.4, 8.8)
n = length(x)
xbar = mean(x)
s = sd(x)
alpha = 0.01
critical_value = abs(qt(alpha, n-1))
miu = 9
t = (xbar - miu) / (s / sqrt(n))

ifelse(t > critical_value, "reject", "accept")

pt(t, n-1)

t.test(x, mu=9, alternative = "less")
