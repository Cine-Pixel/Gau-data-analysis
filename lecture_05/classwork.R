sandooba = 95
# 1 - alpha = sandooba / 100
alpha = 1 - (sandooba / 100)
abs(qnorm(alpha/2))


# 5
x = c(84, 14,	31,	72,	26,	49,	252,	104,	31,	8,	3,	18,	72,	23,	55,	133, 16, 29, 225,	138, 85, 24,	391,	72,	158,	4340,	346,	19,	5,	846,	461,	254,	125,	61,	123,	60,	29,	10,	366,	47,	28,	254,	6,	77,	21,	97,	6,	17,	8,	82)
n = length(x)
sandooba = 90
alpha = 1 - (sandooba / 100)

mean(x) + c(-1, 1) * abs(qnorm(alpha/2)) * sd(x) / sqrt(n)


# 7
