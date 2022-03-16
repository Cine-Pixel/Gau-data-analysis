points = c(1, 11,2, 14, 15, 17, 23, 24, 40)
freq = c(1, 2, 1, 10, 10, 9, 9, 8, 2)

data = sort(rep(points, freq))

p = c(25, 50, 75)
n = length(data)
r = (p/100)* (n+1)

Q = (r - floor(r)) * (data[r + 1] - data[r]) + data[r]

IQR = Q[2] - Q[1]
IQR

lower = Q[1] - 1.5 * IQR
upper = Q[2] + 1.5 * IQR

lower_whisker = min(data[data >= lower])
upper_whisker = max(data[data <= upper])
