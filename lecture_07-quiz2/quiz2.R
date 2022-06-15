survey = read.csv("lecture_07-quiz2/fl_student_survey.csv", header = T, sep=",", dec=".")


# 1
confidence = 99
alpha = 1 - confidence / 100
n = length(survey$religiosity)
x = length(survey[survey$religiosity == 3, ]$religiosity)
p_hat = x / n
t_alpha_half = abs(qt(alpha/2, n-1))

l = t_alpha_half * sqrt((p_hat * (1 - p_hat)) / n)
l * 2


# 2
confidence = 90
alpha = 1 - confidence / 100
n = length(survey$abortion_legalize)
x = length(survey[survey$abortion_legalize == 'y',]$abortion_legalize)
p_hat = x / n
t_alpha_half = abs(qt(alpha/2, n-1))

l = t_alpha_half * sqrt((p_hat * (1 - p_hat)) / n)
l * 2


# 3
confidence = 90
alpha = 1 - confidence / 100
n = length(survey$high_sch_GPA)
s = sd(survey$high_sch_GPA)
X_ = mean(survey$high_sch_GPA)
t_alpha_half = abs(qt(alpha/2, n-1))
l = t_alpha_half * s / sqrt(n)
l


# 4
confidence = 70
alpha = 1 - confidence / 100
n = length(survey$age)
s = sd(survey$age)
X_ = mean(survey$age)
t_alpha_half = abs(qt(alpha/2, n))
l = t_alpha_half * s / sqrt(n)
X_ - c(1, -1) * l


# 5
lower = 0.2949774
upper = 0.4093272
l = (upper - lower)/2
n = length(survey$political_affiliation)
x = length(survey[survey$political_affiliation == 'd', ]$political_affiliation)
p_hat = x / n
t_alpha_half = l / sqrt(p_hat * (1-p_hat) / n)
1 - (1 - pt(t_alpha_half, n))*2
