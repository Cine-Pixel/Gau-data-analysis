survey = read.csv("lecture_07-quiz2/fl_student_survey.csv", header = T, sep=",", dec=".")

sandooba = 90
alpha = sandooba/100
t_alpha_half = abs(pt(alpha/2, length(survey$age)-1))

mean(survey$age) - c(1, -1) * t_alpha_half * sd(survey$age) / sqrt(length(survey))
min(survey$age)
max(survey$age)

t.test(survey$age, conf.level = 0.9)



head(survey)
p_hat = length(survey[survey$political_affiliation == 'r', ]$political_affiliation) / length(survey$political_affiliation)
n = length(survey$political_affiliation)
sandooba = 95
alpha = 1 - sandooba/100
t_alpha_half = abs(pt(alpha/2, n))

p_hat - c(1, -1) * t_alpha_half * sqrt((p_hat*(1-p_hat)) / n)


table(survey$political_affiliation)
prop.test(15, 60, conf.level = 0.95, correct = T)



# 4.68 - 5.68
head(survey)
mean(survey$sports)

t_alpha_half = (5.68 - 4.68)/2
s = sd(survey$sports)
n = length(survey$sports)
l = t_alpha_half * sqrt(n-1) / s
probability = pt(l, n-1) 
probability
(1 - probability)