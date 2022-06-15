survey = read.csv("lecture_09/fl_student_survey.csv")

miu = 28
# alt: miu > 28

n = nrow(survey[survey$gender == "m", ])
s = sd(survey[survey$gender == "m", ]$age)
p = nrow(survey[survey$gender == "m",]) / n
xbar = mean(survey[survey$gender == "m", ]$age)

t = (xbar - miu) / (s/ sqrt(n))

pval = 1-pt(t, n-1)
pval
