survey = read.csv("lecture_07-quiz2/fl_student_survey.csv", header = T, sep=",", dec=".")
head(survey)

# 1
confidence = 95
alpha = 1 - confidence / 100
n = length(survey$life_after_death)
x = length(survey[survey$life_after_death == "y", ]$life_after_death)
p_hat = x / n
t_alpha_half = abs(qt(alpha/2, n-1))

l = t_alpha_half * sqrt((p_hat * (1 - p_hat)) / n)
l * 2
# 1) 0.25


# 2
confidence = 90
alpha = 1 - confidence / 100
n = length(survey$abortion_legalize)
x = length(survey[survey$abortion_legalize == "y", ]$abortion_legalize)
p_hat = x / n
t_alpha_half = abs(qt(alpha/2, n-1))

l = t_alpha_half * sqrt((p_hat * (1 - p_hat)) / n)
l * 2
# 3) 0.17


# 3
confidence = 90
alpha = 1 - confidence / 100
n = length(survey[survey$gender == "m",]$college_GPA)
s = sd(survey[survey$gender == "m",]$college_GPA)
X_ = mean(survey[survey$gender == "m",]$college_GPA)
t_alpha_half = abs(qt(alpha/2, n-1))
l = t_alpha_half * s / sqrt(n)
l
# 3) 0.09

# 4
miu = 5
n = nrow(survey)
x = mean(survey$TV)
s = sd(survey$TV)
t = (x - miu) / (s / sqrt(n))

1 - pt(t, df=n-1)
# t.test(survey$TV, mu=5, alternative = "greater")
# 3) 0.006


# 5
p = 0.4
n = nrow(survey)
x = nrow(survey[survey$life_after_death == "y",])
p_hat = x / n
z = (p_hat - p) / sqrt((p * (1 - p)) / n)
1 - pnorm(z)
# 2) 0.03


# 6
confidence = 95
alpha = 1 - confidence / 100
non_republican = survey[survey$political_affiliation != 'r',]
no_life_after_death = non_republican[non_republican$life_after_death != 'y',]
p_hat = nrow(no_life_after_death)/nrow(non_republican)
l = abs(qnorm(alpha/2)) * sqrt((p_hat*(1-p_hat)) / nrow(non_republican))
l*2


# 7
lower = 27.8
upper = 32.06
mean(survey$age) - 27.8
l = (upper - lower)/2
n = nrow(survey)
s = sd(survey$age)
t_alpha_half = l / (s / sqrt(n))
1 - (1 - pt(t_alpha_half, n))*2



# 8
x1 = mean(survey[survey$gender=='m',]$age)
s1 = sd(survey[survey$gender=='m',]$age)
n1 = length(survey[survey$gender=='m',]$age)

x2 = mean(survey[survey$gender=='f',]$age)
s2 = sd(survey[survey$gender=='f',]$age)
n2 = length(survey[survey$gender=='f',]$age)

t = (x1 - x2)/sqrt(s1^2 / n1 + s2^2 / n2)
v = ((s1^2/n1 + s2^2/n2)^2)/((s1^2/n1)^2/(n1-1) + (s2^2/n2)^2/(n2-1))

ifelse(-abs(t) < qt(0.05/2,v),'უარვყოფთ','უარყოფის საფუძველი არ გაგვაჩნია')
# 2) უარყოფის საფუძველი არ გაგვაჩნია

# 9
x1 = survey$high_sch_GPA[survey$gender == 'm']
n1 = length(survey$high_sch_GPA[survey$gender == 'm'])

x2 = survey$high_sch_GPA[survey$gender == 'f']
n2 = length(survey$high_sch_GPA[survey$gender == 'f'])

sp = ((n1-1)*(sd(x1)^2) + (n2-1)*(sd(x2)^2)) / ((n1-1) + (n2-1))
t = (mean(x1)-mean(x2)) / sqrt(sp*((1/n1)+(1/n2)))
pnorm(t)
# 1) 0.16


# 10
x1 = survey$abortion_legalize[survey$gender == 'm']=='y'
n1 = length(survey$abortion_legalize[survey$gender == 'm']=='y')

x2 = survey$abortion_legalize[survey$gender == 'f']=='y'
n2 = length(survey$abortion_legalize[survey$gender == 'f']=='y')

sp = ((n1-1)*(sd(x1)^2) + (n2-1)*(sd(x2)^2)) / ((n1-1) + (n2-1))
t = (mean(x1)-mean(x2)) / sqrt(sp*((1/n1)+(1/n2)))
pnorm(t)
# 1) 0.14
