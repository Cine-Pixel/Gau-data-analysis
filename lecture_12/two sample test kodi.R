survey = read.csv("datasets/fl_student_survey.csv",header=TRUE,sep=",",dec=".")
head(survey)

m = survey$age[survey$gender == 'm']
survey[survey$gender == 'm',]$age
f = survey$age[survey$gender == 'f']
zeda = ((sd(m)^2)/length(m)+(sd(f)^2)/length(f))^2
qveda = (((sd(m)^2)/length(m))^2)/(length(m)-1) + (((sd(f)^2)/length(f))^2)/(length(f)-1)

v = zeda/qveda
v
t = (mean(m)-mean(f))/sqrt((sd(m)^2)/length(m)+(sd(f)^2)/length(f))
2*pt(-abs(t),v)
t.test(m , f, var.equal = TRUE)


t.test(m , f, var.equal = TRUE,alt="greater")

#proporciebi
x1 = 49
x2 = 74
n1 = 5103
n2 = 2549
p1 = x1/n1
p2 = x2/n2
p = (x1+x2) / (n1+n2)
z = (p1-p2)/sqrt(p*(1-p)*((1/n1)+(1/n2)))
z
2*pnorm(-abs(z))
pnorm(z)
1-pnorm(z)

