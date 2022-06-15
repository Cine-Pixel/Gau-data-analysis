survey = read.csv("datasets/fl_student_survey.csv",header=TRUE,sep=",",dec=".")
head(survey)


#sandooba 
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

#111111 uaryopa 


x1 <- mean(survey[survey$gender=='m',]$age)
x2 <- mean(survey[survey$gender=='f',]$age)

s1 <- sd(survey[survey$gender=='m',]$age)
s2 <- sd(survey[survey$gender=='f',]$age)

n1 <- length(survey[survey$gender=='m',]$age)
n2 <- length(survey[survey$gender=='f',]$age)

t1 <- (x1 - x2)/sqrt(s1^2 / n1 + s2^2 / n2)

v1 = ((s1^2/n1 + s2^2/n2)^2)/((s1^2/n1)^2/(n1-1) + (s2^2/n2)^2/(n2-1))

ifelse(-abs(t1) < qt(0.05/2,v1),'reject','dont reject')


#22222222222

#meore axali formulit

x1 = survey$high_sch_GPA[survey$gender == 'm']
x2 = survey$high_sch_GPA[survey$gender == 'f']
n1 = length(survey$high_sch_GPA[survey$gender == 'm'])
n2 = length(survey$high_sch_GPA[survey$gender == 'f'])


#ucnobia da mivichnevt aratolad

v = (((sd(x1)^2)/n1)+((sd(x2)^2)/n2))/(((((sd(x1)^2)/n1)^2)/(n1-1)) + ((((sd(x2)^2)/n2)^2)/(n2-1)))

#ucnobia da mivichnevt tolad (swori pasuxiii)

sp = ((n1-1)*(sd(x1)^2) + (n2-1)*(sd(x2)^2)) / ((n1-1) + (n2-1))
t = (mean(x1)-mean(x2)) / sqrt(sp*((1/n1)+(1/n2)))
pnorm(t)


#3333333333

x13 = survey$abortion_legalize[survey$gender == 'm']=='y'
x23 = survey$abortion_legalize[survey$gender == 'f']=='y'
n13 = length(survey$abortion_legalize[survey$gender == 'm']=='y')
n23 = length(survey$abortion_legalize[survey$gender == 'f']=='y')

#ucnobia da mivichnevt tolad (swori pasuxiii)

sp3 = ((n13-1)*(sd(x13)^2) + (n23-1)*(sd(x23)^2)) / ((n13-1) + (n23-1))
t3 = (mean(x13)-mean(x23)) / sqrt(sp3*((1/n13)+(1/n23)))
pnorm(t3)

prop.test(x13,x23,alternative = "less")