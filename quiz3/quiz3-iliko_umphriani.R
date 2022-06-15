survey = read.csv("datasets/fl_student_survey.csv",header=TRUE,sep=",",dec=".")


# task 1
x1 = survey$sports[survey$gender == 'm']
x2 = survey$sports[survey$gender == 'f']
n1 = length(survey$sports[survey$gender == 'm'])
n2 = length(survey$sports[survey$gender == 'f'])


v = (((sd(x1)^2)/n1)+((sd(x2)^2)/n2))/(((((sd(x1)^2)/n1)^2)/(n1-1)) + ((((sd(x2)^2)/n2)^2)/(n2-1)))

sp = ((n1-1)*(sd(x1)^2) + (n2-1)*(sd(x2)^2)) / ((n1-1) + (n2-1))
t = (mean(x1)-mean(x2)) / sqrt(sp*((1/n1)+(1/n2)))
1-pnorm(t)



# task 2
x1 = survey$sports[survey$gender == 'm']
x2 = survey$sports[survey$gender == 'f']
n1 = length(survey$sports[survey$gender == 'm'])
n2 = length(survey$sports[survey$gender == 'f'])


v = (((sd(x1)^2)/n1)+((sd(x2)^2)/n2))/(((((sd(x1)^2)/n1)^2)/(n1-1)) + ((((sd(x2)^2)/n2)^2)/(n2-1)))

sp = ((n1-1)*(sd(x1)^2) + (n2-1)*(sd(x2)^2)) / ((n1-1) + (n2-1))
t = (mean(x1)-mean(x2)) / sqrt(sp*((1/n1)+(1/n2)))
(1-pnorm(t))/2



# task 3
x1 = survey$distance_home[survey$gender == 'm']
x2 = survey$distance_home[survey$gender == 'f']
n1 = length(survey$distance_home[survey$gender == 'm'])
n2 = length(survey$distance_home[survey$gender == 'f'])


v = (((sd(x1)^2)/n1)+((sd(x2)^2)/n2))/(((((sd(x1)^2)/n1)^2)/(n1-1)) + ((((sd(x2)^2)/n2)^2)/(n2-1)))

sp = ((n1-1)*(sd(x1)^2) + (n2-1)*(sd(x2)^2)) / ((n1-1) + (n2-1))
t = (mean(x1)-mean(x2)) / sqrt(sp*((1/n1)+(1/n2)))
(1-pnorm(t))/2


# task 4
x1 = survey$life_after_death[survey$gender == 'm']=='y'
x2 = survey$life_after_death[survey$gender == 'f']=='y'
n1 = length(survey$life_after_death[survey$gender == 'm']=='y')
n2 = length(survey$life_after_death[survey$gender == 'f']=='y')

sp = ((n1-1)*(sd(x1)^2) + (n2-1)*(sd(x2)^2)) / ((n1-1) + (n2-1))
t3 = (mean(x1)-mean(x2)) / sqrt(sp*((1/n1)+(1/n2)))
pnorm(t)


# task 5
x1 = survey$political_affiliation[survey$gender == 'm']=='r'
x2 = survey$political_affiliation[survey$gender == 'f']=='r'
n1 = length(survey$political_affiliation[survey$gender == 'm']=='r')
n2 = length(survey$political_affiliation[survey$gender == 'f']=='r')

sp = ((n1-1)*(sd(x1)^2) + (n2-1)*(sd(x2)^2)) / ((n1-1) + (n2-1))
t3 = (mean(x1)-mean(x2)) / sqrt(sp*((1/n1)+(1/n2)))
pnorm(t)