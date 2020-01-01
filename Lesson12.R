#Lesson 12

#Let X and Y be the last two digits of the fac. number ???? XY.

#ex.1
#Check out the 'homedata' data from the 'UsingR' package. Take 50 consecutive observations starting at XY * 50.
#Build a 97% fair value for home price in 2000. and in 1970. We can assume that the cost of housing
#has not changed significantly?


#The data is normal if the p-value is above 0.05. !!!!!!!!!1

X=4
Y=1

library(UsingR)
data(homedata)

#export data

exportdata=41*50
data=homedata[exportdata:(exportdata+50),]

#check if the data are normal distributed

first=data$y1970
second=data$y2000
print(shapiro.test(fisrt))

#Shapiro-Wilk normality test
#data:  first
#W = 0.96624, p-value = 0.1538  # normal distributed

print(shapiro.test(second))
#Shapiro-Wilk normality test
#data:  second
#W = 0.94606, p-value = 0.02164   # not normal ....

#The data is normal if the p-value is above 0.05. 

# if the data are not normal distributed, we use  wilcox.test()
t=wilcox.test(second, conf.int=T,conf.level=0.97) # for 2000г.
secondInd=t$conf.int
secondInd
#[1] 206850 271300   # confidience interval
#attr(,"conf.level")
#[1] 0.97

t1=wilcox.test(first, conf.int=T,conf.level=0.97)  #  за 1970г.
fisrtint=t1$conf.int
fisrtint
#[1] 63500 72400  # confidience interval
#attr(,"conf.level")
#[1] 0.97

# Hypotheses:
# H0: prices are the same
# H1: The prices are different

wilcox.test(first,second,cond.level=0.97,alternative='two.sided')

#Wilcoxon rank sum test with continuity correction
#data:  first and second
#W = 42.5, p-value < 2.2e-16
#alternative hypothesis: true location shift is not equal to 0

# p-value is really small, so we accept  H1.


#ex.2
#Rainfall in cm/m2 in the summer, as well as cotton crops in kilograms per acre, were measured at twelve Texas farms.
#The results are in a table. What is the expected harvest for the farm where the rainfall was Y + 5cm/m2?

#rain: 8.9,6.4,12.8,9.6,13.8,16.1,12.4,12.8,11.8,16,7.4,12.5
#production: 306.2,272.5,385,392.5,400,531.2,415,407.5,373.7,493.7,325,400

#Can we say that an increase in rainfall of 1cm/m2 leads to a yield increase 
#of at least 24 kilograms per hectare?

rain=c(8.9,6.4,12.8,9.6,13.8,16.1,12.4,12.8,11.8,16,7.4,12.5)
production=c(306.2,272.5,385,392.5,400,531.2,415,407.5,373.7,493.7,325,400)

#постро€ваме линеен модел
l=lm(rain~production)
summary(l)

#Call:
#lm(formula = rain ~ production)
#Residuals:
 # Min      1Q  Median      3Q     Max 
#-2.1337 -0.7228  0.3690  0.6343  1.7700 

#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept) -3.773961   2.070112  -1.823   0.0983 .  
#production   0.039510   0.005203   7.594 1.85e-05 ***
  ---
# Signif. codes:  0 С***Т 0.001 С**Т 0.01 С*Т 0.05 С.Т 0.1 С Т 1

#Residual standard error: 1.244 on 10 degrees of freedom
#Multiple R-squared:  0.8522,	Adjusted R-squared:  0.8374 
#F-statistic: 57.67 on 1 and 10 DF,  p-value: 1.853e-05

plot(rain,production,
     main="Relationship between rain and production",
     xlab="rain",
     ylab="production")
plot(l)

#evaluate for 6 mm rain 

df=data.frame(rain=c(6))
df
print(predict(l,df))

# we check that if it increases by 1 cm the crop will grow by 24 kg

sm=summary(l)
coeff=sm$coefficients
t=(coeffs[2,1]-24)/coeffs[2,2]
res=pt(t,df=6,lower.tail=T) #The Student t Distribution
t
res

#ex.3
#The manufacturer of the X stoves claims that its products heat the room faster than those of its rival manufacturer Y.
#The time in seconds required to raise the temperature by h is measured.
#Data received
#for X: 39.50,61,67,40,40,54
#for Y: 60,53,42,41,40,54,63,69

data1=c(39,50,61,67,40,40,54)
data2=c(60,53,42,41,40,54,63,69)

# check if data is normalized
print(shapiro.test(data1))
#Shapiro-Wilk normality test
#data:  data1
#W = 0.8903, p-value = 0.2762  #normalized
print(shapiro.test(data2))
#Shapiro-Wilk normality test
#data:  data2
#W = 0.9149, p-value = 0.3899  #normalized

## p-value> 0.05 data is normalized => we have numbered data for both data.
# Hypotheses:
# H0: data1 <data2
# H1: data> = data2

# because the data is normalized, we use t.test ()

t.test(data1,data2,alternative="greater",paired=F,var.equal=F)
#Welch Two Sample t-test
#data:  data1 and data2
#t = -0.45541, df = 12.665, p-value = 0.6717
#alternative hypothesis: true difference in means is greater than 0
#95 percent confidence interval:
 # -12.76592       Inf
s#ample estimates:
  #mean of x mean of y 
#50.14286  52.75000 

#p-value=0.6717 is high, the first is assumed to heat up faster


#ex.4
#It turns out that out of 5,000 adults living in a city, 48 have developed lung cancer.
#A study is trying to determine whether the development of cancer is related to the inhalation of asbestos particles.
#It is known that 12 people out of 520 who worked in offices isolated with asbestos have lung cancer.
#Does the occurrence of lung cancer affect the asbestos content in the work environment?

peoplewithcancerazbest=12
peoplewithcancer=48
peoplewithcancernoazbest=peoplewithcancer-peoplewithcancerazbest
allpeople=5000
peopleazbest=520
peoplenoazbest=allpeople-peopleazbest
prop.test(c(peoplewithcancerazbest,peoplewithcancernoazbest),
          c(peopleazbest,peoplenoazbest),
          alternative="less")

#ex.5
#Generate vectors A and B with 100 exponentially distributed random numbers with an expectation of X + 1.
#Let C = A / (A + B). Count how many of the elements of C fall into the intervals, respectively
#[0,0.25], (0.25,0.5], (0.5,0.75], (0.75,1]. Make a histogram at these intervals.
#Check that the elements of C are evenly distributed in [0,1].

data = 4 + 1
# we generate the data
lamdba = 1 / data
A = rexp (100, lamdba)
B = rexp (100, lamdba)
C = A / (A + B)

# we count how many of them fall at appropriate intervals
cuted = cut (C, breaks = c (0,0.25,0.5,0.75,1))
table = table (cuted)
table

#cuted
# (0,0.25] (0.25,0.5] (0.5,0.75] (0.75,1]
# 25 27 21 27

#plot histogram
hist (C, breaks = c (0,0.25,0.5,0.75,1))

# Hypotheses
# H0: C ~ U [0,1]
# H1: C! ~ U (0.1)

# We test the hypotheses by chi square
# we use the data and the intervals at which the generated data falls
# because it compares with a multi-dimensional distribution, the expected probability
# will be 1/4 for each interval

final = chisq.test (table (cuted), p = tail (1 / 4.4))
final
# Chi-squared test for given probabilities
#data: table (cuted)
# X-squared = 0.96, df = 3, p-value = 0.8109

# p-value = 0.8109 large, that is, we accept H0.

