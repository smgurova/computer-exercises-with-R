#Introduction to Hypothesis Testing in R 

#Hypothesis testing ultimately uses a p-value to weigh the strength of the evidence or in other words what the data are about the population. The p-value ranges between 0 and 1. It can be interpreted in the following way:
  
#1) A small p-value (typically  p<= 0.05) indicates strong evidence against
#the null hypothesis, so you reject it.
#2) A large p-value (p> 0.05) indicates weak evidence against
#the null hypothesis, so you fail to reject it.
#A p-value very close to the cutoff (p~0.05) is considered to be 
#marginal and could go either way.

#Type I Error – Type I error occurs when the researcher 
#rejects a null hypothesis (H0) when it is true. The term significance level is used to express 
#the probability of Type I error while testing the hypothesis. The significance level is represented by the symbol alpha.
#Type II Error – Accepting a false null hypothesis H0 is referred to as the Type II error.
#The term power of the test is used to express the probability of Type II error while testing hypothesis. 
#The power of the test is represented by the symbol beta.

#1. Student's T-test in R  --- t.test()

#t.test(data.1, data.2)- t-test is to compare two vectors of numeric data.

#prop.test()- can be used for testing the null that the proportions (probabilities of success) in 
#several groups are the same, or that they equal certain given values.


#prop.test(x, n, p = NULL,
#alternative = c("two.sided", "less", "greater"),
#conf.level = 0.95, correct = TRUE)


#x -a vector of counts of success, a one-dimensional table with 2 entries,
#ot 2-dimentional table..
#n - a vector of counts, trials.
#Ignored if x is a matrix or a table.
#p - probability.
#alternattive
#conf. level
#correct =  TRUE ot FALSE

#ex.1
# We have a coin that we throw 100 times.
# We want to see if a head will fall on the first roll?

#Answer: 
# We use binomial distribution, bc we have n=100 trials, p=1/2, k=1(successful trials).


#  {
heads <- rbinom(1, size = 100, prob = .5) 
prop.test(heads, 100)          # continuity correction TRUE by default

#1-sample proportions test with continuity correction
#data:  heads out of 100, null probability 0.5
#X-squared = 0.01, df = 1, p-value = 0.9203
#alternative hypothesis: true p is not equal to 0.5
#95 percent confidence interval:
 # 0.3894281 0.5913488
#sample estimates:
 # p 
#0.49    ==> reject H0.


#ex.2
#We have two data for smokers - 83,90,129, 70 and patienst - 86,93,136,82. 
#We want to see if the four population from which the patients were drawn 
#have the same true proportion of smokers.

## 
## H0: The null hypothesis is that the four populations from which
##     the patients were drawn have the same true proportion of smokers.
## H1=A:  The alternative is that this proportion is different in at
##     least one of the populations.

smokers  <- c( 83, 90, 129, 70 )
patients <- c( 86, 93, 136, 82 )
prop.test(smokers, patients)


#4-sample test for equality of proportions without continuity correction

#data:  smokers out of patients
#X-squared = 12.6, df = 3, p-value = 0.005585
#alternative hypothesis: two.sided
#sample estimates:
 # prop 1    prop 2    prop 3    prop 4 
#0.9651163 0.9677419 0.9485294 0.8536585 

#p-value =0.0055 <0.05 ==> H0 is rejected.


#2. One-Sample t-test 

#t.test(x,mu=0)
# x - the name of our variable
#mu - mu, which is described by the null
#hypothesis is set equal to the mean.

#ex.3
#If we wanted to test whether the volume of a shipment of lumber was less than usual 
#(??0=37000 cubic feet), we would run:

set.seed(0) 
#Set the seed of R‘s random number generator, which is
#useful for creating simulations or random objects that can be reproduced.
#seed – A number.
ship_vol <- c(rnorm(75, mean = 37000, sd = 2500))
t.test(ship_vol, mu = 39000)

#One Sample t-test
#data:  ship_vol
#t = -7.9555, df = 74, p-value = 1.572e-11
#alternative hypothesis: true mean is not equal to 39000
#95 percent confidence interval:
 # 36417.00 37451.72
#sample estimates:
 # mean of x 
#36934.36  ===> H0 is rejected p-value<0.05

#ex. 4
#We want to determine if 10 randomly chosen normally distributed quantities
#have a mathematical expectation of 5.

x=rnorm(10)
t.test(x,mu=5)
#data:  x
#t = -18.052, df = 9, p-value = 2.237e-08
#alternative hypothesis: true mean is not equal to 5
#95 percent confidence interval:
 # -0.3615729  0.8325190
#sample estimates:
 # mean of x 
#0.235473 ==> p-value <0.05 reject H0.


#ex. 5
set.seed(100)
x <- rnorm(50, mean = 10, sd = 0.5) 
t.test(x, mu=10)
#tetsing if mean of x is 10

#One Sample t-test
#data:  x
#t = 0.70372, df = 49, p-value = 0.4849
#alternative hypothesis: true mean is not equal to 10
#95 percent confidence interval:
 # 9.924374 10.157135
#sample estimates:
 # mean of x 
#10.04075 ==> p-value>0.05 => H0 cannot be rejected 

#3.Paired Sample

#We need either of the two vectors of data, y1 and y2, to conduct a paired-samples test. 
#Then, we will run this code using this using syntax t.test(y1, y2, paired=TRUE).

#ex.6
#We work at a large health clinic and we are testing a new drug, Procardia, whose work 
#is to reduce hypertension. We find 2000 individuals with high systolic blood pressure
#(x??=150 mmHg, SD=10 mmHg) and we provide them Procardia for a month, and 
#then measure their blood pressure again. We find that the mean systolic blood pressure 
#has decreased to 144 mmHg with a standard deviation of 9 mmHg.

set.seed(2820)
pre_Treatment <- c(rnorm(2000, mean = 150, sd = 10))
post_Treatment <- c(rnorm(2000, mean = 144, sd = 9))
t.test(pre_Treatment, post_Treatment, paired = TRUE)

#Paired t-test
#data:  pre_Treatment and post_Treatment
#t = 20.789, df = 1999, p-value < 2.2e-16
#alternative hypothesis: true difference in means is not equal to 0
#95 percent confidence interval:
 # 5.593910 6.759259
#sample estimates:
 # mean of the differences 
#6.176585  ==> p-value<0.05 => H0 is rejected


#3. Independent Samples

#The independent-samples test can take one of three forms, depending on the structure of
#your data and the equality of their variances. The general form of the test is
#t.test(y1, y2, paired=FALSE). 
#By default, R assumes that the variances of y1 and y2 are unequal, thus
#defaulting to Welch’s test. To toggle this, we use the flag, var.equal=TRUE.

#ex.7
#We’ll test the hypothesis in which Clevelanders and New Yorkers spend different amounts for eating outside
#on a monthly basis.

#A) Independent-samples T-test where y1 and y2 are numeric:
set.seed(0)
Spenders_Cleve <- rnorm(60, mean = 350, sd = 77)
Spenders_NY <- rnorm(60, mean = 400, sd = 80)
t.test(Spenders_Cleve, Spenders_NY, var.equal = TRUE)


#Two Sample t-test
#data:  Spenders_Cleve and Spenders_NY
#t = -3.994, df = 118, p-value = 0.0001134
#alternative hypothesis: true difference in means is not equal to 0
#95 percent confidence interval:
 # -76.35948 -25.73807
#sample estimates:
 # mean of x mean of y 
#349.8529  400.9017  p-value<0.05 ==> H0 rejected

#B) Where y1 is numeric and y2 is binary:
Amount_Spent <- c(Spenders_Cleve, Spenders_NY)
city_name <- c(rep("Cleveland", 60), rep("New York", 60))
t.test(Amount_Spent ~ city_name, var.equal = TRUE)

#data:  Amount_Spent by city_name
#t = -3.994, df = 118, p-value = 0.0001134
#alternative hypothesis: true difference in means is not equal to 0
#95 percent confidence interval:
 # -76.35948 -25.73807
#sample estimates:
 # mean in group Cleveland  mean in group New York 
#349.8529                400.9017
#p-vaue<0.05 H0 is rejected

#C) want to test for equality of variances in your data prior to running an independent-samples T-test

var.test(Spenders_Cleve, Spenders_NY)

#F test to compare two variances

#data:  Spenders_Cleve and Spenders_NY
#F = 1.2926, num df = 59, denom df = 59, p-value = 0.327
#alternative hypothesis: true ratio of variances is not equal to 1
#95 percent confidence interval:
 # 0.7721056 2.1639941
#sample estimates:
 # ratio of variances 
#1.292607  ==> p-value>0.05 H0 cant be rejected


#4. Wilcoxon Signed Rank Test

#To test the mean of a sample when normal distribution is not assumed. 
#Wilcoxon signed rank test can be an alternative to t-Test, especially when the 
#data sample is not assumed to follow a normal distribution. 
#It is a non-parametric method used to test if an estimate is different from 
#its true value.

#ex.8

numeric_vector <- c(20, 29, 24, 19, 20, 22, 28, 23, 19, 19)
wilcox.test(numeric_vector, mu=20, conf.int = TRUE)

#Wilcoxon signed rank test with continuity correction
#data:  numeric_vector
#V = 30, p-value = 0.1056
#alternative hypothesis: true location is not equal to 20
#90 percent confidence interval:
 # 19.00006 25.99999
#sample estimates:
# (pseudo)median 
#23.00002  p-Value < 0.05, reject the null hypothesis and accept the alternative hypothesis

#5. Two Sample t-Test and Wilcoxon Rank Sum Test

#Both t.Test and Wilcoxon rank test can be used to compare the mean of 2 samples.
#The difference is t-Test assumes the samples being tests is drawn from a normal distribution, 
#while, Wilcoxon’s rank sum test does not.

#ex.9
x <- c(0.80, 0.83, 1.89, 1.04, 1.45, 1.38, 1.91, 1.64, 0.73, 1.46)
y <- c(1.15, 0.88, 0.90, 0.74, 1.21)
wilcox.test(x, y, alternative = "g")  # g for greater

#Wilcoxon rank sum test
#data:  x and y
#W = 35, p-value = 0.1272
#alternative hypothesis: true location shift is greater than 0
#p-Value of 0.1272, we cannot reject the null hypothesis that both x and y have same means.

#  6. Shapiro Test

#To test if a sample follows a normal distribution.

#ex.10
#Lets see how to do the test on a sample from a normal distribution.

# Example: Test a normal distribution
set.seed(100)
normaly_disb <- rnorm(100, mean=5, sd=1) # generate a normal distribution
shapiro.test(normaly_disb)

#Shapiro-Wilk normality test
#data:  normaly_disb
#W = 0.98836, p-value = 0.535
#p-value >0.05 => H0 is not rejected. The tested sample is confirmed to follow a normal distribution 

#ex.11
# Example: Test a uniform distribution
set.seed(100)
not_normaly_disb <- runif(100)  # uniform distribution.
shapiro.test(not_normaly_disb)

#Shapiro-Wilk normality test
#data:  not_normaly_disb
#W = 0.96509, p-value = 0.009436  
# the null-hypothesis that it is normally distributed can be rejected

#7. Kolmogorov And Smirnov Test
#Kolmogorov-Smirnov test is used to check whether 2 samples follow the same distribution.

#ks.test(x, y) # x and y are two numeric vector

#ex.12
# From different distributions
x <- rnorm(50)
y <- runif(50)
ks.test(x, y)  # perform ks test

#Two-sample Kolmogorov-Smirnov test
#data:  x and y
#D = 0.58, p-value = 4.048e-08  ==> p<0.05 H0 is rejected.Implies x and y from different distributions
#alternative hypothesis: two-sided

#ex. 13
x <- rnorm(50)
y <- rnorm(50)
ks.test(x, y) 

#Two-sample Kolmogorov-Smirnov test
#data:  x and y
#D = 0.18, p-value = 0.3959 => p>0.05 => H0 is not rejected. x and y are from one and the same distributions.
#alternative hypothesis: two-sided

#8. Fisher’s F-Test
#Fisher’s F test can be used to check if two samples have same variance.
#Alternatively fligner.test() and bartlett.test() can be used for the same purpose.

#ex. 14
x<-rnorm(100)
y<-rbinom(100)
var.test(x, y)

#F test to compare two variances
#data:  x and y
#F = 0.93027, num df = 99, denom df = 49, p-value = 0.7485 ==>  Ho cant be rejected
#alternative hypothesis: true ratio of variances is not equal to 1
#95 percent confidence interval:
 # 0.5591201 1.4859940
#sample estimates:
 # ratio of variances 
#0.9302664

#9.  Chi Squared Test
#Chi-squared test in R can be used to test if two categorical variables are 
#dependent, by means of a contingency table.

#ex.15
#In the built-in data set survey, the Smoke column records the students smoking habit, while
#the Exer column records their exercise level. The allowed values in Smoke are 
#"Heavy", "Regul" (regularly), "Occas" (occasionally) and "Never". 
#As for Exer, they are "Freq" (frequently), "Some" and "None".

#Problem
#Test the hypothesis whether the students smoking habit 
#is independent of their exercise level at .05 significance level.

library(MASS)       # load the MASS package 
tbl = table(survey$Smoke, survey$Exer) 
tbl                 # the contingency table 

#Freq None Some
#Heavy    7    1    3
#Never   87   18   84
#Occas   12    3    4
#Regul    9    1    7

chisq.test(tbl) 

#Pearson's Chi-squared test
#data:  tbl
#X-squared = 5.4885, df = 6, p-value = 0.4828  =>
#p>0.05 H0 cant be rejected
#that the smoking habit is independent of the exercise level of the students.