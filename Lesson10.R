#Lesson 10

#Maximum likelihood estimation

#https://en.wikipedia.org/wiki/Maximum_likelihood_estimation

#In statistics, maximum likelihood estimation (MLE) is a method of estimating the parameters of a probability distribution by maximizing a likelihood function, 
#so that under the assumed statistical model the observed data is most probable. The point in the parameter space that maximizes 
#the likelihood function is called the maximum likelihood estimate.

#ex.1
#We start with a simple example so that we can cross check the result.Suppose the observationsX1, X2, ..., Xn
#are fromN(mu, sigma^2) distribution (2 parameters:mu and sigma^2).

#The log likelihood function is
#??????(Xi?????)^2/2??^2???1/2 *log 2?????1/2 log??^2+ logdXi

#actually we do not have to keep the terms ???1/2 log 2?? and logdXi since they are constants.

#store the data in a vector $ function
xvec<-c(2,5,3,7,-3,-2,0)
fn=function(theta){
  sum(0.5*(xvec-theta[1])^2)/theta[2]+0.5*log(theta[2])
}
#where there are two parameters:theta[1]andtheta[2]. 
#They are components of a vector theta. Then we try to find the max 
#(actually the min ofnegative log lik)
 
nlm(fn, theta<-c(0,1),hessian=TRUE)  #minimization
#$minimum [1] 2.687429

#$estimate [1]  1.714285 79.428500

#$gradient [1]  2.745952e-08 -2.515975e-09

#$hessian
#[,1]          [,2]
#[1,]  8.812958e-02 -9.425592e-08
#[2,] -9.425592e-08  7.922174e-05

#$code [1] 1

#$iterations [1] 21

mean(xvec)
#[1] 1.714286 # this checks out with estimate[1]

sum( (xvec -mean(xvec))^2 )/7 
#[1] 11.34694 #this also checks out w/ estimate[2]

output1 <- nlm(fn, theta <- c(2,10), hessian=TRUE)
solve(output1$hessian) #to compute the inverse of hessian# which is the approx. var-cor matrix
#[,1]         [,2]
#[1,] 11.34693420 1.354702e-02
#[2,]  0.01354702 1.262283e+04

sqrt( diag(solve(output1$hessian)) )
#[1]   3.368521 112.351366

11.34694/7
#[1] 1.620991
sqrt(11.34694/7) #  [1] 1.273182    st. dev. of mean checks out

# or 
optim(theta<-c(0,1),fn, hessian=TRUE) # minimization, diff R function

#$par [1]  1.715858 79.459183

#$value [1] 2.687429

#$counts 
#function gradient 
#75       NA 

#$convergence [1] 0

#$message   NULL

#$hessian
#[,1]          [,2]
#[1,]  8.809554e-02 -1.743439e-06
#[2,] -1.743439e-06  7.913137e-05


#You may need to try several starting values (here we used c(0,1)) 
#for the theta ( i.e.theta[1]=0, theta[2]=1.)

# How about the covariance between  ??xandv? here it is approx. 0.0003028(very small). Theory say they are independent, so the true covariance shouldequal to 
#0.-1.743439e-06 => accept


#Example of inverting the (Wilks) likelihood ra-tio test to get confidence interval
xvec <- c(2,5,3,7,-3,-2,0)     # or some other numbers
fn <- function(theta) {sum ( 0.5*(xvec - theta[1])^2/theta[2] + 0.5* log(theta[2]) )}
# test hipotesis
#H0: sigma=1.5 vs H1: sigma!= 1.5
#assume mu=2, so the MLE of sigma is
mleSigma <- sqrt( sum( (xvec - 2)^2 ) /length(xvec))
mleSigma #[1] 3.380617
#Wilks statistics is
WilksStat <- 2*( fn(c(2,1.5^2)) - fn(c(2,mleSigma^2)))
WilksStat #[1] 17.17925 
# this is larger than 3.84 (=5% significance of a chi-square distri-bution),
#so we should reject the hypothesis of??= 1.5)
# After some  trials and error we find
2*( fn(c(2,2.1635^2)) - fn(c(2,mleSigma^2))) #[1] 3.842709
2*( fn(c(2,6.37^2)) - fn(c(2,mleSigma^2))  ) #[1] 3.841142

#So the 95% confidence interval for sigma is (approximately)[2.1635, 6.37].
#We also see that the 95% confidence Interval for  variance (sigma^2) is [2.16352,6.372] 
#sort of invariance property (for the confidence interval).
#We point out that the confidence interval from the Wald construction donot have invariance property.The Wald 
#95% confidence interval for sigma is (using formula we derivedin the midterm exam)
3.380617 - 1.96*3.380617/sqrt(2*length(xvec)) #[1] 1.609742
3.380617 + 1.96*3.380617/sqrt(2*length(xvec)) #[1] 5.151492
# [1.609742,5.151492]

# ANOVA
#Analysis of variance (ANOVA) is a collection of statistical models and their associated estimation procedures 
#(such as the "variation" among and between groups) used to analyze the differences among group means in a sample. 
#ANOVA was developed by statistician and evolutionary biologist Ronald Fisher.
#The ANOVA is based on the law of total variance, where the observed variance in a particular variable is partitioned into 
#components attributable to different sources of variation. In its simplest form, ANOVA provides a statistical test of whether
#two or more population means are equal, and therefore generalizes the t-test beyond two means. 

#https://www.guru99.com/r-anova-tutorial.html#1

#One-way ANOVA
#There are many situations where you need to compare the mean between multiple groups. 
#For instance, the marketing department wants to know if three teams have the same sales performance.

#Team: 3 level factor: A, B, and C
#Sale: A measure of performance
#The ANOVA test can tell if the three groups have similar performances. 

#Hypothesis in one-way ANOVA test:
  
#H0: The means between groups are identical
#H1: At least, the mean of one group is different

#The H0 hypothesis implies that there is not enough evidence to prove the mean of the group (factor) are different from another.
#This test is similar to the t-test, although ANOVA test is recommended in situation with more than 2 groups. 
#Except that, the t-test and ANOVA provide similar results. 


#Before you start to compute the ANOVA test, you need to prepare the data as follow:
#Step 1: Import the data
#Step 2: Remove unnecessary variable
#Step 3: Convert the variable poison as ordered level

#ex.2
#In class we handed out An Example of ANOVA. Below we redo the example using R.
#There are three groups with seven observations per group. We denote groupivalues by yi

y1 = c(18.2, 20.1, 17.6, 16.8, 18.8, 19.7, 19.1)
y2 = c(17.4, 18.7, 19.1, 16.4, 15.9, 18.4, 17.7)
y3 = c(15.2, 18.8, 17.7, 16.5, 15.9, 17.1, 16.7)
y=c(y1,y2,y3) # combine into 1 vector
n=rep(7,3) #econd vector,group, identifying groupmembership
n #[1] 7 7 7
group = rep(1:3, n)
group
#[1] 1 1 1 1 1 1 1 2 2 2 2 2 2 2 3 3 3 3 3 3 3
st
#Here are summaries by group and for the combined data.
#First we show stem-leaf dia-grams
tmp = tapply(y, group, stem)


#The decimal point is at the |
# 16 | 8
#17 | 6
#18 | 28
#19 | 17
#20 | 1
#The decimal point is at the |
#15 | 9
#16 | 4
#17 | 47
#18 | 47
#19 | 1
#The decimal point is at the |
#15 | 29
#16 | 57
#17 | 17
#18 | 8

stem(y)
#The decimal point is at the |
  
#15 | 299
#16 | 4578
#17 | 14677
#18 | 24788
#19 | 117
#20|1

#summary statistics by group and overall. 
#We locally define a temporaryfunction,tmpfn, to make this easier.

tmpfn = function(x) c(sum = sum(x), mean = mean(x), var = var(x),
                      n = length(x))
  tapply(y, group, tmpfn)
 
   tmpfn(y)
  #sum       mean        var          n 
  #371.800000  17.704762   1.798476  21.000000 
   
#ANOVA
data = data.frame(y = y, group = factor(group))
fit = lm(y ~ group, data)
anova(fit)   

#Analysis of Variance Table

#Response: y
#Df Sum Sq Mean Sq F value  Pr(>F)  
#group      2 11.007  5.5033  3.9683 0.03735 *
#Residuals 18 24.963  1.3868                  
---
#Signif. codes:  0 *** 0.001 ** 0.01 * 0.05 . 0.1   1
  
#Theanova(fit)object can be used for other computations on the handout and in class.
#For instance, the tabled F values can be found by the following. First we extract the treat-ment and error degrees of freedom.
#Then we useqtto get the tabled F values.
df = anova(fit)[, "Df"]
names(df) = c("trt", "err")
df
#trt err 
#2  18
alpha = c(0.05, 0.01)
qf(alpha, df["trt"], df["err"], lower.tail = FALSE)  #[1] 3.554557 6.012905

#A confidence interval on the pooled variance can be computed as well using theanova(fit)object.

anova(fit)["Residuals", "Sum Sq"] #[1] 24.96286
anova(fit)["Residuals", "Sum Sq"]/qchisq(c(0.025, 0.975), 18,  lower.tail = FALSE)
#[1] 0.7918086 3.0328790