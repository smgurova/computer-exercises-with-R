#Lesson 6

# Tests, confidence intervals

#ex. 1
#When checking the survey, 87 out of 150 respondents said they had used a product.
#Build a 95% confidence interval for the number of people using the product.

# Xi in Bi(1,p)
# p~ = xn_=sum xi/n
#t=(xn_ -p)/(sqrt(p~(1-p~))/n)
#q1<t<q2 // q1+t+q2 =1
#p(xn_ - q*sqrt(xn_(1-xn_)/n))<p<xn_+q*sqrt(xn_(1-xn_)/n)

#xn=87/150
#t=0.95


#prop.test(x, n, p = NULL,
#alternative = c("two.sided", "less", "greater"),
#conf.level = 0.95, correct = TRUE)

#prop.test can be used for testing the null that the proportions (probabilities of success) in several 
#groups are the same, or that they equal certain given values.
#Arguments

#x - a vector of counts of successes, a one-dimensional table with two entries, or a two-dimensional table (or matrix) with 2 columns, giving the counts of successes and failures, respectively.
#n - a vector of counts of trials; ignored if x is a matrix or a table.
#p - a vector of probabilities of success. The length of p must be the same as the number of groups specified by x, and its elements must be greater than 0 and less than 1.
#alternative - a character string specifying the alternative hypothesis, must be one of "two.sided" (default), "greater" or "less". You can specify just the initial letter. Only used for testing the null that a single proportion equals a given value, or that two proportions are equal; ignored otherwise.
#conf.level - confidence level of the returned confidence interval. Must be a single number between 0 and 1. Only used when testing the null that a single proportion equals a given value, or that two proportions are equal; ignored otherwise.
#correct - a logical indicating whether Yates' continuity correction should be applied where possible.


#prop. test(87,150,conf.level=0.95)
#t=prop. test(87,150,conf.level=0.95) # in t there are variables


#conf.int(level = 0.95, size = 50, cl = c("red", "gray"), ...)
#Arguments:

#level - the confidence level (1?????), e.g. 0.95
#size -the sample size for drawing samples from N(0, 1)
#cl -two different colors to annotate whether the confidence intervals cover the true mean (cl[1]: no; cl[2]: yes)

#t$conf.int[1]/[2] borders 

conf.intervals=function()
{
  xn=87/150
  q1=qnorm(0.025)
  q2=qnorm(0.975)
  c=sqrt(xn*(1-xn)/150)
  d1=xn+q1*c
  d2=xn+q2*c
  return(c(d1,d2))
}
conf.intervals() # [1] 0.5010156 0.6589844
zad1.prop=function()
{
  t=prop.test(87,150,conf.level = 0.95)
  return(c(t$conf.int[1],t$conf.int[2]))
}
zad1.prop() #[1] 0.4966533 0.6591851

#ex.2
#Acute leukemia is one of the most lethal forms of cancer. Previous studies have shown that the time of
#experience after the initial detection of leukemia is a normally distributed random variable with mathematical
#expectation 13 months and standard deviation 3 months. new treatment is being introduced and is expected to prolong the average
#lifespan without affecting dispersion. 16 patients were observed:
#10, 13.6, 13.2, 11.6, 12.5, 14.2, 14.9, 14.5, 13.4, 8.6, 11.5, 16, 14.2, 16.8, 17.9, 17
#Build a 95% confidence interval for patients' average life expectancy. Construct the same interval,
#in case of unknown dispersion.


#conf. invervals when we know the variance
#X in N(13,3^3) # EX=13, VarX =3^2 , stand.def=3
#t=(Xn_-EX)/sqrt((VarX)^2/4)
#p(Xn_ - q*sqrt((VArX)^2/n))< EX< Xn_ + q*sqrt((Varx)^2/n)
#x=scan() introduction

zad2.dopINt=function()
{
  #n=16
  arr=c(10, 13.6, 13.2, 11.6, 12.5, 14.2, 14.9, 14.5, 13.4, 8.6, 11.5, 16, 14.2, 16.8, 17.9, 17)
  xn=sum(arr)/length(arr)
  q1=qnorm(0.02)
  q2=qnorm(0.98)
  c=3/4
  d1=xn+q1*c
  d2=xn+q2*c
  return(c(d1,d2))
}
zad2.dopINt()  #[1] 12.20344 15.28406

#when we don't know the EX & Var X
#S^2=sum(xi-x_n)^2/n-1
#S^2=vor(xi)
#t=(xn_ -EX)/sqrt(S^2/4)
#t.test() # we use it when have a normal destribution
#wilcox.test()   if it's not a normal destribution

zad2.novar=function()
{
  arr=c(10, 13.6, 13.2, 11.6, 12.5, 14.2, 14.9, 14.5, 13.4, 8.6, 11.5, 16, 14.2, 16.8, 17.9, 17)
  res=t.test(arr,conf.level=0.95)
  return(c(res$conf.int[1],res$conf.int[2]))
}
zad2.novar() #[1] 12.39066 15.09684


#ex.3
#Check out the 'malpract' package data for the magnitude of 'UsingR' procurement abuses.
#Find a 95% confidence interval for the average abuse rate.

#conf.int=T  we told it to evaluate the interval
library(UsingR)
data(malpract)
malpract
#[1]  760  380  125  250 2800  450  100  150 2000  180  650  275  850 1700 1500 3000  390


zad3.confInt=function(x)
{
  res=wilcox.test(x,conf.int=T,conf.level=0.95)
  return(c(res$conf.int[1],res$conf.int[2]))
}
zad3.confInt(malpract) #[1]  320 1500

#ex.4
#Check out the 'rat' data from the 'UsingR' package. Find a 90% confidence interval for the average time
#of rats experience.

library(UsingR)
data (rat)
rat #[1] 152 152 115 109 137  88  94  77 160 165 125  40 128 123 136 101  62 153  83  69
matr=matrix(rat, nrow=5,ncol-4) #turns vector into matrix
zad.4.dovInt=function(x)
{
  res=t.test(x,conf.level=0.95)
  return(c(res$conf.int[1],res$conf.int[2]))
}
zad.4.dovInt(rat) #[1]  96.69997 130.20003

#ex.5
#Generate 30 observations over a random variable normally distributed with expectation 5 and variance1 4.
#Construct a 92% confidence interval for mathematical expectation. Repeat the experiment 100 times. Check how many cases
#mathematical expectation belongs to the confidence interval.

#matplot(x, y, type = "p", lty = 1:5, lwd = 1, lend = par("lend"),pch = NULL,
#col = 1:6, cex = NULL, bg = NA, xlab = NULL, ylab = NULL, xlim = NULL, ylim = NULL,
#log = "", …, add = FALSE, verbose = getOption("verbose"))
#Plot the columns of one matrix against the columns of another.
# x,y-  vectors or matrices of data for plotting. The number of rows should match. If one of them are missing, the other is taken as y and an x vector of 1:n is used. Missing values (NAs) are allowed.
#type -character string (length 1 vector) or vector of 1-character strings indicating the type of plot for each column of y, see plot for all possible types. The first character of type defines the first plot, the second character the second, etc. Characters in type are cycled through; e.g., "pl" alternately plots points and lines.
#lty,lwd,lend -vector of line types, widths, and end styles. The first element is for the first column, the second element for the second column, etc., even if lines are not plotted for all columns. Line types will be used cyclically until all plots are drawn.
#pch -character string or vector of 1-characters or integers for plotting characters, see points. The first character is the plotting-character for the first plot, the second for the second, etc. The default is the digits (1 through 9, 0) then the lowercase and uppercase letters. 
#col- vector of colors. Colors are used cyclically.

zad5.dovInt=function()
{
  matrix=matrix(c(0,0),2,1);
  cnt=0;
  for(i in 1:100)
  {x=rnorm(30,mean=5,sd=4)
  t=t.test(x,conf.level=92/100)
  matrix=cbind(matrix,c(t$conf.int[1],t$conf.int[2]))
               if(5>t$conf.int[1] &&5<t$conf.int[2])
               {cnt=cnt+1
               }
  }
matrix=matrix[,2:ncol(matrix)]
y=rbind(c(1:100),c(1:100))
matplot(matrix,y,type='l')
print(cnt)
return(matrix)
}