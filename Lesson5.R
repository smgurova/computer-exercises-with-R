#Lesson 5

#ex.1
#Generate observations over a normally distributed random variable X. Build a hitogram.
#Add empirical and theoretical density. Observe how empirical density changes
#as the number of observations increases.

exp.razpredelenie=function()
{
  x=rnorm(1000)
 # hist(x,probability=T)
  #curve(dnorm(x),add=T)
  #lines(density(y), col('red'))
  #curve(dnorm(x),add=T)
  
}
mf=function(n=100)
{
  y=rnorm(100) 
  hist(y,Probability=T)
  for(i in 1:100){
    y=c(y,rnorm(10*i))
    hist(y,probability = T,
         breaks=c(seq(-5,5,0.25)),
         ylim=c(0.0,0.6))  #c(0.0,0.2,0.4,0.6)
  #empirical
    lines(density(y),col='red')
    #theoretical
    curve(dnorm(x),add=T)
    text(-4,0.6,i)
    Sys.sleep(0.1)
    }
}
exp.razpredelenie()

#ex.2
#Let the random variable X be standard normally distributed, i.e. X in N (0,1).
#Generate 100 observations.What part of them are in the intervals A = (- 1,1),
#B = (- 2,2), C = (- 3,3). Calculate the theoretical probabilities of X to belong 
#to these intervals. Summarize an arbitrary normal distributed random variable.

fun=function()
{
  y=rnorm(100)
  inA=sum(y>-1 & y<1)
  print(inA)
  #theretical probability to be in the interval
  print(pnorm(1)-pnorm(-1)) # end - begin
  inB=sum(y>-2 & y<2)
  print(inB)
  print(pnorm(2)-pnorm(-2)) # end - begin
  inC=sum(y>-3 & y<3)
  print(inC)
  print(pnorm(3)-pnorm(-3))
}
fun()
#[1] 72
#[1] 0.6826895
#[1] 97
#[1] 0.9544997
#[1] 100
#[1] 0.9973002

#ex.3
#Normally distributed random variable X in N (25,36). The melons are divided 
#into 3 groups smaller than 20 cm are considered 3rd quality and the rest are split into two equal parts. 
#What part are third quality? How big must a melon be in order to be first quality?
  
  pipesh = function ()
  {
    #quantile - where to the left, mathematical expectation, standard deviation
    p = pnorm (20,25,6)
    # probability to quantil we are looking for, math expectation, standard deviation
    qnorm ((1-p) / 2 + p, 25.6)
  }
  pipesh() #[1] 25.85636
  
#ex. 4
#Write a function that by the given parameter n. Generate 100 observations for each of
#random variables X1, X2, ..., Xn and returns a vector Y = X1 + X2 + ... + Xn. 
#In the case where:
#a) Xi are evenly distributed [1,5];
#b) Xi are exponentially distributed with parameter lambda = 4;
#c) Xi are binomially distributed Bi (3,0.2);
#what can you say about the distribution of Y. Consider the random n = 1,2,10,100,1000.
#Which n distribution starts to look like normal?
  
  zad4a=function(n)
  {
    i=0
    Y=runif(100,1,5)
    while(i<n-1)
    {Y=Y+runif(100,1,5)
    i=i+1}
    return(Y)}

zad4b=function(n)
{
  i=0
  Y=rexp(100,5)
  while(i<n-1)
  {
    Y=Y+rexp(100,4)
    i=i+1}
  return(Y)
}
zad4c=function(n,second=3)
{
  i=0
  Y=rbinom(100,second,0.2)
  while(i<n-1)
  {
    Y=Y+rbinom(100,second,0.2)
    i=i+1}
  return(Y)
}
#ex.5
#Look at the 'anscombe' data. For each pair of variables Xi, Yi define:
#mathematical expectations, variance, correlation coefficient. Present the data graphically.

# point estimates
# EX -mean (x)
# DX - var (x)
# Standard deviation - (sqrt Dx) - sd (x)
# x - distribution
# cov (X, Y) - covariance
# cor (X, Y) - corelation

# dataframe: anscombe $ x1; anscome [, x1]; attach (anscome)
#lm (formula = y ~ x) returns coefficients (linear regression)
# abline (lm (formula = y ~ x)) draws
#plot (x, y) shows data scattered, graphically displayed

zad5EX=function(x,y)
{
  plot(mean(x),mean(y))
  #abline(lm(y ~ x))
}
zad5DX=function(x,y)
{
  plot(var(x),var(y))
  #abline(lm(y ~ x))
}
zad5Cov=function(x,y)
{
  plot(cov(x,y))
  #abline(lm(y ~ x))
}
zad5Cor=function(x,y)
{
  plot(cor(x,y))
  #abline(lm(y ~ x))
}

