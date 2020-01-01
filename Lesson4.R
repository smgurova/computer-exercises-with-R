# Lesson 4
#Introduction in  distributions

#In R we have several basic functions that define the basic distributions (discrete & continuous):
  
#Distribution Function (arguments)
 
#beta - beta (shape1, shape2, ncp)
#binomial - binom (size, prob)
#chi-squared - chisq (df, ncp)
#exponential - exp (rate)
#gamma - gamma (shape, scale)
#logistic - logis (location, scale)
#normal - norm (mean, sd)
#Poisson - pois (lambda)
#Student's t - t (df, ncp)
#uniform - unif (min, max)

#Distributions are:
# discrete
#continious

#Discrete distributions:
#
#Bernoulli

#We can draw from a Bernoulli using sample(), runif() or rbinom() with size = 1.

 n <- 1000
 x <- sample(c(0,1), n, replace=T)
 x <- sample(c(0,1), n, replace=T, prob=c(0.3,0.7))
 x <- runif(n) > 0.3
 x <- rbinom(n, size=1, prob=0.2)
 
 #Binomial
 
#We can sample from a binomial distribution using the rbinom() function with arguments n for number of samples to take, size defining the number of trials and prob defining the probability of success in each trial.
 
 x <- rbinom(n=100,size=10,prob=0.5)
 x
 
 #Hypergeometric distribution
 
#We can sample n times from a hypergeometric distribution using the rhyper() function.
 
 #dhyper(x, m, n, k, log = FALSE)
 #phyper(q, m, n, k, lower.tail = TRUE, log.p = FALSE)
 #qhyper(p, m, n, k, lower.tail = TRUE, log.p = FALSE)
 #rhyper(nn, m, n, k)
 #x, q -	vector of quantiles representing the number of white balls drawn without replacement from an urn which contains both black and white balls.
 #m 	- the number of white balls in the urn.
 #n 	- the number of black balls in the urn.
 #k 	- the number of balls drawn from the urn.
 #p 	- probability, it must be between 0 and 1.
 #nn 	-number of observations. If length(nn) > 1, the length is taken to be the number required.
 
 
 x <- rhyper(n=100, 15, 5, 5) 
 x
 
 #Geometric distribution
 #The geometric distribution.
 
  N <- 10000 # trails
  x <- rgeom(N, .5) # probability 0.5
  x <- rgeom(N, .01) #probability 0.01
 x
 
 #Multinomial
 #The multinomial distribution.
 
 sample(1:6, 100, replace=T, prob= rep(1/6,6))
 
 
 #Negative binomial distribution
 #The negative binomial distribution is the distribution of the number of failures before k successes in a series of Bernoulli events.
 
N <- 100000
x <- rnbinom(N, 10, .25)
 x
 
#Poisson distribution
#We can draw n values from a Poisson distribution with a mean set by the argument lambda.
 
x <- rpois(n=100, lambda=3)
x 

#Continuous distributions

#Chi Square distribution
#Quantile of the Chi square distribution:
qchisq(.95,1) #[1] 3.841459
 qchisq(.95,10) #[1] 18.30704
 qchisq(.95,100) #[1] 124.3421

# Exponential
#We can sample n values from a exponential distribution with a given rate (default is 1) using the rexp() function

x <- rexp(n=100, rate=1)
 x

 #Fisher-Snedecor
# We can draw the density of a Fisher distribution (F-distribution) :
   
 par(mar=c(3,3,1,1))
 x <- seq(0,5,len=1000)
  plot(range(x),c(0,2),type="n")
  grid()
  lines(x,df(x,df1=1,df2=1),col="black",lwd=3)
  lines(x,df(x,df1=2,df2=1),col="blue",lwd=3)
  lines(x,df(x,df1=5,df2=2),col="green",lwd=3)
  lines(x,df(x,df1=100,df2=1),col="red",lwd=3)
  lines(x,df(x,df1=100,df2=100),col="grey",lwd=3)
  legend(2,1.5,legend=c("n1=1, n2=1","n1=2, n2=1","n1=5, n2=2","n1=100, n2=1","n1=100, n2=100"),col=c("black","blue","green","red","grey"),lwd=3,bty="n")
 
  #Gamma
#We can sample n values from a gamma distribution with a given shape parameter and scale parameter ?? {\displaystyle \theta } \theta using the rgamma() function. Alternatively a shape parameter and rate parameter ?? = 1 / ?? {\displaystyle \beta =1/\theta } {\displaystyle \beta =1/\theta } can be given.
  
 x <- rgamma(n=10, scale=1, shape=0.4)
 x <- rgamma(n=100, scale=1, rate=0.8)
x

#Normal and related distributions

#We can sample n values from a normal or gaussian Distribution with a given mean (default is 0) and sd (default is 1) using the rnorm() function

x <- rnorm(n=100, mean=0, sd=1)
x

#Quantile of the normal distribution

qnorm(.95) #[1] 1.644854
qnorm(.975) #[1] 1.959964
qnorm(.99) #[1] 2.326348

#Uniform distribution

#We can sample n values from a uniform distribution (also known as a rectangular distribution] between two values (defaults are 0 and 1) using the runif() function

runif(n=100, min=0, max=1)


#Table 1:  Common Probability      Distribution Functions in R

#Name 	  Probability Density 	Cumulative Distribution 	Quantile
#Normal 	   dnorm(Z,mean,sd) 	  pnorm(Z,mean,sd) 	       qnorm(Q,mean,sd)
#Poisson     dnorm(N,lambda) 	   pnorm(N,lambda) 	        qnorm(Q,lambda)
#Binomial    dbinom(N,size,prob)  pbinom(N,size,prob) 	  qbinom(Q,size,prob)
#Exponential  dexp(N,rate) 	      pexp(N,rate) 	            qexp(Q,rate)
#??2           dchisq(X,df) 	      pchisq(X.df) 	             qchisq(X,df)
 
# Normal distribution 
z<-seq(-3.5,3.5,0.1)  # 71 points from -3.5 to 3.5 in 0.1 steps
q<-seq(0.001,0.999,0.001)  # 1999 points from 0.1% to 99.9% on 0.1% steps
dStandardNormal <- data.frame(Z=z, 
                              Density=dnorm(z, mean=0, sd=1),
                              Distribution=pnorm(z, mean=0, sd=1))  
qStandardNormal <- data.frame(Q=q, 
                              Quantile=qnorm(q, mean=0, sd=1))  
head(dStandardNormal)

head(qStandardNormal)

plot(qStandardNormal)
plot(dStandardNormal)



#ex. 1

#Assume a random variable Z is distributed according to the normal distribution with mean 6 and standard deviation 4. 
#What is the probability that Z takes on a value between -1 and 3 ?
# A: subtract the c.d. at -1 from the c.d. at 3
pnorm(3, 6, 4) - pnorm(-1, 6, 4) #[1] 0.1865682

#ex.2
# Assume a random variable Z is distributed according to the normal distribution with mean 20 and standard deviation 10. 
#What is the 90% confidence interval around the mean for the expected value of Z?
 #  Use the quantile function
 
 upper <- qnorm(0.95, 20, 10)
lower <- qnorm(0.05, 20, 10)
c(lower, upper)   #[1]  3.551464 36.448536


#Poisson Distribution
lower<-qpois(0.001, lambda=2.5)
upper<-qpois(0.999, lambda=2,5)
n<-seq(lower,upper,1)
q<-seq(0.001,0.999,0.001)
dPoisson25 <- data.frame(N=n, 
                         Density=dpois(n, lambda=2.5),
                         Distribution=ppois(n, lambda=2.5))  
qPoisson25 <- data.frame(Q=q, Quantile=qpois(q, lambda=2.5))  
head(dPoisson25)
head(qPoisson25)
plot(dPoisson25)
plot(qPoisson25)

#ex. 3
#Assume a a ball from the driving range next door lands in your yard at an average rate of 3 balls per hour during the day. 
#What is the probability that 10 or fewer golf balls will land in your yard during the afternoon, assuming the afternoon is 5 hours long?
  
#  mean is 15 = 3 * 5 for the entire afternoon
  ppois(10, 15)  #[1] 0.1184644
  
#Binomial Distribution

  lower<-qbinom(0.001, size=100, prob=0.5)
  upper<-qbinom(0.999, size=100, prob=0.5)
  n<-seq(lower,upper,1)
  q<-seq(0.001,0.999,0.001)
  dBinom100 <- data.frame(N=n, 
                          Density=dbinom(n, size=100, prob=0.5),
                          Distribution=pbinom(n, size=100, prob=0.5))  
  qBinom100 <- data.frame(Q=q, Quantile=qbinom(q, size=100, prob=0.5))  
  head(dBinom100)
  
  #ex. 4
 # Assume a coin is weighted so that it comes up heads 60% of the time. 
#What is the prbability that you will obtain 25 or more heads after 50 flips?
    
#  Use pbinom to get the probability of 25 or less heads, and subtract from 1 

 pbinom(25,50,0.6)

 #ex. 5  
# Assume you flip a fair coin 100 times.
#What is the number N that, 90% of the time, the number of heads is less than or equal to N?
   
   # Use the quantile function.  
   qbinom(0.9, 100, 0.5)
   
   
#Exponential Distribution
   
   lower <- floor(qexp(0.001, rate=0.2))
   upper <- ceiling(qexp(0.999, rate=0.2))
   t <- seq(lower,upper,0.1)
   q <- seq(0.001,0.999,0.001)
   dexp02 <- data.frame(T=t, 
                        Density=dexp(t, rate=0.2),
                        Distribution=pexp(t, rate=0.2))  
   qexp02 <- data.frame(Q=q, Quantile=qexp(q, rate=0.2))  
   head(dexp02)
   head(qexp02)
   
  #ex. 6
#Assume the lifetime of a metastable nuclear isomer ?? is exponentially distributed with a mean lifetime of 20 minutes. 
#What is the probabilty that a ?? nucleus will decay within the next 15 minutes?
     
# The rate is 1/20 = 0.05.  Use pexp to get the probability of decay
 #  in 15 or less minutes 
     pexp(15, 0.05)
     
 #ex.7
#Assume that a light bulb has a mean lifetime of 1000 hours. What is the probability that the light bulb survives to 2000 hours?
       
#  The rate is 1/1000 = 0.001.  Use pexp to get the probability of burnout within
#    2000 hours, and subtract from 1.
 pexp(2000, 0.001)