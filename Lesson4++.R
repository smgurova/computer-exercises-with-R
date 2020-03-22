# Lesson 4++

#1.Uniform distribution (continuous) 
#Notation	U~(a,b)
#Parameters:	 -\infty <a<b<\infty 
#Support: x\in [a,b]
#PDF: {\begin{cases}{\frac {1}{b-a}}    &{\text{for x\in [a,b]}
#                     \\0               & {\text{otherwise}}\end{cases}}}
#CDF:{\begin{cases} 
#                  0      &{\text{for x<a}\\
#      \\{\frac {x-a}{b-a}}   &{\text{for x\in [a,b]\\
#                   1       &{\text{for }}x>b\end{cases}}} 
#Mean: \frac {1}{2}}(a+b)}
#Median:  \frac {1}{2}}(a+b)}
#Mode:	any value in {\displaystyle (a,b)}(a,b)
#Variance:	\frac {1}{12}}(b-a)^{2}}

#dunif(x, min = 0, max = 1, log = FALSE)  - gives the density
#punif(q, min = 0, max = 1, lower.tail = TRUE, log.p = FALSE) - gives the distribution function
#qunif(p, min = 0, max = 1, lower.tail = TRUE, log.p = FALSE) - gives the quantile function 
#runif(n, min = 0, max = 1) -generates random deviates,
#where 0 and 1 are by default


#Arguments:
#x, q	- vector of quantiles.
#p-	vector of probabilities.
#n-number of observations. If length(n) > 1, the length is taken to be the number required.
#min, max	-lower and upper limits of the distribution. Must be finite.
#log, log.p	-logical; if TRUE, probabilities p are given as log(p).
#lower.tail	- if TRUE (default), probabilities are P[X <= x], otherwise, P[X > x].


#ex.1
#A) Generate 10 realizations of a random uniform variable in [2,7]
runif(10,2,7)
#B) Draw a histogram that estimate the density of 100 random uniform variables.
x=runif(100)
hist(x,probability = TRUE,col=gray(.5),main="uniform in [0,1]")
#C)  Evaluate the density of a random uniform variable in [0,1] in 0.5 
dunif(0.5,0,1) #[1] 1
#D) Evaluate the destribution function of a random uniform variable in 0.2, 
#this means --> P(X<0.2)
punif(0.2,0,1) #[1] 0.2
#E)Evaluate in case  P(X>0.2)
punif(0.2,0,1, lower.tail=FALSE) #[1] 0.8
#F) Find the  quantile  
qunif(0.6,0,1) #[1] 0.6
qunif(0.3,0,1) #[1] 0.3
qunif(0.3,0,1,lower.tail = FALSE)  #[1] 0.7


#2.Binomial distribution

#Notation:	Bi(n,p)
#Parameters:	 n\in \{0,1,2,..}– number of trials,
#                p\in [0,1]}– success probability for each trials
#               q=1-p - unsuccess trials
#{\displaystyle q=1-p}{\displaystyle q=1-p}
#Support:	k\in \{0,1,...,n\}
#pmf:	\binom {n}{k}}p^{k}q^{n-k}}
#CDF:	 I_{q}(n-k,1+k)} idicator function 
#Mean:	np
#Median:	[np] 
#Mode:	[(n+1)p] or [(n+1)p]-1  
#Variance:	 npq


#dbinom(x, size, prob, log = FALSE) - gives the density P(x=k)
#pbinom(q, size, prob, lower.tail = TRUE, log.p = FALSE)-gives the distribution function
#qbinom(p, size, prob, lower.tail = TRUE, log.p = FALSE)-gives the quantile function
#rbinom(n, size, prob)-random deviates.

#Arguments:
#x, q-vector of quantiles.
#p-vector of probabilities.
#n-number of observations. If length(n) > 1, the length is taken to be the number required.
#size-number of trials (zero or more).
#prob-probability of success on each trial.
#log, log.p-logical; if TRUE, probabilities p are given as log(p).
#lower.tail-logical; if TRUE (default), probabilities are P[X <= x], otherwise, P[X > x].

#ex.2
#A) Draw a histogram of x~Bi(10,0.8)
y=dbinom(seq(0,10,1),10,0.8)
plot(0:10,y,type="s",col="red")
#B)Find the third quantile of x~Bi(5,0.3) 
# fisrt quantile =25%, second =50%, third=75%
qbinom(0.75,5,0.3) #[1] 2
#C) Find that x for which P(x>=y)<=0.25
qbinom(0.25,5,0.3, lower.tail=FALSE) #[1] 2
#D) Evaluae the destribution function 
pbinom(2,5,0.3) #[1] 0.83692
#E)Find the realizations for 10 successful trials with p=0.8 where n=100 
#and draw a histogram
y=rbinom(100,10,0.8)
hist(y,probability = TRUE,col=gray(.9),main="Binomial for N=100, p=0.8")

#3.Poisson distribution

#Use for: model the number of customers
#who arrive in a given system in a single time, where \lambda is the average

#Notation:	Po(\lambda)
#Parameters:	 \lambda \in R^{+} 
#Support:	 k\in {N _{0} (Natural numbers starting from 0)
#pmf:	\frac {\lambda ^{k}e^{-\lambda }}{k!}}
#Mean:	\lambda 
#Variance: \lambda

#dpois(x, lambda, log = FALSE)
#ppois(q, lambda, lower.tail = TRUE, log.p = FALSE)
#qpois(p, lambda, lower.tail = TRUE, log.p = FALSE)
#rpois(n, lambda)

#Arguments
#x-vector of (non-negative integer) quantiles.
#q-vector of quantiles.
#p-vector of probabilities.
#n-number of random values to return.
#lambda-vector of (non-negative) means.
#log, log.p	-logical; if TRUE, probabilities p are given as log(p).
#lower.tail	-logical; if TRUE (default), probabilities are P[X <= x], otherwise, P[X > x].

#ex.3
#A) Draw a histogram of x~Po(4)
z=dpois(seq(0,15,1),4)
plot(0:15,z,type="s",col="darkred")

#ex.4
#A)If an average of 12 cars cross a bridge per minute, find 
#the probability of up to 16 cars crossing the bridge in the next minute.
ppois(16,lambda = 12) #[1] 0.898709
#B) Find the probability 17 and more cars to cross the bridge. P[x>16]
ppois(16,lambda = 12,lower.tail = FALSE) #[1] 0.101291
#C) Find the third quantile of x~Po(5)
qpois(0.75,5) #[1] 6
#D) Draq a histogram of 100 random variables x~Po(4)
t=rpois(100,4)
hist(t,probability = TRUE,col=gray(0.5),main="Poission with lambda=4")


#4.Normal distribution

#Notation:	N(\mu ,\sigma ^{2})}
#Parameters:	 \mu \in R=mean(location),\sigma ^{2}>0=variance (squared scale)
#Support:	 x\in R
#PDF: \frac {1}{\sigma {\sqrt {2\pi }}}}e^{-{\frac {1}{2}}\left({\frac {x-\mu }{\sigma }}\right)^{2}}}
#Mean:	\mu 
#Median:	\mu 
#Mode:	\mu 
#Variance:	\sigma ^{2}

#dnorm(x, mean = 0, sd = 1, log = FALSE)- Density
#pnorm(q, mean = 0, sd = 1, lower.tail = TRUE, log.p = FALSE)- distribution function
#qnorm(p, mean = 0, sd = 1, lower.tail = TRUE, log.p = FALSE)-quantile function
#rnorm(n, mean = 0, sd = 1)-random generation

#Arguments
#x, q-vector of quantiles.
#p-vector of probabilities.
#n-number of observations. If length(n) > 1, the length is taken to be the number required.
#mean-vector of means.
#sd-vector of standard deviations.
#log, log.p-logical; if TRUE, probabilities p are given as log(p).
#lower.tail-logical; if TRUE (default), probabilities are P[X <= x] otherwise, P[X > x].

#ex.5
#Find the density function with the formula when \mu=0, \sigma^2=1 for x \in[-3,3]. 
x=seq(-3,3,0.1)
y=dnorm(x,mean=0,sd=1)
plot(x,y,type="l",col="green")

#ex.6
#A) If x~N(0,1) find the area of the shaded part
#from the  figure befor when x=2, t.e P[x<=2]
pnorm(2,mean=0,sd=1) #[1] 0.9772499
#B) The area of the non shaded part, t.e. P[x>2]
pnorm(2,mea=0,sd=1,lower.tail = FALSE) #[1] 0.02275013
#C) Find the tird quantile 
qnorm(0.75,0,1)  #[1] 0.6744898
#D) Find that x  for which P(y=>x)<=0.25
qnorm(0.25,0,1,lower.tail = FALSE)  #[1] 0.6744898
pnorm(0.6744898,0,1) #[1] 0.75 
# qnorm is the reverse of pnorm

#ex.7
#It is known that the distribution of unloaded gasoline 
#to customers at a power station is X ~ N (20,5).
#A) Simulate the estimated loaded quantities of the next 30 clients
#and draw a histogram
x=rnorm(30,mean=20,sd=5); x
hist(x,probability = TRUE,col=gray(0.9),main="Normal distrtibution with mean=10, sd^2=25")
y=dnorm(seq(5,32,0.5),mean=20,sd=5)
lines(seq(5,32,0.5),y,type="l")
#B) Simulate for 1000

x=rnorm(1000,mean=20,sd=5); 
hist(x,probability = TRUE,col=gray(0.9),main="Normal distrtibution with mean=10, sd^2=25")
x1=seq(5,32,0.5)
y1=dnorm(x1,mean=20,sd=5)
lines(x1,y1,type="l")

#ex.8 
#If the results of the college entrance exam test are average 72 and 
#standard deviation of arithmetic mean 15.2. What is the expected 
#percentage of students from the whole population who have at least 84 points?
# Find for wich x~N(72,15.5^2) that P[x>84]

pnorm(84,mean=72,sd=15.2, lower.tail = FALSE) #[1] 0.2149176 ~ 21.5%

#A)Simulate approx. results for 5 obvervations

rnorm(5,mean=72,sd=15.2)

#ex.9
#If the results of the IQ test are normally distributed with a mean of 100 and
#a standard deviation of 16. Is it common for a person to score above 150 points? 
#Simulate 10 results of such observation.

#Find that x~N(100,16^2) that P[x>=150] ?
pnorm(150,mean=100,sd=16,lower.tail = FALSE)
#[1] 0.0008890253 => NO bc it's too small
rnorm(10,mean=100,sd=16)


