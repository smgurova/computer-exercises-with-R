# Lesson 3

#The number of possible combinations is   C(n,r) = \frac{n!}{r!(n-r)!}.
#Some recursive relations:
 # C(n,r) = C(n,n-r)
#C(n,l)C(l,r) = C(n-1,r)C(n-1,r-1)
#\sum_{k=0}^n C(n,k) = 2^n
#C(m+n,r)=C(m,0)C(n,r)+C(m,1)C(n,r???1)+…C(m,r)C(n,0)

#Combinations, among many other things, give the coefficients for the binomial expression:
 # (a+b)^n = \sum_{i=0}^n C(n,i) a^{n-i}b^i.

#ex.1
#To make a matrix with all combinations of the elements of S (of size n) taken r at a time:
S <- letters[1:5] # generate a vector with letters a,b,c,d,e
n <- length(S) # find the length of the vector
r <- 3 # initialization 
result <- t(combn(S,r)) # combination 
result

apply(result,1,function(x) paste0(x,collapse="")) # just return as a vector of strings

#The number of possible combinations is C(n,r) = \frac{n!}{r!(n-r)!}. 
#This number is computed in R by the choose function:
choose(5,0)
## [1] 1
choose(5,1)
## [1] 5
choose(5,2)
## [1] 10
choose(5,3)
## [1] 10
choose(5,4)
## [1] 5
choose(5,5)
## [1] 1

#ex. 2
#Generate the  Pascal Triangle

pascal <- matrix(rep(NA,100),ncol=10) # generate it in a matrix form
for(i in 0:10) # matrix 10x10
  for(j in 0:i)
    pascal[i,j] <- choose(i,j)  #  put each Pascal coef. in the matrix
print(pascal, na.print=" " )

#To make the same but where the order is relevant, 
#a permutation (if m is equal to the length of S we get all possible arrangements) (ex.1):

library(gtools)
S <- letters[1:5] # generate a vector with letters a,b,c,d,e
n <- length(S) # find the length of the vector
r <- 3 # initialization 
result <- permutations(n, r, S)
apply(result,1,function(x) paste0(x,collapse=""))

#The number of possible permutations is  P(n,r)=n!(n???r)!
#No restrictions:

result <- as.matrix(expand.grid(lapply(numeric(r), function(x) S)), ncol=r)
apply(result,1,function(x) paste0(x,collapse=""))  
#This is a vector with r^n elements.


#ex.3
#Write a function that calculates by number p how many people n should choose
#in such a way that the birthdays of at least two of them are likely to be greater than p.

func3=function(p)
{x=1
currp=1 # the probability
while(currp>p) #currp should be less than p, p is also probability
{currp=(currp*(366-x))/366 # calculate the new probability 
x=x+1}  # number of people
return(x)} # return number of people
 
func3(0.2) # p=02. probability  [1] 35 people
func3(0.5)  #p-0.5  [1] 23   people

#ex.4
#Throw 100 dice. How many 6s appear.

fun4=function()
{randoms=sample(1:6,100,replace=TRUE)
br=0
for(num in randoms)
{if(num==6)
{br=br+1
}
  }
  return(br)
}

fun4() # write it in the console in this way , to see the result [1] 13

#ex.5
#Throw a coin 10 times. Did the 'EETET' sequence appear? 
#Throw the coin until it appeared, how many throws did you make?

fun5=function()
{
  res=c('E','E','T','E','T') #  create our result
  vector=sample(c('E','T'),10,replace=TRUE) # sampling  a vector cointaing E, T.
  # 10 times  with replacing
  if(findVectorinVector(vector,res))  # function check of the vector and result
  {return ("YES")}
  else
  {br=10   # the coin is thrown 10 times
  while(TRUE) # while it is true make the statements in the body below
  {br=br+1     
  newRand=sample(c('E','T'),1,replace=TRUE) # generate new vector
  vector=append(vector,newRand,length(vector)+1)
  #  append(x, values, after=length(x))
#  x: vector
 # values: for appends
#  after: subscript position which the values are to be appended

  if(findVectorinVector(vector,res))
  {return(br);}
  }
  }
  }
findVectorinVector=function(source,item)
{
  lens=length(source)
  len=length(item)
  start=1
  while(TRUE)
  {
   # print("s")
  # print(star)
  #  print("len")
   # print(start+len-1)
  #  print("res")
    print(source[start:(start+len-1)])
    if(all(source[start:(start+len-1)]==item))
       {return(TRUE)}
       if(start+len>lens)
       {return(FALSE)}
       start=start+1
  }
}
fun5() # call the function

#ex.6
#20 dice are thrown. What is the probability of them falling below 3 6s?
#Compare theoretical probability with experimental data.

#https://stat.ethz.ch/R-manual/R-devel/library/stats/html/Binomial.html
# The binomial distribution with size = n and prob = p has density

#p(x) = choose(n, x) p^x (1-p)^(n-x)


fun6=function()
{
  # n number 
  #size - how many dice we have
  #prob - probability of 1 manner
  y=rbinom(n=100,size=20,prob=1/6) # generate binomial distribution with 
 #with n=100 of observations
  pbinom(2.9,20,1/6) # generate binomial distribution with vector q=2.9
  #p(x=10)
  #pbinom(9.9,20,1/6)
  #p(5<x<=8)
  #pbinom(8,20,1/6)-pbinom(4.9,20,1/6)
}
fun6() #[1] 0.3286591