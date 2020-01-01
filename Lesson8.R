# Lesson 8

#Hypotheses - continued

#ex.1
#The results are obtained at 150 dice and are written in the table:
 # points 1 2 3 4 5 6
#frequency 22 21 22 27 22 26
#Can we assume that the dice is correct?
  
  
  # x1, .., xn <-N (0,1) we want them to be normally distributed
  # We know x
  # H0 - distribution type is normal, H1- is not normal distribution
  # mi - values for each interval
  # ei - Target values at each interval
  # t = sum (mi-ei) ^ 2 / ei, if small is H0, otherwise - H1
  
#Chi-test

#rep(x,n) replicates the values in x, n times. It is a generic function, and the (internal) default method is described here.

  zad1.hypothesis= function ()
  {
    m = c (1,2, 3, 4, 5, 6)
    e = rep (25,6)
    rr = (m-e) ^ 2
    t=sum (rr) / 25
    #evaluate the probability to be true
    # t- t data lowertail the different side
    # k = pchisq (t)
    k1 = chisq.test (m, p = rep(1/6,6))
    return (k1)
  }
  zad1.hypothesis()
# Chi-squared test for given probabilities
#  data:  m
# X-squared = 5, df = 5, p-value = 0.4159
  
#ex.2
#The variable pi2000 of the 'UsingR' package contains the first 2000 digits of the number pi. Look at the first 200 digits.
#Can we assume that every digit has the same probability. 
  
zad2.chiSq=function()
  {
    library(UsingR)
    data=pi2000[1:200]
    t=table(data)
    #default p - uniform
    res=chisq.test(t)
    return(res)
  }
zad2.chiSq()
#Chi-squared test for given probabilities
#data:  t
#X-squared = 7.2, df = 9, p-value = 0.6163
  
#ex.3
#The frequency of occurrence of letters in English is as follows:
# letter    E     T      A     O     I      N     ...
#frequency 12.7% 9.56% 8.17% 7.51% 6.97% 6.75% 48.34%

#A text of 1036 letters has been analyzed and it turns out that the number of occurrences of these letters is respectively

# letter    E   T   A O   I N
#frequency 102 108 90 95 82 40

#Is the text in English?

zad3.chisq=function()
{
  m=c(102, 108,   90,   95,   82,   40)
  el=1036-sum(m)
  m=c(102,  108,   90,   95,   82,   40,el)
  pi=c(0.127,0.0956,0.0817,0.0751,0.0697,0.0675,0.4834)
  res=chisq.test(m,p=pi)
  return(res)
}
zad3.chisq()
#Chi-squared test for given probabilities
#data:  m
#X-squared = 26.396, df = 6, p-value = 0.0001878

#ex.4
#The table gives information about the passengers injured in the crash depending on the seat belt

#           injuries
#            without   light  medium    to heavy
#with belt    12813     647     359         42
#without belt  65963    4000     2642       303
#Can we claim that seat belts reduce the likelihood of drivers being injured?
 
#H0: X, Y  independent variables
#H1: otherwise
#the values of the marginal distributions should be equal to the principle distribution
#nij - value for every element
#ri - sums for rows
#cj - sums for columns
# It should be nij/n=cj=ri
#t = chisq.test()

zad4.chisq=function()
{
  m=matrix(c(12813,647,359,42,65963,4000,2642,303),2,4)
  t=chisq.test(m)
  return(t)
}
zad4.chisq()

#Pearson's Chi-squared test
#data:  m
#X-squared = 150.6, df = 3, p-value < 2.2e-16

#ex.5
#We assume that there is a link between the quality of the product and the day of the week 
#in which is produced. 500 products were measured.

#           M  T  W  Th  F
#excellent  44 74 79 72 31
#good       14 25 27 24 10
#average    15 20 20 23 9
#bad         3 5   5  0 0

#Was the claim confirmed?

zad5.chisq=function()
{
  m=matrix(c(44,74,79,72,31,14,25,27,24,10,15,20,20,23,9,3,5,5,0,0),4,5)
  t=chisq.test(m)
  return(t)
}
zad5.chisq()

#Pearson's Chi-squared test
#data:  m
#X-squared = 53.173, df = 12, p-value = 3.837e-07