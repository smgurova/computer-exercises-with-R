# Probability

#http://www.r-tutor.com/elementary-statistics/probability-distributions
# https://www.isibang.ac.in/~athreya/psweur/
#http://www.atmos.albany.edu/facstaff/timm/ATM315spring14/R/
#https://cran.r-project.org/doc/contrib/Seefeld_StatsRBio.pdf
#https://rafalab.github.io/dsbook/probability.html
#https://districtdatalabs.silvrback.com/conditional-probability-with-r
#http://www.atmos.albany.edu/facstaff/timm/ATM315spring14/R/IPSUR.pdf


# 1.1 Samples space 
#
# For a random experiment E, the set of all possible outcomes of E is called the sample space
#and is denoted by the letter S. For the coin-toss experiment S={"Head", "TAil"}.

#A sample space is (usually) represented by a data frame, that is, a rectangular
#collection of variables.Each row of the data frame corresponds to an outcome of the experiment.

#ex.1
#Consider the random experiment of dropping a Styrofoam cup onto the floor
#from a height of four feet. The cup hits the ground and eventually comes to rest. It could land
#upside down, right side up, or it could land on its side. We represent these possible outcomes
#of the random experiment by the following.

S <- data.frame(lands = c("down", "up", "side"))
S

#ex.2
#Consider the random experiment of tossing a coin. The outcomes are H and T.

#install prob from Tools-> install packages
library(prob)
tosscoin(1) # S={H,T} sample set
#toss1
#1     H
#2     T
#number 1 tells tosscoin whah we only want to toss the coin one.

tosscoin(3)  # toss 3 times
#toss1 toss2 toss3
#1     H     H     H
#2     T     H     H
#3     H     T     H
#4     T     T     H
#5     H     H     T
#6     T     H     T
#7     H     T     T
#8     T     T     T

#1.2 Sampling from Urns

#This is perhaps the most fundamental type of random experiment. We have an urn that contains
#a bunch of distinguishable objects (balls) inside. We shake up the urn, reach inside, grab a ball,
#and take a look. That’s all.


#ordered or unordered sampling

#The prob package accomplishes sampling from urns with the urnsamples function, which has
#arguments x, size, replace, and ordered. 
# x- represents the urn from which sampling is to be done
# size- how large the sample will be
# ordered and replce - logical & specify how sampling will be performed.

#ex.3
#Let our urn simply contain three balls, labeled 1, 2, and 3, respectively. We are
#going to take a sample of size 2 from the urn.

#a) ordered with replacement 

urnsamples(1:3, size = 2, replace = TRUE, ordered = TRUE)
#by “ordered” we mean that we shall keep track of the order of the draws that we observe

#b) ordered without replacement
urnsamples(1:3, size = 2, replace = FALSE, ordered = TRUE)

# c) unordered without replacement
urnsamples(1:3, size = 2, replace = FALSE, ordered = FALSE)

#d) unordered with replacement
urnsamples(1:3, size = 2, replace = TRUE, ordered = FALSE)

#1.3 Events
#An event A is merely a collection of outcomes, or in other words, a subset of the sample
#space.A. We say that a bunch of events A1, A2, A3, . . . are
#mutually exclusive or disjoint if Ai ??? Aj = ??? for any distinct pair A_i, A_j is different.

#ex. 4
#Given a data frame sample/probability space S.

S <- tosscoin(2, makespace = TRUE)
S

#1.3.1 Functions for Finding Subsets
#ex. 5
#Form a set S with all "Heart" cards. After that find subset from 7 till 9 cards from
#"Heart", "Club", "Diamond", "Spade".

C<-cards()
subset(C, suit=="Heart")

#The function %in% helps to learn whether each value of one vector lies somewhere inside another vector.

subset(C, rank %in% 7:9)

# example for %in% function
x <- 1:10
 y <- 8:12
 y %in% x
 #[1]  TRUE  TRUE  TRUE FALSE FALSE
 
 #It is more common to want to know whether the whole vector y is in x. We can do this with the
# isin function.
 
 isin(x,y) #[1] FALSE
 
 #1.3.2 Set Union, Intersection, and Difference
 
 #Name        Denoted    Defined by elements        Code
 #Union         A ??? B       in A or B or both      union(A,B)
 #Intersection  A ??? B       in both A and B         intersect(A,B)
 #Difference    A\B       in A but not in B         setdiff(A,B)
 
 #ex. 6
 # We have two sets A={all cards "Hearts"}, B={7,8,9 cards from all types of cards}.
 #Make a neq set S - union, intersect, setdiff from A and B.
S = cards()
A = subset(S, suit == "Heart")
B = subset(S, rank %in% 7:9)
union(A, B)
intersect(A, B)
setdiff(A,B)
setdiff(B,A)  #Notice that setdiff is not symmetric

#ex. 7
#The Equally Likely Model asserts that every outcome of the sample space has
#the same probability, thus, if a sample space has n outcomes, then probs would be a vector
#of length n with identical entries 1/n. The quickest way to generate probs is with the rep
#function. We will start with the experiment of rolling a die, so that n = 6. We will construct the
#sample space, generate the probs vector, and put them together with probspace.

outcomes <- rolldie(1)
outcomes
p <- rep(1/6, times = 6)
p
#[1] 0.1666667 0.1666667 0.1666667 0.1666667 0.1666667 0.1666667
probspace(outcomes, probs = p)
probspace(1:6, probs = p) # another wrote
probspace(1:6)

#ex. 8 An unbalanced coin. 
#Suppose our coin is not perfectly balanced, for instance,
#maybe the “H” side is somewhat heavier such that the
#chances of a H appearing in a single toss is 0.70 instead of 0.5. 

probspace(tosscoin(1), probs = c(0.7, 0.3))
#toss1 probs
#1     H   0.7
#2     T   0.3

#1.4 Probability

#The factorial n! is computed with the command factorial(n) and the binomial coefficient
#with the command choose(n,k). The nsamp function will calculate the number of rows in a sample space
#made by urnsamples without actually devoting the memory resources necessary to generate
#the space. The arguments are n, the number of (distinguishable) objects in the urn, k, the
#sample size, and replace, ordered, as above.

#ex. 9
#There are 11 artists who each submit a portfolio containing 7 paintings for competition in an art exhibition. Unfortunately, the gallery director only has space in the winners’
#section to accommodate 12 paintings in a row equally spread over three consecutive walls. The
#director decides to give the first, second, and third place winners each a wall to display the
#work of their choice. The walls boast 31 separate lighting options apiece. How many displays
#are possible?
#Answer: The judges will pick 3 (ranked) winners out of 11 (with rep = FALSE, ord =TRUE).
#Each artist will select 4 of his/her paintings from 7 for display in a row (rep = FALSE,ord = TRUE), 
#and lastly, each of the 3 walls has 31 lighting possibilities (rep = TRUE, ord=TRUE).
 
n <- c(11, 7, 31)
k <- c(3, 4, 3)
r <- c(FALSE, FALSE, TRUE)      
x <- nsamp(n, k, rep = r, ord = TRUE)
prod(x)  #[1] 24774195600

#ex.10 The Birthday Problem.
#Suppose that there are n people together in a room.Each person announces the date of his/her birthday in turn. 
#The question is: what is the probability of at least one match?
#If we let the event A={there is at least one match}, then
#would like to know P(A), but as we will see, it is more convenient to calculate IP(A^)=1-P(A).

#Answer:
#P(A)=1-P(A^)=1-\frac{#A^}{#S}
#Let us then suppose that there are no matches. The first person has one 
#of 365 possible birthdays. The second person must not match the first, thus,
#the second person has only 364 available birthdays from which to choose. Similarly, the third
#person has only 363 possible birthdays, and so forth, until we reach the n
#th person, who has only (365 ??? n + 1) remaining possible days for a birthday. 
#By the Multiplication Principle, we
#have #(A^)= 365*364*...*(365 ??? n + 1)

#P(A)=1-\frac{365*364*...*(365 ??? n + 1)}{365^n}

#1.5 Conditional Probability

#ex. 11
#Consider an urn with 10 balls inside, 7 of which are red and 3 of which are green.
#Select 3 balls successively from the urn.
#Answer:
#Let A={1st ball is red}, B={2nd ball is red}, C={3rd ball is red}.
#We need to find P(ABC)=?. P(ABC)=7/10*6/9*5/8=0.29
library(prob)
 L <- rep(c("red", "green"), times = c(7, 3))
 M <- urnsamples(L, size = 3, replace = FALSE, ordered = TRUE)
 N <- probspace(M)
 prob(N, isrep(N, "red", 3))  #[1] 0.2916667
 
#a)  What is the probability of getting two "red"s?
 prob(N, isrep(N, "red", 2)) #[1] 0.525
 
#b)What is the probability of observing "red", then "green", then "red"?
 prob(N, isin(N, c("red", "green", "red"), ordered = TRUE)) #[1] 0.175
 
#c) What is the probability of observing "red", "green", and "red", in no particular order?
 prob(N, isin(N, c("red", "green", "red"))) 
 
 #1.5.1 Independent Events
 
#ex. 12
#Toss ten coins. What is the probability of observing at least one Head? 
#Answer:  A_i={ith coin shows "H"}. 
#P(at least one "H")=1-P(~A_1~A_2....~A_10)=1-(1/2)^10.
S <- tosscoin(10, makespace = TRUE)
 A <- subset(S, isrep(S, vals = "T", nrep = 10))
 1 - prob(A)
 
