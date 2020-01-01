# Lesson 2
#ex.1 

# Create a vector x = (8,3,3,7,15,9,12,4,9,10,5,1). Turn the vector into a 6x2 matrix.
#Specify an array name for the matrix such as r1, r2, r3, r4, r5, r6. Add another column containing
#odd numbers - 1,3,5,7,9,11. Sort the matrix by the first column in ascending order.
#Sort by the first two columns.
#
a<-c(8,3,8,7,15,9,12,4,9,10,5,1)
 matrr<-matrix(a,6,2) # create matrix 6x2
 rownames(matrr)<-c("r1","r2","r3","r4","r5","r6") # named rows
 coll=seq(1,12,2) # create a vector from 1 to 12 with step 2 
 # so this seq. contains odd numbers 
 matr2=cbind(matrr,coll) 
matr2  #create matrix 6x3 where the last column is named
          coll
 r1  8 12    1
 r2  3  4    3
 r3  8  9    5
 r4  7 10    7
 r5 15  5    9
 r6  9  1   11
 
 matr2=cbind(matrr,seq(1,12,2)) # create a matrix 6x3 where thee 3column is without name
 matr2
 [,1] [,2] [,3]
 r1    8   12    1
 r2    3    4    3
 r3    8    9    5
 r4    7   10    7
 r5   15    5    9
 r6    9    1   11
 
 ord=order(matr2[,1]) 
 matr2[ord,] # sorting   by 1 coloumn
 
 cur=order(matr2[,1],matr[,2])  # sorting the matrix by 1,2 coloumns
 matr2[cur,]
 
 
 #ex.2 

# Write a function f (x), calculating f (x) = Sum ((100s ^ 2) / (9 + 4x ^ 2), s = 1, x).
# Let the vector x = (1,4,7,3,18,6) be given.
 #Calculate f (x) for each element of x, save the result in a new vector w.
# How arithmetic mean on w?
#
 fsum=function(x)
 {i=0
 sum=0
 while(i<=x)
 {sum=sum+(100*i*i*i)/(9+4*x*x*x*x)
   i=i+1
 }
 return(sum)
 }
zad1fun=function(x)
{
  w=sapply(x,fsum);
  sum=0
  for(el in w){
    sum=sum+el
  }
return(sum/length(x))
}
c<-c(1,4,7,3,18,6) # create the vector v
fsum(c)
#[1] 7.6923076923 0.0968054211 0.0104025798 0.3003003003 0.0002381446 0.0192566917
zad1fun(c)
#[1] 8.632512 # math expectation


#ex. 3
#Check out the 'homedata' data from the 'UsingR' package. Create two vectors containing house prices in 2000.
#and 1970. Define:
#a) the most expensive and the cheapest house in 2000. Their prices in 1970.
#b) the prices of the 5 most expensive houses in 2000.
#c) the number of houses more expensive than 750,000 in 2000.
#d) the average price in 1970. of houses from c)
#e) the price in 2000. of those houses whose price has decreased
#f) the 10 houses with the highest percentage increase in price.
#Divide the houses into 3 groups: cheap with 2000 price. to 100,000, medium - from 100,000 to 500,000 and expensive
#over 500,000. Calculate the number and average cost for each of the groups. 

install.packages("UsingR")
library("UsingR")
data(homedata)
attach(homedata)

#a)
indMax=which(max(y2000)==y2000) #finding the index of the expensive house
indMin=which(min(y2000)==y2000) #finding the index of the cheapest house
> y1970[indMin] # finding the price of the house
#[1] 10000
> y1970[indMax] #  finding the price of the house
#[1] 198900

#b)
sorted=order(y2000)
m=y2000[sorted]
m[1:5]

#c)
y2000[750000<y2000] # finding the prices's house in 2000 under 750 000
#[1] 1085000  782500 1042000  988900  880300  831800 1093500 1182800  780300  943800  886300
#[12]  818300  886300  886300  839800  986300  885000  792800  760700
length(y2000[750000<y2000]) # number of them
#[1] 19

#d

s=length(y2000[750000<y2000])
sm=sum(y2000[750000<y2000])
sm/s  #[1] 913300

#e)
indexes=which(y2000<y1970)
y2000[indexes] # [1] 7400

#v

procent=(y2000-y1970)/y1970
ordered=ordered(procent)
ord=y2000[ordered]
ord[1:10]
#  [1] 335600 474600 397600 328000 125400  97600 171800 146800 506200 445000

#z
res=cut(y2000,c(0,5000,10000,12200202),labels=c("ch","ave","exp"))
table(res)
# finding the number of cheapest, average, expensive houses...
res
#ch  ave  exp 
#0    1 6840 

#ex.4
#
#Check out the survey data from the 'MASS' package.
#a) Build a barplot for students' gender and smoking. Reorder smoking data.
#Make a common barplot for both.
#b) Build a barplot for the height of the wells. Make a separate boxplot for men and women
#C) Make a histogram for the student's heart rate. Add the density.
#d) Make separate histograms for height of men and women.
#e) Divide the cold by age groups into 3 groups - "young" to 30, "middle age" from 30 to 60, and
#"over" 60. Present graphically.
#f) Make a table for the distribution of smokers in different age groups, present graphically.


library(MASS)
data(survey)
survey[1:5,] # show the first 5 lines from the survey with all columns
view(survey)
attach(survey)
t=table(Sex)
 t
#Sex
#Female   Male 
#118    118 

barlot(t)

s=table(Smoke)
s
#Smoke
#Heavy Never Occas Regul 
#11   189    19    17 
barplot(s)
levels(s)
#NULL
 levels(Smoke)[c(2,3,4,1)]
#[1] "Never" "Occas" "Regul" "Heavy" # prenarejdane

sm=factor(Smoke,levels=levels(Smoke)[c(2,3,4,1)])
table(Smoke)
s=table(sm)
barplot(s)

p=table(Sex,sm)
p
#
#sm
#Sex      Never Occas Regul Heavy
#Female    99     9     5     5
#Male      89    10    12     6
barplot(p,beside=T,legend.text=levels(Sex))

#b)
boxplot(Height~Sex)

#c)
hist(Pulse)
#adding density
is.na(Pulse)
which(is.na(Pulse))
d=density(Pulse,na.rm=T)
d
#Call:
#density.default(x = Pulse, na.rm = T)
#
#Data: Pulse (192 obs.);	Bandwidth 'bw' = 3.286
#
#x                y            
#Min.   : 25.14   Min.   :7.160e-06  
#1st Qu.: 47.32   1st Qu.:9.431e-04  
#Median : 69.50   Median :4.717e-03  
#Mean   : 69.50   Mean   :1.126e-02  
#3rd Qu.: 91.68   3rd Qu.:2.096e-02  
#Max.   :113.86   Max.   :3.536e-02  

hist(Pulse)
hist(Pulse,probability=T)
lines(d)

#d)

plot(Height,Pulse)
plot(Height,Wr.Hnd)
plot(NW.Hnd,Wr.Hnd)

sv=na.omit(survey)
cor(sv$Wr.Hnd,sv$NW.Hnd) # correlation [1] 0.9651297 <=1
l=lm(Wr.Hnd~NW.Hnd) # linejnost mejdu dvete velichini
abline(l) # put the regression line
identify(NW.Hnd,Wr.Hnd,n=2)
survey[3,]
plot(x,y)
lm(y~x)

