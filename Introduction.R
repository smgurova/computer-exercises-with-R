## Lesson 1 - Introduction ##

#R is a programming language and free software environment for statistical computing and
#graphics supported by the R Foundation for Statistical Computing.
#The R language is widely used among statisticians and data miners for developing statistical
#software and data analysis.

# download  free
# https://www.r-project.org/ 

# download fisrt R vesrion (***) and after that R studio 
# https://rstudio.com/products/rstudio/

#https://www.econometrics-with-r.org/2-1-random-variables-and-probability-distributions.html


# In R  we have reserved words
# enter in the console 
?reserved # it will open the help and you will be able to see all reserved words

typeof(5) # "double"
typeof("5") # character
tepeof(5i)  #complex

##########
# Aritmetic operations

# we difine x and y 
x<-5
y<-16
 x+y # addition
#[1] 21
 x-y # difference
#[1] -11
 x*y  # multiplication
#[1] 80
 x^y  # power 
#[1] 152587890625
y/x
#[1] 3.2
 y%%x #division by module
[1] 1
 y%/%x #integer division
[1] 3
###

# Operations with vectors
#create 2 vector with a same length
x<-c(2,8,3)
y<-c(6,4,1)
 x+y
#[1]  8 12  4
 x-y
 #[1] -4  4  2
 x/y
 #[1] 0.3333333 2.0000000 3.0000000
 
 #ex.1
 #creating a multiplication table
 num= 3
 #use for loop to interate 10 times
 for(i in 1:10)
 {print(paste(num,'x',i,'=',num*i))}
 
 # Loop
 #1.1 
 x<-5
 if(x<0)
   {print("the number is  negative")}
 else
 {print("it's a possitive number")}
 
 #ex.2
 a=c(5,7,2,9)
 ifelse(a%%2==0,"even","odd")
 #[1] "odd"  "odd"  "even" "odd"
 
 #ex.3
 x<-c(2,5,3,9,8,11,6)
 count<-0
 for(val in x){
   if(val%%2==0) count=count+1}
 print(count)
  # [1] 3 
 
 #ex.4 
 i<-1
 while(i<6){
   print(i)
   i=i+1
 }
 
 # break statement
 X<-1:5
 for(val in x){
   if(val==3) {break}
   print(val)
 }
 # will print 1,2 from the vector .. break at 3
 
 #create a vector
 x<-c(1,5,4,9,0)
 typeof(x) # double
 length(x) #5

  # 2 manner for creation of vectors
x<-1:7;x
y<--2:2;y
 
# 3rd manner

x<- seq(1,3,by=0.2) # start from 1 til 3 with a step 0.2
x
#[1] 1.0 1.2 1.4 1.6 1.8 2.0 2.2 2.4 2.6 2.8 3.0
x[3] # select the 3 element - 1.4
x[c(2,4)] # select the 2 and 4 element from the vector - 1.2, 1.6
x[-1] # remove the first element from the vector 
# [1] 1.2 1.4 1.6 1.8 2.0 2.2 2.4 2.6 2.8 3.0
x[2]<-0;x  # the old value 1.2 is now 0
# [1] 1.0 0.0 1.4 1.6 1.8 2.0 2.2 2.4 2.6 2.8 3.0
x[x>1.3]<-5;x # values bigger than 1.3 now are 5
#[1] 1 0 5 5 5 5 5 5 5 5 5

#functions
 #ex.5 degrees
pow<-function(x,y)
  {result<-x^y
print(paste(x,"raised to the power",y,"is",result))}
pow(2,6) #[1] "2 raised to the power 6 is 64"

#ex.6 recursive function
recursive.factorial<-function(x)
{
  if(x==0) return(1)
  else 
    return(x*recursive.factorial(x-1))
}
recursive.factorial(3) #[1] 6

#Creating matrices
x<-matrix(1:9, nrow=3)
# with x we call the matrix
x

x[c(1,2),c(2,3)] #select rows 1,2 and column (2,3)
x[c(3,2),] # create new matrix without row 1 and  row 3 is now row 2 
x[-1] # select the matrix in 1 vector without the first element (1)
x[x>5] #[1] 6 7 8 9
x[2,2]<-10;x # element a(2,2)=5 before now is a(2,2)=10

#graphics
#1. barplot
#ex.1
max.temp<-c(23,56,21,2,0,4,5,66)
barplot(max.temp)

#ex.2
age<-c(17,18,5,6,1,10,17,18,5,5,17,1)
table(age)
barplot(table(age),
        main="Age count of 12 students",
        xlab="age",
        ylab="count",
        border="red",
        col="blue",
        density=10)

#2. histogram
#ex.1
str(airquality)
Temperature<-airquality $Temp
hist(Temperature)
hist(Temperature,
     main="max daily temo at LGA",
     xlab="temp in degree F",
     xlim=c(50,100),
     col="darkmagenta",
     freq=FALSE)

#3. boxplot
str(airquality)
boxplot(airquality)

#ex
x<-seq(-pi,pi,0.1)
     plot(x,sin(x), 
          main="The sine function",
          ylab="sin(x)")
     
#3D plots

cone<-function(x,y)
{
  sqrt(x^2+y^2)}
  x<-y<-seq(-1,1,length=20)
z<-outer(x,y,cone)
persp(x,y,z)
