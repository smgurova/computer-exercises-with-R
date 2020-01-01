#Lesson 9

#Linear regression

#The aim of linear regression is to model a continuous variable Y as a mathematical function of one or more X variable(s), so that we can use this regression model to predict the Y when only the X is known. 
#This mathematical equation can be generalized as follows:
#Y=??1+??2X+?? ,where, ??1 is the intercept and ??2 is the slope. 
#Collectively, they are called regression coefficients. ?? is the error term, the part of Y the regression model is unable to explain.

#Graphical Analysis:
#Scatter plot: Visualize the linear relationship between the predictor and response
#Box plot: To spot any outlier observations in the variable. Having outliers in your predictor can drastically affect the predictions as they can easily affect the direction/slope of the line of best fit.
#Density plot: To see the distribution of the predictor variable. Ideally, a close to normal distribution (a bell shaped curve), without being skewed to the left or right is preferred.

#http://r-statistics.co/Linear-Regression.html


#ex.1
#The 'anscombe' data shows four different sets of observations Xi, Yj.
#Check for which cases the linear model is appropriate.
#If we want to reject H0, we have to look the value for p-value. 
#If it's big it can also be a mistake, but  we can't comfirm it with p-value.
#We have to see the plot line in the scatter plot.

zad1.Lreg=function()
{
  attach(anscombe)
  l1=lm(y1~x1)
  plot(l1)
  #return(summary(l1))
  l2=lm(y2~x2)
  plot(l2)
  #return(summary(l2))
  l3=lm(y3~x3)
  plot(l3)
  #return(summary(l3))
  l4=lm(y4~x4)
  plot(l4)
  #return(summary(l4))
}

#ex.2
#The relationship between beer consumption and blood alcohol levels is being sought. Observations were made:
  
#number of beers    5     2    9    8    3     7    3     5  3    5
#blood alcohol      0.1 0.03 0.19 0.12 0.04 0.095 0.07 0.06 0.02 0.05

#Construct a linear model. Check the hypothesis that drinking 1 more beer increases your alcohol percentage by 0.02
#against the alternative that the increase is less. Test the free member in the model is zero.

zad2.lReg = function ()
{
  x = c (5,2,9, 8,3,7,3,5,3,5)
  y = c (0.1,0.03,0.19,0.12,0.04,0.095,0.07,0.06,0.02,0.05)
  l = lm (y ~ x)
  sm = summary (l)
  b1 = sm $ coefficients [2,1]
  e1 = sm $ coefficients [2,2]
  t = (b1-0.02) / e1
  #return(sm)
  #return (t)
  res = pt (t, df = 8, lower.tail = T)
  return (res) # [1] 0.4127342
  #if res is big => H0 is accepted
}
#Call:
 # lm(formula = y ~ x)

#Residuals:
 # Min      1Q  Median      3Q     Max 
#-0.0275 -0.0187 -0.0071  0.0194  0.0357 

#Coefficients:
 # Estimate Std. Error t value Pr(>|t|)    
#(Intercept) -0.018500   0.019230  -0.962 0.364200    
#x            0.019200   0.003511   5.469 0.000595 ***
  ---
 # Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Residual standard error: 0.02483 on 8 degrees of freedom
#Multiple R-squared:  0.789,	Adjusted R-squared:  0.7626 
#F-statistic: 29.91 on 1 and 8 DF,  p-value: 0.0005953
  

#ex. 3
#Galileo explores the trajectory along which objects that hit horizontal velocity before launching. 
#For the purposemeasures two dimensions — height and distance of fall.

#height 100 200 300 450 600 800 1000
#distance 253 337 395 451 495 534 574

#The unit of measure used at the time in Italy was 'points'. Construct a linear model describing the data.
#Present the result graphically. Look for another better model. 

# abline(a = NULL, b = NULL, h = NULL, v = NULL, reg = NULL, coef = NULL, untf = FALSE, ...)
#This function adds one or more straight lines through the current plot.

d=c(253 ,  337,   395,   451,   495,   534,  574)
h=c(100,   200 ,  300 ,  450 ,  600 ,  800,  1000)
plot(d,h,
     main="Plot",
     ylab="height",
     xlab="distance")
#plot the original set ot values
l=lm(d~h) #linear regression
abline(l) #
plot(l,which=1)
summary(l)
#Call:
 # lm(formula = d ~ h)

#Residuals:
  #1        2        3        4        5        6        7 
#-49.8788   0.7086  25.2959  31.1769  25.0578  -2.7675 -29.5929 

#Coefficients:
 # Estimate Std. Error t value Pr(>|t|)    
#(Intercept) 269.46607   24.18421  11.142 0.000102 ***
 # h             0.33413    0.04181   7.992 0.000495 ***
  ---
  #Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Residual standard error: 33.5 on 5 degrees of freedom
#Multiple R-squared:  0.9274,	Adjusted R-squared:  0.9129 
#F-statistic: 63.88 on 1 and 5 DF,  p-value: 0.0004951

l2=lm(d~h+I(h^2))
sm2=summary(l2)
sm
#Call:
#lm(formula = d ~ h + I(h^2))

#Residuals:
 # 1       2       3       4       5       6       7 
#-14.420   9.192  13.624   2.060  -6.158 -12.912   8.614 

#Coefficients:
 # Estimate Std. Error t value Pr(>|t|)    
#(Intercept)  2.002e+02  1.695e+01  11.811 0.000294 ***
 # h            7.062e-01  7.568e-02   9.332 0.000734 ***
  #I(h^2)      -3.410e-04  6.754e-05  -5.049 0.007237 ** 
  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Residual standard error: 13.79 on 4 degrees of freedom
#Multiple R-squared:  0.9902,	Adjusted R-squared:  0.9852 
#F-statistic: 201.1 on 2 and 4 DF,  p-value: 9.696e-05
 
  
  
#The one-way analysis of variance (ANOVA), also known as one-factor ANOVA, is an extension of independent 
#two-samples t-test for comparing means in a situation where there are more than two groups. In one-way ANOVA, the data is organized into several groups 
#base on one single grouping variable (also called factor variable). This tutorial describes the basic principle of the one-way ANOVA test and provides
#practical anova test examples in R software.

#ANOVA test hypotheses:
#Null hypothesis: the means of the different groups are the same
#Alternative hypothesis: At least one sample mean is not equal to the others  
  
anova(l,l2)
#Analysis of Variance Table

#Model 1: d ~ h
#Model 2: d ~ h + I(h^2)
#Res.Df    RSS Df Sum of Sq     F   Pr(>F)   
#1      5 5611.6                               
#2      4  761.2  1    4850.4 25.49 0.007237 **
  ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
  
plot(l2,which=1)
plot(h,d)
abline(l)
c=l2$coeffients
x=seq(min(h),max(h),length.out=100)
y=c[1]+c[2]*x+c[3]*x^2+c[4]*x^3
lines(x,y,col='blue')
#plot na dannite dava nesho, koeto ne prilicha na prava, zatova probvame s po-golemi stepeni 2,3
# izpolzvame I(h^2)/I(h^3), za da vzemem indentiteta, zashoto she pravi vs vujmojni proizvedeniq 

#ex.4
#Each of the three examiners reviewed 10 exam papers. Can we accept that the examiners have
#same criteria?
  
#checking 1 5 4 4 6 4 6 3 3 4 5
#checking 2 3 2 4 5 3 4 3 4 2 4
#checking 3 4 6 4 2 4 5 5 3 6 4

#x1:x11,x12,...xn1,
....
#xn:xn1,xn2,...xnn

#H0: Ex1=Ex2=..=Exn  (* in this case n=3*)

#SStot total sum square=suma suma (xij-x_)^2 ( totalno razsejvane na vs grupi)
#SSwithin=suma(xij-xj_)^2 (vutreshno razsejvane na grupata)
#SStreatment=suma(xj_-x)^2 (mejdugrupovo razsejvane)
#SStot=SSwithin+SStreatment
#t=(SStreatment/(p-1))/(SSwithin/(n-p))
#t<-F (Fisher distribution)

zad4.analise=function()
{
  x1=c(5, 4 ,4 ,6, 4, 6, 3, 3, 4, 5)
  x2=c(3, 2, 4, 5, 3, 4, 3, 4, 2, 4)
  x3=c(4, 6, 4, 2, 4, 5, 5, 3, 6, 4)
  d=data.frame(x1,x2,x3) #The function data.frame() creates data frames, tightly coupled collections of variables which share many of the properties of matrices and of lists, 
  #used as the fundamental data structure by most of R's modeling software.
  ds=stack(d) #Stacking vectors concatenates multiple vectors into a single vector along with a factor indicating where each observation originated. 
  
  #if they are normal for every group
  a1=anova(lm(values~ind,data=ds))
  a=aov(values~ind,data=ds)
  s=summary(a)
  sm=summary(a1)
  s
  sm
  #p-value >=10% accept
  #p-value <5% reject
}

zad4.analise()
#Df            Sum Sq          Mean Sq         F value          Pr(>F)      
#Min.   : 2.00   Min.   : 6.067   Min.   :1.219   Min.   :2.489   Min.   :0.1018  
#1st Qu.: 8.25   1st Qu.:12.775   1st Qu.:1.672   1st Qu.:2.489   1st Qu.:0.1018  
#Median :14.50   Median :19.483   Median :2.126   Median :2.489   Median :0.1018  
#Mean   :14.50   Mean   :19.483   Mean   :2.126   Mean   :2.489   Mean   :0.1018  
#3rd Qu.:20.75   3rd Qu.:26.192   3rd Qu.:2.580   3rd Qu.:2.489   3rd Qu.:0.1018  
#Max.   :27.00   Max.   :32.900   Max.   :3.033   Max.   :2.489   Max.   :0.1018  
#NA's   :1       NA's   :1       

# Pr(>F) p-value 10% ---> accept


##
x1=c(5, 4 ,4 ,6, 4, 6, 3, 3, 4, 5)
x2=c(3, 2, 4, 5, 3, 4, 3, 4, 2, 4)
x3=c(4, 6, 4, 2, 4, 5, 5, 3, 6, 4)
d=data.frame(x1,x2,x3) #The function data.frame() creates data frames, tightly coupled collections of variables which share many of the properties of matrices and of lists, 
 #used as the fundamental data structure by most of R's modeling software.
ds=stack(d)
 l=lm(values~ind,data=ds)
 summary(l)
 #Call:
  # lm(formula = values ~ ind, data = ds)
 
 #Residuals:
  # Min     1Q Median     3Q    Max 
 #-2.3   -0.4   -0.3    0.6    1.7 
 
 #Coefficients:
  # Estimate Std. Error t value Pr(>|t|)    
 #(Intercept)   4.4000     0.3491  12.605 7.99e-13 ***
  # indx2        -1.0000     0.4937  -2.026   0.0528 .  
 #indx3        -0.1000     0.4937  -0.203   0.8410    
 #---
  # Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
 
 #Residual standard error: 1.104 on 27 degrees of freedom
 #Multiple R-squared:  0.1557,	Adjusted R-squared:  0.09315 
 #F-statistic: 2.489 on 2 and 27 DF,  p-value: 0.1018
 plot(l)
 
 #ex.4
# 'InsectSpray' data contains the number of insects in areas treated with different insect repellents.
#Present the data graphically. Check that the preparations work the same.  Which preparations can we claim
 #to have the same effect?
  
 attach(InsectSprays)
 InsectSpreys
 boxplot(count~spray)
 a=shapiro.test(count[spray=='A']) 
 a
 #Shapiro-Wilk normality test
 #data:  count[spray == "A"]
 #W = 0.95757, p-value = 0.7487

 b=shapiro.test(count[spray=='B'])
 b # return info for the test
 c=shapiro.test(count[spray=='C'])
 c # return info for the test
 d=shapiro.test(count[spray=='D'])
 d # return info for the test
 e=shapiro.test(count[spray=='E'])
 e # return info for the test
 f=shapiro.test(count[spray=='F'])
 f # return info for the test
 
 total=kruskal.test(count~spray)  
  totalAF=kruskal.test(count~spray,data=InsectSprays[spray=='A'|spray=='F',])
 totalAF
 
# Kruskal-Wallis rank sum test
 #data:  count by spray
 #Kruskal-Wallis chi-squared = 0.65762, df = 1, p-value = 0.4174