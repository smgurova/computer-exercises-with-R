#Lesson 11

#Factor analysis

#https://www.promptcloud.com/blog/exploratory-factor-analysis-in-r/
#https://www.statmethods.net/advstats/factor.html

#Factor analysis is a statistical method used to describe variability among observed, correlated variables in terms of a potentially lower number of unobserved variables called factors. 
#For example, it is possible that variations in six observed variables mainly reflect the variations in two unobserved (underlying) variables. 
#Factor analysis searches for such joint variations in response to unobserved latent variables. 
#The observed variables are modelled as linear combinations of the potential factors, plus "error" terms. 
#Factor analysis aims to find independent latent variables. 

#ex.1
#We will use the Psych package in R which is a package for personality, psychometric, and psychological research.
#It consists a dataset – the bfi dataset which represents 25 personality items with 3 additional demographics for 2800 data points. 
#The columns are already classified into 5 factors thus their names start with letters A (Agreeableness), C (Conscientiousness), 
#E (Extraversion), N (Neuroticism) and O (Openness).



#Installing the Psych package and loading it
install.packages("psych")
library(psych)
#Loading the dataset
bfi_data=bfi

#Remove rows with missing values and keep only complete cases
bfi_data=bfi_data[complete.cases(bfi_data),]

#Create the correlation matrix from bfi_data
bfi_cor <- cor(bfi_data)

#Factor analysis of the data
factors_data <- fa(r = bfi_cor, nfactors = 6)

#The fa() function needs correlation matrix as r and number of factors. 
#The default value is 1 which is undesired so we will specify the factors to be 6 for this exercise.

#Getting the factor loadings and model analysis
factors_data


#Factor Analysis using method =  minres
#Call: fa(r = bfi_cor, nfactors = 6)
#Standardized loadings (pattern matrix) based upon correlation matrix
#MR1   MR2   MR3   MR4   MR5   MR6    h2   u2 com
#A1        -0.23 -0.04  0.15  0.03 -0.49  0.24 0.379 0.62 2.2
#A2         0.46  0.31 -0.20  0.09  0.33 -0.07 0.467 0.53 3.3
#A3         0.52  0.31 -0.24  0.04  0.24  0.12 0.506 0.49 2.8
#A4         0.40  0.13 -0.18  0.24  0.14  0.08 0.294 0.71 2.8
#A5         0.57  0.19 -0.25 -0.02  0.14  0.17 0.470 0.53 2.0
#C1         0.33  0.11  0.43  0.19 -0.01  0.08 0.344 0.66 2.6
#C2         0.32  0.17  0.42  0.36  0.01  0.18 0.475 0.53 3.7
#C3         0.32  0.04  0.28  0.36  0.02  0.05 0.317 0.68 3.0
#C4        -0.47  0.13 -0.39 -0.33 -0.01  0.22 0.555 0.45 3.5
#C5        -0.49  0.15 -0.21 -0.34  0.11  0.05 0.433 0.57 2.6
#E1        -0.42 -0.21  0.23  0.16  0.21  0.27 0.414 0.59 3.9
#E2        -0.62 -0.06  0.19  0.11  0.29  0.18 0.559 0.44 1.9
#E3         0.54  0.33 -0.05 -0.23 -0.14  0.17 0.507 0.49 2.5
#E4         0.61  0.19 -0.32  0.01 -0.21  0.11 0.565 0.44 2.1
#E5         0.52  0.28  0.13 -0.03 -0.20 -0.08 0.410 0.59 2.1
#N1        -0.42  0.64  0.07  0.07 -0.23 -0.14 0.666 0.33 2.2
#N2        -0.41  0.63  0.12  0.05 -0.17 -0.20 0.654 0.35 2.3
#N3        -0.40  0.62  0.06  0.06 -0.01 -0.01 0.549 0.45 1.7
#N4        -0.53  0.40  0.11 -0.05  0.21  0.09 0.506 0.49 2.4
#N5        -0.35  0.44 -0.08  0.20  0.11  0.04 0.376 0.62 2.6
#O1         0.32  0.14  0.31 -0.34 -0.02  0.15 0.357 0.64 3.8
#O2        -0.20  0.11 -0.36  0.29 -0.07  0.15 0.295 0.70 3.3
#O3         0.39  0.25  0.27 -0.43  0.01  0.09 0.485 0.52 3.4
#O4        -0.08  0.22  0.23 -0.21  0.28  0.11 0.241 0.76 4.4
#O5        -0.20 -0.02 -0.34  0.35 -0.16  0.16 0.330 0.67 3.5
#gender     0.11  0.21 -0.15  0.21  0.12 -0.21 0.184 0.82 4.9
#education  0.06 -0.02  0.09 -0.09  0.17 -0.15 0.072 0.93 3.5
#age        0.15 -0.05  0.05  0.00  0.18 -0.20 0.098 0.90 3.2

#MR1  MR2  MR3  MR4  MR5  MR6
#SS loadings           4.59 2.34 1.60 1.34 1.01 0.63
#Proportion Var        0.16 0.08 0.06 0.05 0.04 0.02
#Cumulative Var        0.16 0.25 0.30 0.35 0.39 0.41
#Proportion Explained  0.40 0.20 0.14 0.12 0.09 0.05
#Cumulative Proportion 0.40 0.60 0.74 0.86 0.95 1.00

#Mean item complexity =  2.9
#Test of the hypothesis that 6 factors are sufficient.

#The degrees of freedom for the null model are  378  and the objective function was  7.79
#The degrees of freedom for the model are 225  and the objective function was  0.57 

#The root mean square of the residuals (RMSR) is  0.02 
#The df corrected root mean square of the residuals is  0.03 
#Fit based upon off diagonal values = 0.98
#Measures of factor score adequacy             
#MR1  MR2  MR3  MR4  MR5  MR6
#Correlation of (regression) scores with factors   0.95 0.92 0.86 0.83 0.80 0.73
#Multiple R square of scores with factors          0.90 0.84 0.74 0.70 0.65 0.53
#Minimum correlation of possible factor scores     0.80 0.68 0.48 0.39 0.29 0.05

#The factor loadings show that the first factor represents N followed by C,E,A and O. 
#This means most of the members in the data have Neuroticism in the data.
#We also notice that the first five factors adequately represent the factor categories as the data is meant for.


#Now we need to consider the loadings more than 0.3 and not loading on more than one factor. 
#Note that negative values are acceptable here. So let’s first establish the cut off to improve visibility:

print(factors_data$loadings,cutoff = 0.3)
#Loadings:
 # MR2    MR3    MR1    MR5    MR4    MR6   
#A1                             -0.561         0.352
#A2                              0.644              
#A3                              0.596              
#A4                              0.411              
#A5                              0.473              
#C1                0.543                            
#C2                0.661                            
#C3                0.562                            
#C4               -0.669                            
#C5               -0.563                            
#E1                       0.614                     
#E2                       0.676                     
#E3                      -0.320         0.378       
#E4                      -0.493                0.305
#E5                      -0.391                     
#N1         0.822                                   
#N2         0.825                                   
#N3         0.690                                   
#N4         0.439         0.428                     
#N5         0.473                                   
#O1                                     0.569       
#O2                                    -0.427       
#O3                                     0.655       
#O4                       0.337         0.367       
#O5                                    -0.497       
#gender                          0.327              
#education                                          
#age                                                

#MR2   MR3   MR1   MR5   MR4   MR6
#SS loadings    2.463 2.000 1.863 1.860 1.717 0.820
#Proportion Var 0.088 0.071 0.067 0.066 0.061 0.029
#Cumulative Var 0.088 0.159 0.226 0.292 0.354 0.383

#As you can see two variables have become insignificant 
#and two other have double-loading. Next, we’ll  consider ‘4’ factors:


sevenfactor <- fa(r=bfi_cor,nfactors = 7,rotate = "oblimin",fm="minres")
 print(sevenfactor$loadings,cutoff = 0.3)

#Loadings:
 # MR2    MR3    MR5    MR1    MR4    MR7    MR6   
#A1                      -0.504                       0.306
#A2                       0.638                            
#A3                       0.630                            
#A4                       0.428                            
#A5                       0.506                            
#C1                0.534                                   
#C2                0.659                                   
#C3                0.554                                   
#C4               -0.676                                   
#C5               -0.570                                   
#E1                              0.615                     
#E2                              0.676                     
#E3                             -0.330  0.353              
#E4                       0.303 -0.498                     
#E5                             -0.395                     
#N1         0.828                                          
#N2         0.819                                          
#N3         0.681                                          
#N4         0.443                0.428                     
#N5         0.461                                          
#O1                                     0.543              
#O2                                    -0.463              
#O3                                     0.642              
#O4                              0.336  0.365              
#O5                                    -0.540              
#gender                   0.323                            
#education                                     0.401       
#age                                           0.621       

#MR2   MR3   MR5   MR1   MR4   MR7   MR6
#SS loadings    2.442 1.988 1.933 1.875 1.680 0.668 0.629
#Proportion Var 0.087 0.071 0.069 0.067 0.060 0.024 0.022
#Cumulative Var 0.087 0.158 0.227 0.294 0.354 0.378 0.401
 fa.diagram(sevenfactor)
 print(sevenfactor)
 
 
# Cluster Analysis 
 
#https://www.statmethods.net/advstats/cluster.html
#https://uc-r.github.io/kmeans_clustering
#http://girke.bioinformatics.ucr.edu/GEN242/pages/mydoc/Rclustering.html
 #https://data-flair.training/blogs/clustering-in-r-tutorial/
 
#Cluster analysis is part of the unsupervised learning. A cluster is a group of data that share similar features. We can say, 
#clustering analysis is more about discovery than a prediction. The machine searches for similarity in the data. For instance, you can use cluster analysis for the following application:
#1.Customer segmentation: Looks for similarity between groups of customers
#2.Stock Market clustering: Group stock based on performances
#3. Reduce dimensionality of a dataset by grouping observations with similar values

 #The most striking difference between supervised and unsupervised learning lies in the results. 
 #Unsupervised learning creates a new variable, the label, while supervised learning predicts an outcome.
 #The machine helps the practitioner in the quest to label the data based on close relatedness.
 #It is up to the analyst to make use of the groups and give a name to them. 
 
 #ex.1
#You have data on the total spend of customers and their ages. 
#To improve advertising, the marketing team wants to send more targeted emails to their customers. 
 
 library(ggplot2)
 df <- data.frame(age = c(18, 21, 22, 24, 26, 26, 27, 30, 31, 35, 39, 40, 41, 42, 44, 46, 47, 48, 49, 54),
                  spend = c(10, 11, 22, 15, 12, 13, 14, 33, 39, 37, 44, 27, 29, 20, 28, 21, 30, 31, 23, 24)
 )
 ggplot(df, aes(x = age, y = spend)) +
   geom_point()
 
 #In the figure above, you cluster the observations by hand and define each of the three groups.

  #K-means algorithm
 
#The algorithm tries to find groups by minimizing the distance between the observations, called local optimal solutions. 
#The distances are measured based on the coordinates of the observations. 
 
#ex.3
#We will use the Prices of Personal Computers dataset to perform our clustering analysis. 
#This dataset contains 6259 observations and 10 features. 
#The dataset observes the price from 1993 to 1995 of 486 personal computers in the US. 
#The variables are price, speed, ram, screen, cd among other. 
 
 
#Algorithm:
 #1.Import data
 #2.Train the model
 #3.Evaluate the model
 
 
 #1) Import data
 library(dplyr)
 PATH <-"https://raw.githubusercontent.com/guru99-edu/R-Programming/master/computers.csv"
 df <- read.csv(PATH) %>%
   select(-c(X, cd, multi, premium))
 glimpse(df)
 summary(df)
 
 #From the summary statistics, you can see the data has large values.
 #A good practice with k mean and distance calculation is to rescale the data so that the mean is equal to one and the standard deviation is equal to zero. 
 
 rescale_df <- df
 resclae_df
 summary(rescale_df)
 #kmeans(df, k)
           #arguments:
#df: dataset used to run the algorithm
 #k: Number of clusters
 #Let's k=3 clusters
 
 #2) Train the model 
 install.packages("animation")	
 set.seed(2345)
 library(animation)
 kmeans.ani(rescale_df[2:3], 3)
 #Select the columns 2 and 3 of rescale_df data set and run the algorithm with k sets to 3.
 #Plot the animation.
 
 pc_cluster <-kmeans(rescale_df, 5)
 #Optimal k
#You can construct the elbow graph and find the optimal k as follow:
#Step 1: Construct a function to compute the total within clusters sum of squares
#Step 2: Run the algorithm times
#Step 3: Create a data frame with the results of the algorithm
#Step 4: Plot the results
 
 #Step 1) Construct a function to compute the total within clusters sum of squares
 kmean_withinss <- function(k)  # set the number of arguments in the function
   {
   cluster <- kmeans(rescale_df, k) #run algorithm k-times
   return (cluster$tot.withinss) #Store the total within clusters sum of squares
 }
 kmean_withinss(2) #[1] 1105373774
 
 #Step 2) Run the algorithm n times
#You will use the sapply() function to run the algorithm over a range of k. 
#This technique is faster than creating a loop and store the value. 
 
 # Set maximum cluster 
 max_k <-20  # max number  20
 # Run algorithm over a range of k 
 wss <- sapply(2:max_k, kmean_withinss) # run the function kmean_withinss()
 #over a range 2:max_k, i.e. 2 to  20
 wss
 #[1] 1105373790  762231349  640802505  493184982  416605336  354598290  305237294
 #[8]  267748649  250908794  230462282  216922931  206272726  198145952  178657242
 #[15]  172761159  165151066  155376980  147659812  142104841
 
 #Step 3) Create a data frame with the results of the algorithm 
 
 # Create a data frame to plot the graph
 elbow <-data.frame(2:max_k, wss) #Create a data frame with the output of 
 #the algorithm store in wss 
 
 #Step 4) Plot the results
 ggplot(elbow, aes(x = X2.max_k, y = wss)) +
   geom_point() +
   geom_line() +
   scale_x_continuous(breaks = seq(1, 20, by = 1))
 
 pc_cluster_2 <-kmeans(rescale_df, 7)
 pc_cluster_2$cluster
 pc_cluster_2$centers
 pc_cluster_2$size	#[1] 1250 1339  933  225 1746  444  322 
 # 1 cluster - 1250 , 2 cluster-1339 3-933 ..
 # Standardization makes the interpretation easier. 
 #Positive values indicate the z-score for a given cluster is above the overall mean. 
 #For instance, cluster 5 has the highest price average among all the clusters. 
 
 center <-pc_cluster_2$centers
 center
 
 
 #To create a heat map, you proceed in three steps:
   
#1.Build a data frame with the values of the center and create a variable with the number of the cluster
#2.Reshape the data with the gather() function of the tidyr library. You want to transform data from wide to long.
#3.Create the palette of colors with colorRampPalette() function
 
 #Step 1) Build a data frame 

 library(tidyr)

# create dataset with the cluster number

cluster <- c(1: 7)
center_df <- data.frame(cluster, center)

# Reshape the data

center_reshape <- gather(center_df, features, values, price: trend)
head(center_reshape)
#cluster features   values
#1       1    price 1502.334
#2       2    price 2374.324
#3       3    price 2883.901
#4       4    price 3736.560
#5       5    price 1925.683
#6       6    price 2231.718

#Step 2) Reshape the data
#The code below create the palette of colors you will use to plot the heat map.

library(RColorBrewer)
# Create the palette
hm.palette <-colorRampPalette(rev(brewer.pal(10, 'RdYlGn')),space='Lab')

#Step 3) Visualize
#You can plot the graph and see what the clusters look like. 
# Plot the heat map
ggplot(data = center_reshape, aes(x = features, y = cluster, fill = values)) +
  scale_y_continuous(breaks = seq(1, 7, by = 1)) +
  geom_tile() +
  coord_equal() +
  scale_fill_gradientn(colours = hm.palette(90)) +
  theme_classic()