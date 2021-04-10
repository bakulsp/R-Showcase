
# Practice 2 Exercise | Data Science with R
# Date : 7th February 2021
# Author: Bakul Purohit
# Source Data used : Practice2.csv
#Purpose: Practice hypothesis testing
# Problem Statement :
# Please investigate the below and answer the questions:
  
# 1.	Do data science students’ income is different before and after the data science class?
# 2.	Do data science students make more money after taking the course compared to what they were making prior to taking the course? Is this significantly different?
# 3.	What is the average income for males compared to female students after taking the course?
# 4.	Do female students make more money than male students after taking the data science course?
# 5.	Create two categories of income and call it “income_categry” ($0<50,000 and  >50,000), then examine and see if there is any difference in income “income_category” comparing males and females students after taking the Simplilearn certificate course 

getwd()
setwd("/Users/bakul/Documents/DataSciencewithR/Sample Data")

library(utils)
mydata_2 <- read.csv("Practice2.csv",header =TRUE, sep = ",")


# *************** Question 1 ********************

#Income BEFORE taking the data science certificate course
mean_IncBefore <- mean(mydata_2$IncBefore) # Find the Mean Value of Income Before the Course
mean_IncBefore

hist(mydata_2$IncBefore) # Plot a Histogram
Before <- density(mydata_2$IncBefore) # See a Density Plot

Before
plot(Before)

#Income AFTER taking the data science certificate course

mean_IncAft <- mean(mydata_2$IncAfter) # Find the Mean Value of Income Before the Course
mean_IncAft

hist(mydata_2$IncAfter) # Plot a Histogram
After <- density(mydata_2$IncAft) # See a Density Plot

After
plot(After)

# Compare and observe whether Income Before and After is different

#Test to see if there is any statistically significant change in Income  AFTER taking the data science certificate course
t.test(mydata_2$IncBefore,mydata_2$IncAfter, paired=TRUE)

# Inference --> Since p Value is less than 0.05 we reject Null and that means there 
# is a statistically significant evidence that Income After Taking the course is not same as Income Before.



# *************** Question 3 ********************

# Let's split (Prepare) data by gender

attach(mydata_2)
library(dplyr)
FemaleIncome <- (filter(mydata_2,Sex == "Female"))
FemaleIncome
MaleIncome <- (filter(mydata_2,Sex == "Male"))
MaleIncome

# Income Before, Male versus Female 
# let's look at average income before taking the data science course by gender 
mean(FemaleIncome$IncBefore)
mean(MaleIncome$IncBefore)
#Visualize the income by gender 
hist(FemaleIncome$IncBefore)
hist(MaleIncome$IncBefore)

#See if there is any significant difference in income before taking the class by gender 
t.test(FemaleIncome$IncBefore, MaleIncome$IncBefore) 


# *************** Question 4 ********************
# Income AFTER, Male versus Female 
# let's look at average income before taking the data science course by gender 
mean(FemaleIncome$IncAfter)
mean(MaleIncome$IncAfter)
#Visualize the income by gender 
hist(FemaleIncome$IncAfter)
hist(MaleIncome$IncAfter)

#See if there is any significant difference in income before taking the class by gender 
t.test(FemaleIncome$IncAfter, MaleIncome$IncAfter) 


