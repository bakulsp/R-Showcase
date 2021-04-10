# Course | PG DS- Data Science with R
# Author: Bakul Purohit # Date: 13th February 2021
# Project 1 | Retail Analysis with Walmart Data
# Domain : Retail Industry - Inventory Management|
# Source Data used: Walmart_Store_sales.csv


#Step 1: Data Read and Explore Dataset

# >   Set up directories and import the files
getwd()
setwd("/Users/bakul/Documents/DataSciencewithR/Sample Data from Simon")

# > Import the Input Dataset csv file
# > Use read.csv function from utils library

library(utils)
mydata <- read.csv("Walmart_Store_sales.csv",header =TRUE, sep = ",")

# > Fetch structure of source data using str function and also see a few records using head function
str(mydata)

# > Summarize the Dataset using Summary Function
summary(mydata)

# > attach data for cross referencing simplicity
attach(mydata)

# Perform Exploratory Analysis
mean(Weekly_Sales) # validate Mean of Weekly Sales Column
median(Weekly_Sales)

# this means we have 143 weeks worth of data per Store
table(Store)

# Qs 1 --> Which store has Maximum Sales?

library(dplyr)
library(magrittr)

# Use summarize function to extract key values from the Dataset
summary_1 <-
  mydata %>% 
  group_by(Store) %>% 
  summarize(mean = mean(Weekly_Sales),
            sum = sum(Weekly_Sales),
            max = max(Weekly_Sales), 
            std = sd(Weekly_Sales))

# Arrange Data
arrange <- arrange(summary_1, desc(max))
arrange

barplot(arrange$max,col=1:45,main= "Store Wise Sales", xlab="Store No", ylab="Sales in $",names.arg = c(1:45))

# Which store has maximum standard deviation i.e.,
# the sales vary a lot. Also, find out the coefficient of mean to standard deviation
# Qs 2 | Part 1--> Which store has Maximum Std Deviation? 
arrange <- arrange(summary_1, desc(sd))
arrange

# Qs 2 | Part 2--> Which store has Maximum Coefficient of Variation? 
summary_1$cv= round(((summary_1$std/summary_1$mean)*100), 2)
arrange <- arrange(summary_1, desc(cv))
arrange


# Qs 3 Which store/s has good quarterly growth rate in Q3’2012
# Data Extraction and Alignment is essential befoire arriving at a solution

# First we need to change Date format from character to Format Date
# Find out the Data Type of each variables in Dataset

sapply(mydata,class) 

# Align the Date Format to a directly interpretable format
# This will change Date Field from Character to Date

mydata$date_vetted <- as.Date(Date, format = "%d-%m-%Y")
sapply(mydata,class)
# Fetch the Year and Month
mydata$month_derived <- format(as.Date(mydata$date_vetted, format="%d-%m-%Y"),"%m")
mydata$year_derived <- format(as.Date(mydata$date_vetted, format="%d-%m-%Y"),"%Y")
sapply(mydata, class) # Validate the class of the derived Month and Year Values

# Reformat Month Data from Character to Numeric to use it for mathematical operations

mydata$month_arith <-as.numeric(mydata$month_derived)

# create a variable named Quarter for data mapping into Quarters

attach(mydata)
mydata$quarter[month_arith < 4] <- "Q1"
mydata$quarter[month_arith > 3 & month_arith <= 6] <- "Q2"
mydata$quarter[month_arith > 6 & month_arith <= 9] <- "Q3"
mydata$quarter[month_arith > 9] <- "Q4"

# Construct the Quarter in a Year
mydata$year_quarter  <- paste(mydata$year_derived,mydata$quarter, sep = "-")

# Preparatory work for finding Quarterly Growth Insight

mydata_agg <- aggregate(mydata$Weekly_Sales, by=list( store=mydata$Store, y_q=mydata$year_quarter), FUN=sum)


mydata_agg$store_y_q <- <- paste(mydata_agg$store,mydata_agg$y_q)
mydata_agg <- arrange(mydata_agg,(store_y_q) )
attach(mydata_agg)
mydata_final <- select(mydata_agg, store, y_q, x)  

library(plyr)
df4 <- ddply(mydata_final,"store",transform, growth=c(NA,exp(diff(log(x)))-1))
df4$growth = round((df4$growth*100),1) 


df5 <- filter(df4,y_q == "2012-Q3")
df5 <- arrange(df5,  desc(growth) )
View(df5)

# Q4 Compare Holiday Sales

df_hd <- filter(mydata, Holiday_Flag=="1")
df_non_hd <- filter(mydata, Holiday_Flag !="1")

# Average Non Holiday Sales across all stores from 2011 to 2013 (for entire dataset)

mean(df_non_hd$Weekly_Sales)
df_hd$sales <- as.numeric(df_hd$Weekly_Sales)


df_hd$flag[df_hd$sales > 1041256] <- "1"
df_hd2 <- filter(df_hd,flag == "1")
distinct(df_hd2, Date)


# Qs 5 Compare a Monthly and Semester View of Sales in the Units and Provide meaningful interpretation

# Monthly Data
mydata$year_month <- format(as.Date(mydata$date_vetted, format="%d-%m-%Y"),"%Y-%m")


mydata_m1 <- aggregate(mydata$Weekly_Sales, by=list( y_m=mydata$year_month), FUN=sum)
mydata_m1 <- arrange(mydata_m1,(y_m) )
mydata_m1$monthly_sale <- round((mydata_m1$x/1000000),2)

attach(mydata_m1)
plot(monthly_sale, type ="l")


library(ggplot2)
ggplot(mydata_m1, aes(x=y_m, y=monthly_sale, group=1)) + geom_line(col="gray", linetype = "solid") + 
  geom_point(col="blue") +  
  theme(axis.text.x=element_text(color = "blue", size=11, angle=90, vjust=.8, hjust=0.8)) 


# Semester View (Half Yearly)
#

mydata$semester[month_arith <= 6 ] <- "S1"
mydata$semester[month_arith > 6] <- "S2"
attach(mydata)
table(semester)

mydata$year_semester <- paste(mydata$year_derived,mydata$semester)

mydata_s1 <- aggregate(mydata$Weekly_Sales, by=list( y_s=mydata$year_semester), FUN=sum)
mydata_s1 <- arrange(mydata_s1,  (y_s) )
mydata_s1$half_year_sale <- round((mydata_s1$x/1000000),2)


ggplot(mydata_s1, aes(x=y_s, y=half_year_sale , group=1)) + geom_line(col="green", linetype = "solid") + 
  geom_point(col="gray") +  
  theme(axis.text.x=element_text(color = "black", size=11, angle=90, vjust=.8, hjust=0.8)) 


# Qs-7 Statistical Model
# For Store 1 – Build  prediction models to forecast demand
# Linear Regression – Utilize variables like date and restructure dates as 1 for 5 Feb 2010 (starting from the earliest date in order). Hypothesize if CPI, unemployment, and fuel price have any impact on sales.

# Select the model which gives best accuracy.


mydata_store_1 <- filter(mydata,Store =="1")

mydata_store_1$date <- as.Date(mydata_store_1$Date,format = "%d-%m-%Y")
mydata_store_1 <- arrange(mydata_store_1,(date) )

mydata_store_1 <- cbind(date_new = rownames(mydata_store_1), mydata_store_1)
rownames(mydata_store_1) <- 1:nrow(mydata_store_1)
mydata_store_1$sales <- as.numeric(mydata_store_1$Weekly_Sales)
mydata_store_1$date_new <- as.numeric(mydata_store_1$date_new)


model_obj = lm(sales ~ date_new + CPI + Unemployment + Fuel_Price , data=mydata_store_1)  
summary(model_obj)
AIC(model_obj)

BIC(model_obj)

# Change dates into days by creating new variable.

mydata$day <- format(as.Date(mydata$date_vetted, format="%d-%m-%Y"),"%d")


