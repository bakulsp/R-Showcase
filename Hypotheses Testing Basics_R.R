# Hypothesis Testing
# Step 1: State a Null Hypothesis for a Bag of Chips, the average Fat content is assumed 
# at 2 grams

#H0 mu= 2 grams

# Step 2: State an Alternate Hypothesis for a Bag of Chips, the average Fat content is assumed 
# at greater 2 grams

#H1 mu>2 grams

#Step 3:
x_bar <- 2.1  # This is Sample Mean
mu <- 2       # This is Population Mean
s <- 0.25     # This is sample Std Deviation
n <- 35       # This is no of Samples


# Step 4: Find the T Value

t<- (x_bar-mu)/((0.25/sqrt(35)))
t

# Set your Alpha or level of significance at say 5%, with no of samples as 35, 
# degree of freedom as n-1 - 34
# Lookup t table to find the critical value with alpha as 0.05 amd df = 34
# critical value - 1.69
# Looking at t value since it is greater than critical value we expect our Null Hypotheses to be wrong

# Step 5: Find the p Value

p_value <- 1-0.9906
p_value

if (p_value < 0.05) {
  print ("Reject Null")
}