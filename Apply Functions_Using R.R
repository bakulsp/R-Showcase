# Apply Functions
# This Program will demonstrate Apply, lapply, sapply, mapply, vapply and tapply

# Create a matrix to work with
m <- matrix(c(1:10,11:20),nrow=10,ncol=2)
m

# Calculate mean of rows and Columns using Apply Function

mean_m_row <- apply(m,1,mean
mean_m_row

mean_m_col <- apply(m,2,mean)
mean_m_col

# Calculate sum of rows and Columns using Apply Function

sum_m_row <- apply(m,1,sum)
sum_m_row
 
sum_m_col <- apply(m,2,sum)
sum_m_col                   
                   
# Create a function and use it with Apply

Divide <- apply(m,1:2,function(x) x/2)
Divide

# List Apply (lapply function)
# List Apply function will always need a list to work with

l <- list (m)
l

# Use lapply to calculate mean

l_mean <- lapply(l,mean)
l_mean

l_sum <- lapply(l, sum)
l_sum

# Create a list of Matrices
A <- matrix(1:9,3,3)
B <- matrix (4:15,4,3)
C <- matrix (8:10,3,2)

MyList <- list(A,B,C)
MyList

#Use lapply function to extract a targeted entry from the list using selection operator "["]

extract_col <- lapply(MyList, "[", ,2)
extract_col

# Use lapply to extract a targeted entry from the list using a selection operator

extract_row <- lapply(MyList, "[",1 ,)
extract_row

# sapply function is a simplified apply function

extract_row_simple <- sapply(MyList, "[", 1,)
extract_row_simple

# vapply function (BP TO VALIDATE AND GOOGLE MORE EXAMPLES)
l <- list(a =1:10,b=11:20)
l.fivenum <- vapply(l, fivenum, c("Min." =0, “1st Qu.”=0, "Median."=0, “3rd Qu.”=0, "Max."=0))
class(l.fivenum)
l.fivenum


# mapply

list_1 <- list(a =c(1:10), b= c(11:20))
list_2 <- list(c = c(21:30), d = c(31:40))

mapply_res <- mapply(sum, list_1$a, list_1$b, list_2$c, list_2$d)
mapply_res
-




