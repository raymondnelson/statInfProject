########## Coursera statinference-005 Statistical Inference 9-2-2014 to 9-29-2014
#
# Raymond Nelson
# 
# Course project
#
######
#
# The exponential distribution can be simulated in R with rexp(n, lambda) 
# where lambda is the rate parameter. 
# The mean of exponential distribution is 1/lambda 
# and the standard deviation is also also 1/lambda. 
# Set lambda = 0.2 for all of the simulations. 
# In this simulation, you will investigate the distribution 
# of averages of 40 exponential(0.2)s. 
# Note that you will need to do a thousand or so 
# simulated averages of 40 exponentials.
# 
# Illustrate via simulation and associated explanatory text 
# the properties of the distribution of the mean of 40 exponential(0.2)s.  
# You should
# 1. Show where the distribution is centered at and compare it to the theoretical center of the distribution.
# 2. Show how variable it is and compare it to the theoretical variance of the distribution.
# 3. Show that the distribution is approximately normal.
# 4. Evaluate the coverage of the confidence interval for 1/lambda: .
#
############################

# first set the working directory
setwd("~/Documents/RAYMOND/Coursera/Statistical Inference 9-1-2014 to 9-29-2014/statInfProject")

simNum <- 1000
numVars <- 40
lambda <- .2

# construct the NULL matrix to hold the data
# 40 columns represent random exp values
# 1000 rows will hold the results of the simulation run

set.seed(1234)
simMatrix <- matrix(rexp(nosim * n, lambda), nosim)

# 1. Show where the distribution is centered at and compare it to the theoretical center of the distribution.

# using rowMeans
rm <- rowMeans(simMatrix)
m <- mean(rm)
# another method using apply
m <- mean(apply(simMatrix, 1, mean))

# the theoretical center of the distribution is 1 / lambda
tm <- 1 / lambda

# 2. Show how variable it is and compare it to the theoretical variance of the distribution.

# calculation of the sampling SD is done with apply 
s <- mean(apply(simMatrix, 1, sd))
# standard deviation
s
# variance
s^2

# the theoretical SD of the distribution is 1 / lambda for which the square is the variance.
tsd <- 1 / lambda
tsd^2

# 3. Show that the distribution is approximately normal.

# histogram to evaluate normality of the distribution
hist(rowMeans(simMatrix), breaks = 30)

# another example with ggplot
rm <- data.frame(rowMeans(simMatrix))
ggplot(rm, aes(x=rowMeans.simMatrix.)) + 
        geom_histogram(aes(y=..density..),
                       binwidth=.5,
                       colour="black", fill="white") +
        geom_density(alpha=.2, fill="pink")           

# 4. Evaluate the coverage of the confidence interval for 1/lambda: .

# first calculate the standard error of the SD of the simulated distribution
ses <- sd(apply(simMatrix, 1, sd))
# upper limit
ul <- s + 1.9599 * (ses / sqrt(n))
ul        
# lower limit        
ll <- s - 1.9599 * (ses / sqrt(n))
ll


