set.seed(185794) # Setting a random seed
library(pracma) # Library for capturing running time



##************************Implementation of main function******************************##
##*************************************************************************************##
##*************************************************************************************##

Randomized_median <- function(Inp, power)
{
  ## From here Implementation of the algorithm starts
  n = length(Inp) # Length of the input
  expec = ceil(n^(power)) # Expected size of random sample
  comp = 0
  t = ceil(sqrt(expec*log(n))) # parameter for finding a and b
  p = expec/n # probability of selection of each element in random sample from Input array
  
  # Random Sampling
  random_sample = Inp[sample(c(TRUE, FALSE),n, prob = c(p, 1-p), replace = TRUE)]
  l = length(random_sample)
  
  # Check for size of Random Sample
  comp = comp + 1
  if(l > 2*expec)
  {message("Algorithm gone through bad random sample of the array. Please run it again!");return(Inf)}
  
  
  # Sorting Random Sample (Takes only o(n) time)
  random_sample = sort(random_sample)
  comp = comp + l*log(l)
  # Fixing a and b
  a = random_sample[ceil(expec/2-t)]
  b = random_sample[ceil(expec/2+t+1)]
  
  # Finding rank of a in original array (Takes n comparison)
  comp = comp + n
  Inp_1 = Inp[which(a < Inp)]
  rank_a = n - length(Inp_1)
  comp = comp + 2
  if(rank_a > n/2 || rank_a < n/2-(2*t*n/expec))
  {message("Algorithm gone through bad random sample of the array. Please run it again!");return(Inf)}
  
  # Finding rank of b in original array (Takes 0.5n + o(n) comparison)
  Inp_2 = Inp_1[which(b>=Inp_1)]
  comp = comp + length(Inp_1)
  rank_b = rank_a + length(Inp_2)
  comp = comp + 2
  if(rank_b < n/2 || length(Inp_2) > (4*t*n/expec))
  {message("Algorithm gone through bad random sample of the array. Please run it again!");return(Inf)}
  if(n%%2 == 0){med = mean(sort(Inp_2,partial = c(n/2-rank_a,n/2-rank_a+1))[c(n/2-rank_a,n/2-rank_a+1)])}
  # Returning median
  else{med = sort(Inp_2,partial = n/2-rank_a+1)[n/2-rank_a+1]}
  comp = comp + length(Inp_2)*log(length(Inp_2))
  return(c(med,comp))
}

##*************************************************************************************##
##*************************************************************************************##
##*********************************End of Function*************************************##



## Driver code
## You can fix any value of n (should be an integer), p (numeric between 0 and 1) and 
## itr (no. of iterations for given n and p).
## from the given n and p, function randomly draw size of array from 
## Binomial(n,p) distribution and generates a random array of that size.

## Note that function will not return nothing, it just print the output from 
## function Randomized_median() and In-built function median(). Also, it will mention 
## the running time of each function.


run<- function(n, p,power, itr)
{
  for(i in 1:itr)
  {
    m <- rbinom(1, n, p)
    cat(m, "\n")
    Inp <- runif(m, min = -10000000, max=10000000)
    tic()
    x = Randomized_median(Inp, power)
    cat(x[2]/m)
    cat("Output from Randomized Algorithm: ",x[1],"in",x[2],"comparisons\n")
    toc()
    
    tic()
    cat("Output from Inbuilt median finding Algorithm: ", median(Inp), "\n")
    toc()
    cat("\n")
  }
}

# Example
run(100000000,0.8,2/3, 100)
Randomized_median(Inp, 0.75)
