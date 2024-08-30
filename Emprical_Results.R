library(latex2exp)
library(ggplot2)
library(hrbrthemes)
library(stringr)
library(scales)
library(gridExtra)

##************************************************************************************##
##****************************Analysis about constant c*******************************##
##************************************************************************************##

legend = paste("c =",c(0.67,1:4))
col = c("palevioletred3", "steelblue3", "tan3", "palegreen3", "dimgrey")
n = 1:100
expec = 2*sqrt(log(n))
t = n^(3/8)
plot(x=n,y = t - expec,"l", ylab = "", xlab = TeX("$n$"),lwd = 2.5,ylim = c(-7,3),col = col[1])
title(ylab=TeX("$n^{3/8}-2\\sqrt{1.5c\\log(n)}$"), line=2, cex.lab=1.2)

for(i in 1:4)
{
  n = 1:100
  expec = 2*sqrt(1.5*i*log(n))
  t = n^(3/8)
  lines(x=n,y = t - expec,"l", lwd = 2.5, col = col[i+1])
}

legend("top", legend = legend, col = col, lwd=2.2,xpd = TRUE, horiz = TRUE, cex = 1.25, 
       seg.len = 1, bty = "n",inset = c(0,-0.3),x.intersp = 0.25)
##************************************************************************************##



##************************************************************************************##
##**********************Empirical Analysis of the Algorithm***************************##
##************************************************************************************##
##************************************************************************************##

## Running of Experiment

set.seed(173423)
n <- c(50,100,200,300,400,500,600,700,800,900,1000,1e4,1e5,1e6,1e7,1e8)
Data <- vector(length=16)
Time_data <- matrix(0,ncol = 16,nrow= 2000)
Time_algo <- vector(length = 16)

for(i in 1:16)
{
  success = 0
  t = 0
  ns = n[i]
  for(j in 1:2000)
  {
    Inp = runif(ns,min=-100000000,max=10000000)
    
    tm1 <- system.time({med1 = Randomized_median(Inp,0.67)})
    tm2 <- system.time({med2 = median(Inp)})
    
    if(med1 == med2)
    {
      success = success + 1
      Time_data[j,2] = tm1[3] - tm2[3]
      t = t + tm1
    }
  }
  Data[i] = success
  Time_algo[i] = t/success
}


## Plotting barplots for success rate for different input size

data <- as.data.frame(cbind(n,Data/n))
colnames(data) <- c("n","Proportion_of_Correct_output")

p <- ggplot(data, aes(x=n, y=Proportion_of_Correct_output)) + geom_bar(stat = "identity")
p


## Plotting density plots for delta t for different input size
Time_data <- as.data.frame(Time_data)

p1 = ggplot(data=Time_data,aes(x=V13)) +
  geom_density(fill="#69b3a2", color="#e9ecef", alpha=0.8)+
  labs(title = TeX("Approximate density distribution of $\\Delta t$ for $n=10^5$"),
       x = TeX("$\\Delta t$"), y = "Density")
  

p2 = ggplot(data=Time_data,aes(x=V14)) +
  geom_density(fill="sienna1", color="#e9ecef", alpha=0.8)+
  labs(title = TeX("Approximate density distribution of $\\Delta t$ for $n=10^6$"),
       x = TeX("$\\Delta t$"), y = "Density")

p3 = ggplot(data=Time_data,aes(x=V15)) +
  geom_density(fill="deepskyblue1", color="#e9ecef", alpha=0.8)+
  labs(title = TeX("Approximate density distribution of $\\Delta t$ for $n=10^7$"),
       x = TeX("$\\Delta t$"), y = "Density")

p4 = ggplot(data=Time_data,aes(x=V16)) +
  geom_density(fill="plum2", color="#e9ecef", alpha=0.8)+
  labs(title = TeX("Approximate density distribution of $\\Delta t$ for $n=10^8$"),
       x = TeX("$\\Delta t$"), y = "Density")

Patterns <- grid.arrange(p1, p2, p3, p4, top = TeX("Analysis of $\\Delta t $"))


## Obtaining Summary Statistics for Delta t
Time_data = as.matrix(Time_data)
## Function of mode
getmode <- function(v) 
{
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
apply(Time_data,2,mean)
apply(Time_data,2,median)
apply(Time_data,2,getmode)


## Plot of Average time vs n
data <- as.data.frame(cbind(n,Time_algo))
p5 = ggplot(data, aes(x=n, y=Time_algo)) +
      geom_line( color="red", size=1.5, alpha=0.9, linetype=1) +
      labs(title = TeX("Average Time taken by Algoritm vs $n$"),
           y = TeX("$\\bar{T}_{Randomized\\,\\, median}$"), x = TeX("$n$"))+
  theme(axis.title.y = element_text(angle=0, vjust = 0.5))
      theme_ipsum()
 

## Plot of Average time vs log(n)
data <- as.data.frame(cbind(log(n),Time_algo))
p6 <- ggplot(data, aes(x=V1, y=Time_algo)) +
      geom_line(color="blue", size=1.5, alpha=0.9, linetype=1) +
      labs(title = TeX("Average Time taken by Algoritm vs $\\log(n)$"),
           y = TeX("$\\bar{T}_{Randomized\\,\\, median}$"), x = TeX("$\\log(n)$"))+
  theme(axis.title.y = element_text(angle=0, vjust = 0.5))
  theme_ipsum()

Patterns <- grid.arrange(p5,p6)

 

v1 <- c(322, 971, 2753, 3737, 4841, 5368, 5927, 10397, 18649, 26458, 33225, 42877, 74782, 146884, 208160, 265906, 318251,
        589581, 1093223, 1591146, 2032185, 2481772, 4719898, 8855825, 12807924, 16770828, 20502494, 39197081, 75747617,
        111182316, 145995166, 179674957)
v2 <- c(277, 725, 1201, 1090, 1553, 1798, 
        1825, 5248, 9633, 17708, 17734, 30830, 71733, 118772, 193154, 301797, 234259, 505781, 1200819, 2110340, 2546989, 2675274, 5745577, 11132467, 
        16358396, 21985048, 28961496, 53554670, 115726666, 164986429, 203223259, 289289681)
x <- c(100,200,400,600,800,900,1000,2000,4000,6000,8000,10000,20000,40000,60000,80000,100000,200000,400000,600000,800000,1000000,2000000,4000000,6000000,8000000,10000000,20000000,40000000,60000000,80000000, 100000000)

v1 <- v1/x
v2 <- v2/x

x = log10(x)
dat <- as.data.frame(cbind(x,v1,v2))

plot(x=x,y=v1,"l",col="red",lwd=1.5,xlim = c(2,8.1), ylim = range(1, 6.88250),
     main=TeX("Number of Comparisons Deterministic vs Randomized $(p=n^{-0.25})$"),
     xlab =  (TeX("$\\log(n)$")),ylab = 'Ratio of Number of comparisons and n')
lines(x=x,y=v2,"l",col="blue",lwd=1.5)
legend("topright",legend = c("Deterministic Algorithm" , "Randomized Algorithm"),col = c("blue","red"),lwd=2)



v1 = c(273, 524, 1438, 4613, 4963, 5644, 6240, 13126, 22451, 31666, 41145, 49877, 89275, 162421, 220951, 275722, 345892, 639500, 1189286, 1643756, 2114790, 2652757, 5052149, 9300295, 13355721, 17426948, 
       20927133, 40656266, 77742766, 114498973, 151329971, 185461673)
v2 = c(297, 403, 875, 1917, 2753, 2751, 5009, 6096, 11607, 23787, 19149, 27498, 79640, 103810, 154054, 225495, 369510, 667899, 995255, 1675415, 2389166, 2634054, 6195388, 10948683, 
       18052577, 23783323, 31923136, 53308704, 131786674, 178556324, 203424973, 282540917)

x <- c(100,200,400,600,800,900,1000,2000,4000,6000,8000,10000,20000,40000,60000,80000,100000,200000,400000,600000,800000,1000000,2000000,4000000,6000000,8000000,10000000,20000000,40000000,60000000,80000000, 100000000)

v1 <- v1/x
v2 <- v2/x
x = log10(x)
dat <- as.data.frame(cbind(x,v1,v2))

plot(x=x,y=v1,"l",col="red",lwd=1.5,xlim = c(2,8.1), ylim = range(1, 6.88250),
     main=TeX("Number of Comparisons Deterministic vs Randomized $(p=n^{-0.33})$"),
     xlab =  (TeX("$\\log(n)$")),ylab = 'Ratio of Number of comparisons and n')
lines(x=x,y=v2,"l",col="blue",lwd=1.5)
legend("topright",legend = c("Deterministic Algorithm" , "Randomized Algorithm"),col = c("blue","red"),lwd=2)







