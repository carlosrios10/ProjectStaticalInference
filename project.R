getwd()
setwd("C:\\Users\\Usuarioç\\Desktop\\carlos\\statical-inference\\ProjectStaticalInference")

nosim <- 100
n <- 10
sd(apply(matrix(rnorm(nosim * n), nosim), 1, mean))

rnorm(nosim * n)
m<-matrix(rnorm(nosim * n), nosim)

library(manipulate)
myHist <- function(mu){
        g <- ggplot(galton, aes(x = child))
        g <- g + geom_histogram(fill = "salmon",
                                binwidth=1, aes(y = ..density..), colour = "black")
        g <- g + geom_density(size = 2)
        g <- g + geom_vline(xintercept = mu, size = 2)
        mse <- round(mean((galton$child - mu)^2), 3)
        g <- g + labs(title = paste('mu = ', mu, ' MSE = ', mse))
        g
}
manipulate(myHist(mu), mu = slider(62, 74, step = 0.5))

#Illustrate via simulation and associated explanatory text the properties of the distribution of the mean of 40 exponential(0.2)s
lambda<-0.2
popultaionMean<-1/lambda
populationSd<-1/lambda
popupationVar<-1/((lambda)^2)
nosim<-1000
n<-40
sampleMean<-data.frame(x=apply(matrix(rexp(nosim * n,lambda), nosim), 1, mean))
#1. Show where the distribution is centered at and compare it to the theoretical center of the distribution.
mean(sampleMean$x)
u
#2. Show how variable it is and compare it to the theoretical variance of the distribution.
var(sampleMean$x)
var/n

#3. Show that the distribution is approximately normal.
g<-ggplot(sampleMean, aes(x = x))
g<- g + geom_histogram(aes(y = ..density..),binwidth=0.1, position = "stack")  
g<- g + stat_function(fun = dnorm,size=2,colour = "gray",arg = list(mean = popultaionMean,sd=(sqrt(popupationVar)/sqrt(n)) ))
g<- g +  labs(title="Distribution ",x="")
g

#4. Evaluate the coverage of the confidence interval for 1/lambda
(mean(sampleMean$x) + c(-1, 1) * qnorm(0.975) * sd(sampleMean$x)/sqrt(n))


