#Rscript for hw3

#library
library(usethis)
library(devtools)
library(Rcpp)
library(RcppArmadillo)
library(microbenchmark)
library(MASS)

#Making template for rpackage using Rcpp
setwd("D:/Spring_2022_D/BIOS735/hw/bios735")
RcppArmadillo.package.skeleton()
# Added man, R, and src to the my package foler
##(better!!) But, it's okay to add only Makevars and Makevars.win files.
# Modified DESCRIPTION
# Didn't touch NAMESPACE yet.

# Modified DESCRIPTION
# Add .cpp file to src folder, then
load_all()

# now, make cpp file and wrapper R file.

## Q1
document()
load_all()

# check
x <- -10:10
one.or.exp(x)
all.equal(ifelse(x < 0.0,1.0,exp(x)), one.or.exp(x))


## Q2
#
document()
load_all()


# Code 1 from hw
niter <- 1e4
lambda <- .01
randomWalk1 <- function(niter,lambda) {
        x <- numeric(niter)
        y <- numeric(niter)
        for (i in seq_len(niter)[-1]) {
                x[i] <- x[i-1] + lambda * (2.0 * rbinom(1,1,0.5) - 1.0)
        }
        for (i in seq_len(niter)[-1]) {
                y[i] <- y[i-1] + lambda * (2.0 * rbinom(1,1,0.5) - 1.0)
        }
        list(x=x,y=y)
}
myplot <- function(dat) {
        niter <- length(dat$x)
        plot(0,type="n",xlim=c(-2,2),ylim=c(-2,2),xlab="x",ylab="y")
        cols <- colorRampPalette(c("blue","red","orange"))(100)
        with(dat, segments(x[-niter],y[-niter],x[-1],y[-1],col=rep(cols,each=niter/100)))
}
set.seed(5)
dat1 <- randomWalk1(niter,lambda)
str(dat1)
myplot(dat1)


# Code 2 from hw
setwd("D:/Spring_2022_D/BIOS735/hw/bios735")
document()
load_all()
set.seed(5)
dat2 <- randomWalk2(niter,lambda)
str(dat2)
myplot(dat2)
all.equal(dat2$x, dat1$x)
all.equal(dat2$y, dat1$y)
microbenchmark(randomWalk1(niter,lambda),randomWalk2(niter,lambda),times=10)


# Code 3 from hw
randomWalkVectorized <- function(niter,lambda) {
        x <- c(0, lambda * (2.0 * rbinom(niter-1, 1, 0.5) - 1.0))
        y <- c(0, lambda * (2.0 * rbinom(niter-1, 1, 0.5) - 1.0))
        list(x=cumsum(x), y=cumsum(y))
}
set.seed(5)
datVec <- randomWalkVectorized(niter,lambda)
str(datVec)
myplot(datVec)
all.equal(datVec$x, dat1$x)
all.equal(datVec$y, dat1$y)
library(microbenchmark)
microbenchmark(randomWalk1(niter,lambda),randomWalkVectorized(niter,lambda),times=10)
microbenchmark(randomWalk2(niter,lambda),randomWalkVectorized(niter,lambda),times=10)


#
## Q3
document()
load_all()
A <- matrix(runif(12),ncol=3)
x <- matrix(runif(3),ncol=1)
b <- A %*% x
xx <- armadilloSolve(A,b)
all.equal(x, xx)


#
## Q4
document()
load_all()
set.seed(1)
n <- 100
Y <- matrix(rnorm(n*20),nrow=20)
X <- scale(matrix(rnorm(20*2),ncol=2))
lambda <- runif(n,.1,2)
library(MASS)
colRidge1 <- function(Y, X, lambda) {
        df <- as.data.frame(X)
        n <- ncol(Y)
        beta <- matrix(nrow=2,ncol=n)
        stopifnot(length(lambda) == n)
        for (j in seq_len(n)) {
                beta[,j] <- coef(lm.ridge(Y[,j] ~ 0 + V1 + V2, data=df, lambda=lambda[j]))
        }
        beta
}
beta1 <- colRidge1(Y, X, lambda)
beta1[,1:5]
beta2 <- colRidge2(Y, X, lambda)
beta2[,1:5]
plot(beta1[1,], beta2[1,])
abline(0,1)
plot(beta1[2,], beta2[2,])
abline(0,1)
all.equal(beta1[1,], beta2[1,])
all.equal(beta1[2,], beta2[2,])
microbenchmark(colRidge1(Y, X, lambda), colRidge2(Y, X, lambda), times=10)
