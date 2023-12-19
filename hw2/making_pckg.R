# library setting
library(usethis)
library(devtools)

# make R package
setwd("D:/Spring_2022_D/BIOS735/hw")
create_package("bios735", roxygen = TRUE)

# Load
load_all("./bios735")

# Documenting
document("./bios735")

# Check
?getT
set.seed(1)
getT(matrix(rnorm(20), nrow=2), gl(2, 5))
getT(data.frame(matrix(rnorm(20), nrow=2)), gl(2, 5))

# Test
library(testthat)

# make testthat
setwd("D:/Spring_2022_D/BIOS735/hw/bios735")
use_test("getT")

# test
setwd("D:/Spring_2022_D/BIOS735/hw")
load_all("bios735")
document("./bios735")
test_file("bios735/tests/testthat/test-getT.R")

setwd("D:/Spring_2022_D/BIOS735/hw/bios735")
check(manual=TRUE)






getT <- function(x, f){
        browser()
        # warning
        if (ncol(x) != length(f)){
                warning("The number of columns of the matrix doesn't match the length of the two level vector")
        }
        
        # error
        if (length(levels(f)) != 2){
                stop("The level of the level vector is not two")
        }
        
        # Spliting the data
        x1 <- x[,f == levels(f)[1]]
        x2 <- x[,f == levels(f)[2]]
        
        # Calculating mean difference and pooled sample variance
        mean_diff <- rowMeans(x1) - rowMeans(x2)
        
        #pooled_var <- (rowSums((x1 - rowMeans(x1))^2) + rowSums((x2 - rowMeans(x2))^2))/(ncol(x1) + ncol(x2) - 2)
        
        # error
        pooled_var <- (rowSums((x1 - rowMeans(x1))^2) + rowSums((x2 - rowMeans(x2))^2))/(ncol(x1) + ncol(x2) - 2) + "error"
        
        # Calculating t-statistic
        mean_diff/sqrt(pooled_var*(1/ncol(x1) + 1/ncol(x2)))
}

m <- 400
n <- 50
little.n <- n/2
set.seed(1)
x <- matrix(rnorm(m*n),nrow=m,ncol=n)
f <- gl(2,little.n)
getT(x,f)
