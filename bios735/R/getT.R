#' Vectorized t-Test
#'
#' \code{getT} takes a matrix and a vector which assigns levels to the columns
#' of the matrix and executes vectorized t-tests by rows
#' allowing a vector of t-statistics to be returned.
#'
#' \code{f} must have two levels.
#'
#'
#' @param x a matrix of numbers
#' @param f a vector of two factor levels
#'
#' @return a vector of t-statistics executed by rows of \code{x}.
#'
#' @examples
#' set.seed(1)
#' getT(matrix(rnorm(20), nrow=2), gl(2, 5))
#'
#' @export
getT <- function(x, f){
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
        pooled_var <- (rowSums((x1 - rowMeans(x1))^2) + rowSums((x2 - rowMeans(x2))^2))/(ncol(x1) + ncol(x2) - 2)

        # Calculating t-statistic
        mean_diff/sqrt(pooled_var*(1/ncol(x1) + 1/ncol(x2)))
}
