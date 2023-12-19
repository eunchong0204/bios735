test_that("get the exact t-statistic vector", {
        set.seed(1)
        x <- matrix(rnorm(100*20), nrow=100)
        f <- gl(2, 10)

        expect_equal(getT(x, f),
                     unname(sapply(seq_len(nrow(x)), function(i) t.test(x[i,] ~ f, var.equal=TRUE)$statistic)))
})

test_that("simple errors for bad input", {
        set.seed(1)
        x <- matrix(rnorm(100*20), nrow=100)
        f <- gl(2, 10)

        expect_error(getT())
        expect_error(getT(x))
        expect_error(getT(f))

})

test_that("warning sign shows up", {
        set.seed(1)
        x <- matrix(rnorm(100*20), nrow=100)
        f2 <- gl(2, 8)

        expect_warning(getT(x, f2), "match the length")
})

test_that("error", {
        set.seed(1)
        x <- matrix(rnorm(100*20), nrow=100)
        f3 <- gl(4, 5)

        expect_error(getT(x, f3), "is not two")
})
