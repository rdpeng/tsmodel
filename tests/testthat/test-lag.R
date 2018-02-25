context("Lag")

test_that("Lag with Sequential Groups", {
        x <- rep(1:5, 2)
        f <- gl(2, 5)
        expect_equal(Lag(x, 1, f), c(NA, 1, 2, 3, 4, NA, 1, 2, 3, 4))
        expect_equal(Lag(x, -1, f), c(2, 3, 4, 5, NA, 2, 3, 4, 5, NA))
        expect_equal(Lag(x, 0, f), x)
})


test_that("Lag with Grouped Groups", {
        x <- rep(1:5, each = 2)
        f <- gl(5, 2)
        
        expect_equal(Lag(x, 1, f), c(NA, 1, NA, 2, NA, 3, NA, 4, NA, 5))
        expect_equal(Lag(x, -1, f), c(1, NA, 2, NA, 3, NA, 4, NA, 5, NA))
        expect_equal(Lag(x, 0, f), x)
})






