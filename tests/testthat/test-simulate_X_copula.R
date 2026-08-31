test_that("simulate_X_copula returns correct dimensions", {
  cp <- copula::normalCopula(param = 0.5, dim = 3, dispstr = "ex")
  result <- simulate_X_copula(
    n = 50, p = 3, cp = cp,
    margins = c("norm", "norm", "norm"),
    paramMargins = list(
      list(mean = 0, sd = 1),
      list(mean = 0, sd = 1),
      list(mean = 0, sd = 1)
    )
  )
  expect_s3_class(result, "data.frame")
  expect_identical(nrow(result), 50L)
  expect_identical(ncol(result), 3L)
})

test_that("simulate_X_copula names columns x1 through xp", {
  cp <- copula::normalCopula(param = 0.5, dim = 2, dispstr = "ex")
  result <- simulate_X_copula(
    n = 10, p = 2, cp = cp,
    margins = c("norm", "norm"),
    paramMargins = list(
      list(mean = 0, sd = 1),
      list(mean = 5, sd = 2)
    )
  )
  expect_named(result, c("x1", "x2"))
})

test_that("simulate_X_copula works with mixed marginal distributions", {
  cp <- copula::normalCopula(param = 0.8, dim = 4, dispstr = "ar1")
  result <- simulate_X_copula(
    n = 100, p = 4, cp = cp,
    margins = c("norm", "t", "norm", "binom"),
    paramMargins = list(
      list(mean = 2, sd = 3),
      list(df = 2),
      list(mean = 0, sd = 1),
      list(size = 10, prob = 0.5)
    )
  )
  expect_identical(nrow(result), 100L)
  expect_named(result, c("x1", "x2", "x3", "x4"))
})

test_that("simulate_X_copula respects marginal parameters", {
  cp <- copula::normalCopula(param = 0, dim = 2, dispstr = "ex")
  set.seed(42)
  result <- simulate_X_copula(
    n = 5000, p = 2, cp = cp,
    margins = c("norm", "norm"),
    paramMargins = list(
      list(mean = 100, sd = 1),
      list(mean = -50, sd = 0.5)
    )
  )
  expect_gt(mean(result$x1), 99)
  expect_lt(mean(result$x1), 101)
  expect_gt(mean(result$x2), -51)
  expect_lt(mean(result$x2), -49)
})

test_that("simulate_X_copula produces correlated data", {
  cp <- copula::normalCopula(param = 0.9, dim = 2, dispstr = "ex")
  set.seed(42)
  result <- simulate_X_copula(
    n = 5000, p = 2, cp = cp,
    margins = c("norm", "norm"),
    paramMargins = list(
      list(mean = 0, sd = 1),
      list(mean = 0, sd = 1)
    )
  )
  spearman <- cor(result$x1, result$x2, method = "spearman")
  expect_gt(spearman, 0.8)
})

test_that("simulate_X_copula works with n = 1", {
  cp <- copula::normalCopula(param = 0.5, dim = 2, dispstr = "ex")
  result <- simulate_X_copula(
    n = 1, p = 2, cp = cp,
    margins = c("norm", "norm"),
    paramMargins = list(
      list(mean = 0, sd = 1),
      list(mean = 0, sd = 1)
    )
  )
  expect_identical(nrow(result), 1L)
  expect_named(result, c("x1", "x2"))
})

test_that("simulate_X_copula errors when p mismatches copula dimension", {
  cp <- copula::normalCopula(param = 0.5, dim = 2, dispstr = "ex")
  expect_error(simulate_X_copula(
    n = 10, p = 3, cp = cp,
    margins = c("norm", "norm", "norm"),
    paramMargins = list(
      list(mean = 0, sd = 1),
      list(mean = 0, sd = 1),
      list(mean = 0, sd = 1)
    )
  ), "dimension")
})

test_that("simulate_X_copula validates n", {
  cp <- copula::normalCopula(param = 0.5, dim = 2, dispstr = "ex")
  expect_error(simulate_X_copula(
    n = 0, p = 2, cp = cp,
    margins = c("norm", "norm"),
    paramMargins = list(list(mean = 0, sd = 1), list(mean = 0, sd = 1))
  ))
  expect_error(simulate_X_copula(
    n = -1, p = 2, cp = cp,
    margins = c("norm", "norm"),
    paramMargins = list(list(mean = 0, sd = 1), list(mean = 0, sd = 1))
  ))
})

test_that("simulate_X_copula validates cp is a copula", {
  expect_error(simulate_X_copula(
    n = 10, p = 2, cp = "not_a_copula",
    margins = c("norm", "norm"),
    paramMargins = list(list(mean = 0, sd = 1), list(mean = 0, sd = 1))
  ))
})

test_that("simulate_X_copula validates margins length matches p", {
  cp <- copula::normalCopula(param = 0.5, dim = 2, dispstr = "ex")
  expect_error(simulate_X_copula(
    n = 10, p = 2, cp = cp,
    margins = c("norm"),
    paramMargins = list(list(mean = 0, sd = 1), list(mean = 0, sd = 1))
  ))
})

test_that("simulate_X_copula validates paramMargins length matches p", {
  cp <- copula::normalCopula(param = 0.5, dim = 2, dispstr = "ex")
  expect_error(simulate_X_copula(
    n = 10, p = 2, cp = cp,
    margins = c("norm", "norm"),
    paramMargins = list(list(mean = 0, sd = 1))
  ))
})
