test_that("simulate_X_mixture works with continuous only", {
  result <- simulate_X_mixture(
    n = 50, p_cat = 0, p_cont = 3,
    cat_level_list = list(),
    cat_comb_prob = c(),
    cont_para_list = list(list(mean = c(0, 0, 0), sigma = diag(3)))
  )
  expect_s3_class(result, "data.frame")
  expect_identical(nrow(result), 50L)
  expect_named(result, c("x1", "x2", "x3"))
})

test_that("simulate_X_mixture works with categorical only", {
  set.seed(42)
  result <- simulate_X_mixture(
    n = 100, p_cat = 2, p_cont = 0,
    cat_level_list = list(c(0, 1), c(0, 1)),
    cat_comb_prob = c(0.25, 0.25, 0.25, 0.25),
    cont_para_list = list()
  )
  expect_identical(nrow(result), 100L)
  expect_named(result, c("x1", "x2"))
  expect_identical(sort(unique(result$x1)), c(0, 1))
  expect_identical(sort(unique(result$x2)), c(0, 1))
})

test_that("simulate_X_mixture works with mixed categorical and continuous", {
  set.seed(42)
  result <- simulate_X_mixture(
    n = 200, p_cat = 1, p_cont = 2,
    cat_level_list = list(c(0, 1)),
    cat_comb_prob = c(0.4, 0.6),
    cont_para_list = list(
      list(mean = c(0, 0), sigma = diag(2)),
      list(mean = c(5, 5), sigma = diag(2))
    )
  )
  expect_identical(nrow(result), 200L)
  expect_named(result, c("x1", "x2", "x3"))
  expect_identical(sort(unique(result$x1)), c(0, 1))
})

test_that("simulate_X_mixture handles multiple categorical levels", {
  set.seed(42)
  result <- simulate_X_mixture(
    n = 100, p_cat = 1, p_cont = 1,
    cat_level_list = list(c(0, 1, 2)),
    cat_comb_prob = c(0.2, 0.5, 0.3),
    cont_para_list = list(
      list(mean = 0, sigma = matrix(1)),
      list(mean = 1, sigma = matrix(1)),
      list(mean = 2, sigma = matrix(1))
    )
  )
  expect_identical(nrow(result), 100L)
  expect_identical(sort(unique(result$x1)), c(0, 1, 2))
})

test_that("simulate_X_mixture continuous covariates reflect component means", {
  set.seed(42)
  result <- simulate_X_mixture(
    n = 5000, p_cat = 1, p_cont = 1,
    cat_level_list = list(c(0, 1)),
    cat_comb_prob = c(0.5, 0.5),
    cont_para_list = list(
      list(mean = -10, sigma = matrix(1)),
      list(mean = 10, sigma = matrix(1))
    )
  )
  mean_grp0 <- mean(result$x2[result$x1 == 0])
  mean_grp1 <- mean(result$x2[result$x1 == 1])
  expect_lt(mean_grp0, -8)
  expect_gt(mean_grp1, 8)
})

test_that("simulate_X_mixture names columns correctly with many covariates", {
  result <- simulate_X_mixture(
    n = 10, p_cat = 2, p_cont = 3,
    cat_level_list = list(c(0, 1), c(0, 1)),
    cat_comb_prob = c(0.25, 0.25, 0.25, 0.25),
    cont_para_list = list(
      list(mean = c(0, 0, 0), sigma = diag(3)),
      list(mean = c(0, 0, 0), sigma = diag(3)),
      list(mean = c(0, 0, 0), sigma = diag(3)),
      list(mean = c(0, 0, 0), sigma = diag(3))
    )
  )
  expect_named(result, c("x1", "x2", "x3", "x4", "x5"))
})

test_that("simulate_X_mixture validates n", {
  expect_error(simulate_X_mixture(
    n = 0, p_cat = 0, p_cont = 2,
    cat_level_list = list(),
    cat_comb_prob = c(),
    cont_para_list = list(list(mean = c(0, 0), sigma = diag(2)))
  ))
})

test_that("simulate_X_mixture errors when both p_cat and p_cont are 0", {
  expect_error(simulate_X_mixture(
    n = 10, p_cat = 0, p_cont = 0,
    cat_level_list = list(),
    cat_comb_prob = c(),
    cont_para_list = list()
  ), "positive")
})

test_that("simulate_X_mixture validates cat_level_list length", {
  expect_error(simulate_X_mixture(
    n = 10, p_cat = 2, p_cont = 0,
    cat_level_list = list(c(0, 1)),
    cat_comb_prob = c(0.5, 0.5),
    cont_para_list = list()
  ))
})

test_that("simulate_X_mixture validates cat_comb_prob length", {
  expect_error(simulate_X_mixture(
    n = 10, p_cat = 1, p_cont = 1,
    cat_level_list = list(c(0, 1)),
    cat_comb_prob = c(0.5),
    cont_para_list = list(
      list(mean = 0, sigma = matrix(1)),
      list(mean = 0, sigma = matrix(1))
    )
  ))
})

test_that("simulate_X_mixture validates cont_para_list length", {
  expect_error(simulate_X_mixture(
    n = 10, p_cat = 1, p_cont = 1,
    cat_level_list = list(c(0, 1)),
    cat_comb_prob = c(0.5, 0.5),
    cont_para_list = list(
      list(mean = 0, sigma = matrix(1))
    )
  ))
})
