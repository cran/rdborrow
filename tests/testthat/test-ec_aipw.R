test_that("the ec_aipw() sandwich SE equals an independent c' Sigma c", {
  covs <- c("x1", "x2", "x3", "x4", "x5")
  outs <- c("y1", "y2")
  for (w in list(NULL, 0.3, 1)) {
    res <- run_analysis(setup_analysis_primary(
      SyntheticDataII, "S", "A", outs, covs,
      ec_aipw(
        "S ~ x1 + x2 + x3 + x4 + x5",
        paste(outs, "~ x1 + x2 + x3 + x4 + x5"),
        weight = w
      )
    ))
    ind <- independent_ec(
      SyntheticDataII, outs, covs,
      w = res$borrow_weight, augment = TRUE
    )
    expect_equal(res$results$point_estimates, ind$tau, tolerance = 1e-6)
    expect_equal(res$results$standard_deviation, ind$sd, tolerance = 1e-6)
  }
})

test_that("ec_aipw(weight = 0) matches an independent M-estimator", {
  covs <- c("x1", "x2", "x3", "x4", "x5")
  outs <- c("y1", "y2")
  res <- run_primary(
    SyntheticDataII,
    ec_aipw(
      "S ~ x1 + x2 + x3 + x4 + x5", paste(outs, "~ x1 + x2 + x3 + x4 + x5"),
      weight = 0
    )
  )
  ind <- independent_ec(SyntheticDataII, outs, covs, w = 0, augment = TRUE)
  expect_lt(ind$max_ee, 1e-8)
  expect_equal(res$results$point_estimates, ind$tau, tolerance = 1e-8)
  expect_equal(res$results$standard_deviation, ind$sd, tolerance = 1e-6)
})

test_that("ec_aipw() validates outcome_formula and bootstrap_ci_type", {
  ps <- "S ~ x1"
  of <- c("y1 ~ x1", "y2 ~ x1")
  expect_error(ec_aipw(ps, character(0)))
  expect_error(ec_aipw(ps, of, bootstrap = 50, bootstrap_ci_type = "stud"))
  for (type in c("perc", "bca", "norm", "basic")) {
    expect_s4_class(
      ec_aipw(ps, of, bootstrap = 50, bootstrap_ci_type = type),
      "ec_aipw_method"
    )
  }
  expect_identical(ec_aipw(ps, of, bootstrap = 50)@bootstrap_ci_type, "perc")
  expect_null(ec_aipw(ps, of)@bootstrap_ci_type)
})

test_that("collinear covariates in either ec_aipw() model are dropped", {
  d <- SyntheticDataII
  d$x6 <- 2 * d$x5
  fit <- function(ps, rhs) {
    method <- ec_aipw(ps, paste(c("y1", "y2"), "~", rhs))
    run_primary(d, method, covariates = c("x5", "x6"))
  }
  expect_equal(fit("S ~ x5 + x6", "x5 + x6"), fit("S ~ x5", "x5"))
})

test_that("the ec_aipw() SE uses the outcome model from the point estimate", {
  covs <- c("x1", "x2", "x3", "x4", "x5")
  d <- SyntheticDataII
  ctrl <- d$A == 0
  basis <- splines::ns(d$x5[ctrl], df = 3)
  b <- predict(basis, d$x5)
  d$b1 <- b[, 1]
  d$b2 <- b[, 2]
  d$b3 <- b[, 3]
  fit <- function(formula, covariates) {
    method <- ec_aipw("S ~ x1 + x2 + x3 + x4 + x5", formula, weight = 0.4)
    run_primary(d, method, outcomes = "y1", covariates = covariates)
  }
  expect_equal(
    fit("y1 ~ splines::ns(x5, df = 3)", covs),
    fit("y1 ~ b1 + b2 + b3", c(covs, "b1", "b2", "b3"))
  )
})
