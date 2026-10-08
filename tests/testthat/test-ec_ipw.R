test_that("the ec_ipw() sandwich SE equals an independent c' Sigma c", {
  covs <- c("x1", "x2", "x3", "x4", "x5")
  outs <- c("y1", "y2")
  for (w in list(NULL, 0.3, 1)) {
    res <- run_analysis(setup_analysis_primary(
      SyntheticDataII, "S", "A", outs, covs,
      ec_ipw("S ~ x1 + x2 + x3 + x4 + x5", weight = w)
    ))
    ind <- independent_ec(SyntheticDataII, outs, covs, w = res$borrow_weight)
    expect_equal(res$results$point_estimates, ind$tau, tolerance = 1e-6)
    expect_equal(res$results$standard_deviation, ind$sd, tolerance = 1e-6)
  }
})

test_that("ec_ipw(weight = 0) SE is the closed-form Hajek variance", {
  d <- SyntheticDataII
  res <- run_primary(d, ec_ipw("S ~ x1 + x2 + x3 + x4 + x5", weight = 0))
  trt <- d[d$S == 1 & d$A == 1, c("y1", "y2")]
  ctl <- d[d$S == 1 & d$A == 0, c("y1", "y2")]
  ml_var <- function(y) colMeans(sweep(y, 2, colMeans(y))^2)
  expected <- sqrt(ml_var(trt) / nrow(trt) + ml_var(ctl) / nrow(ctl))
  expect_equal(res$results$standard_deviation, unname(expected), tolerance = 1e-8)
})

test_that("ec_ipw(weight = 0) does not use the external outcomes", {
  shifted <- SyntheticData
  shifted[shifted$S == 0, c("y1", "y2")] <- shifted[shifted$S == 0, c("y1", "y2")] + 1000
  method <- ec_ipw("S ~ x1 + x2 + x3 + x4 + x5", weight = 0)
  expect_equal(run_primary(shifted, method), run_primary(SyntheticData, method))
})

test_that("the optimal weight matches Eq 11 on unbalanced arms", {
  covs <- c("x1", "x2", "x3", "x4", "x5")
  ps <- "S ~ x1 + x2 + x3 + x4 + x5"
  expected <- independent_opt_weight(SyntheticDataII, covs)
  expect_equal(
    run_primary(SyntheticDataII, ec_ipw(ps))$borrow_weight, expected,
    tolerance = 1e-8
  )
  expect_equal(
    run_primary(
      SyntheticDataII,
      ec_aipw(ps, paste(c("y1", "y2"), "~ x1 + x2 + x3 + x4 + x5"))
    )$borrow_weight,
    expected,
    tolerance = 1e-8
  )
})

test_that("the normal intervals of both primary methods use alpha", {
  ps <- "S ~ x1 + x2 + x3 + x4 + x5"
  of <- paste(c("y1", "y2"), "~ x1 + x2 + x3 + x4 + x5")
  for (method in list(ec_ipw(ps), ec_aipw(ps, of))) {
    res <- run_primary(SyntheticDataII, method, alpha = 0.2)$results
    half <- qnorm(0.9) * res$standard_deviation
    expect_equal(res$upper_CI_normal - res$point_estimates, half)
    expect_equal(res$point_estimates - res$lower_CI_normal, half)
  }
})

test_that("a single outcome gives the first row of a two-outcome fit", {
  ps <- "S ~ x1 + x2 + x3 + x4 + x5"
  of <- paste(c("y1", "y2"), "~ x1 + x2 + x3 + x4 + x5")
  pairs <- list(
    list(ec_ipw(ps, weight = 0), ec_ipw(ps, weight = 0)),
    list(ec_ipw(ps), ec_ipw(ps)),
    list(ec_aipw(ps, of[1]), ec_aipw(ps, of))
  )
  for (m in pairs) {
    one <- run_primary(SyntheticDataII, m[[1]], outcomes = "y1")
    two <- run_primary(SyntheticDataII, m[[2]])
    expect_equal(one$results, two$results[1, ])
  }
})

test_that("covariate columns that no formula uses do not change the fit", {
  method <- ec_ipw("S ~ x1")
  expect_equal(
    run_primary(SyntheticDataII, method, covariates = "x1"),
    run_primary(SyntheticDataII, method, covariates = c("x1", "x2"))
  )
})

test_that("result rows are named tau1 to tauT for every inference path", {
  ps <- "S ~ x1 + x2 + x3 + x4 + x5"
  of <- paste(c("y1", "y2"), "~ x1 + x2 + x3 + x4 + x5")
  methods <- list(
    ec_ipw(ps), ec_ipw(ps, bootstrap = 2),
    ec_aipw(ps, of), ec_aipw(ps, of, bootstrap = 2)
  )
  for (method in methods) {
    set.seed(1)
    res <- suppressWarnings(run_primary(SyntheticData, method))
    expect_identical(rownames(res$results), c("tau1", "tau2"))
  }
})

test_that("the sandwich variance does not build an N x N matrix", {
  set.seed(1)
  n <- 6000
  x <- rnorm(n)
  s <- rbinom(n, 1, plogis(0.5 * x))
  a <- ifelse(s == 1, rbinom(n, 1, 0.5), 0)
  d <- data.frame(S = s, A = a, x1 = x, y1 = x + a + rnorm(n))
  extra_mb <- function(method) {
    analysis <- setup_analysis_primary(d, "S", "A", "y1", "x1", method)
    invisible(gc(reset = TRUE))
    before <- sum(gc()[, 2])
    run_analysis(analysis)
    m <- gc()
    sum(m[, which(colnames(m) == "max used") + 1]) - before
  }
  expect_lt(extra_mb(ec_ipw("S ~ x1")), 100)
  expect_lt(extra_mb(ec_aipw("S ~ x1", "y1 ~ x1")), 100)
})

test_that("a collinear or constant covariate is dropped, as glm() drops it", {
  d <- SyntheticDataII
  d$x6 <- 2 * d$x5
  d$x7 <- 1
  fit <- function(ps) {
    run_primary(d, ec_ipw(ps), covariates = c("x5", "x6", "x7"))
  }
  expect_equal(fit("S ~ x5 + x6"), fit("S ~ x5"))
  expect_equal(fit("S ~ x5 + x7"), fit("S ~ x5"))
})
