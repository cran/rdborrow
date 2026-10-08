test_that("setup_method returns default object", {
  obj <- setup_method()
  expect_s4_class(obj, "method_obj")
  expect_identical(obj@method_name, "")
})

test_that("setup_method accepts valid arguments", {
  obj <- setup_method(method_name = "AIPW")
  expect_identical(obj@method_name, "AIPW")
})

test_that("setup_method validates method_name", {
  expect_error(setup_method(method_name = 123))
  expect_error(setup_method(method_name = NA))
  expect_error(setup_method(method_name = c("a", "b")))
})

test_that("all bootstrap CI types return finite bounds", {
  covs <- c("x1", "x2", "x3", "x4", "x5")
  ps <- "S ~ x1 + x2 + x3 + x4 + x5"

  for (ci_type in c("perc", "norm", "basic", "bca")) {
    method <- ec_ipw(ps, bootstrap = 100, bootstrap_ci_type = ci_type)
    analysis <- setup_analysis_primary(
      data = SyntheticData,
      trial_status_col_name = "S",
      treatment_col_name = "A",
      outcome_col_name = c("y1", "y2"),
      covariates_col_name = covs,
      method_weighting_obj = method
    )

    set.seed(42)
    res <- run_analysis(analysis)$results

    expect_all_true(is.finite(res$lower_CI_boot))
    expect_all_true(is.finite(res$upper_CI_boot))
    expect_all_true(res$lower_CI_boot < res$upper_CI_boot)
  }
})

test_that("constructors reject fewer than two bootstrap replicates", {
  f <- "y1 ~ x1"
  constructors <- list(
    \(b) ec_ipw("S ~ x1", bootstrap = b),
    \(b) ec_aipw("S ~ x1", outcome_formula = f, bootstrap = b),
    \(b) did_ec_ipw("S ~ x1", bootstrap = b),
    \(b) did_ec_aipw("S ~ x1", outcome_formula = f, bootstrap = b),
    \(b) did_ec_or(f, f, f, bootstrap = b),
    \(b) scm(bootstrap = b)
  )

  for (make in constructors) {
    expect_error(make(1), "bootstrap")
    expect_s4_class(make(2), "method_obj")
  }
})

test_that("bootstrap_ci_type rejects studentized intervals", {
  expect_error(
    ec_ipw("S ~ x1", bootstrap = 100, bootstrap_ci_type = "stud")
  )
})

test_that("bootstrap_ci_type without bootstrap is an error", {
  expect_error(
    ec_ipw("S ~ x1", bootstrap_ci_type = "bca"),
    "bootstrap_ci_type.*needs.*bootstrap"
  )
  expect_error(
    ec_aipw("S ~ x1", "y1 ~ x1", bootstrap_ci_type = "norm"),
    "bootstrap_ci_type.*needs.*bootstrap"
  )
  expect_null(ec_ipw("S ~ x1")@bootstrap_ci_type)
  expect_identical(ec_ipw("S ~ x1", bootstrap = 2)@bootstrap_ci_type, "perc")
})

test_that(".match_outcome_formulas orders formulas by their left-hand side", {
  outs <- c("y1", "y2", "y3")
  f <- c("y3 ~ x1", "y1 ~ x1", "y2 ~ x2")
  expect_identical(
    .match_outcome_formulas(f, outs, "outcome_formula"),
    c("y1 ~ x1", "y2 ~ x2", "y3 ~ x1")
  )
  expect_identical(
    .match_outcome_formulas("`week 12` ~ x1", "week 12", "f"),
    "`week 12` ~ x1"
  )
  expect_error(
    .match_outcome_formulas(c("y1 ~ x1", "z ~ x1", "y3 ~ x1"), outs, "f"),
    "f.*z"
  )
  expect_error(
    .match_outcome_formulas(c("y1 ~ x1", "y1 ~ x2", "y3 ~ x1"), outs, "f"),
    "f.*y1"
  )
  expect_error(
    .match_outcome_formulas(c("y1 ~ x1", "y2 ~ x1"), outs, "f"),
    "f.*y3"
  )
})

test_that(".match_outcome_formulas rejects transformed and missing outcomes", {
  outs <- c("y1", "y2")
  expect_error(
    .match_outcome_formulas(c("y1 ~ x1", "log(y2) ~ x1"), outs, "f"),
    "f.*outcome name.*log\\(y2\\)"
  )
  expect_error(
    .match_outcome_formulas(c("I(2 * y1) ~ x1", "y2 ~ x1"), outs, "f"),
    "outcome name"
  )
  expect_error(
    .match_outcome_formulas(c("~ x1", "y2 ~ x1"), outs, "f"),
    "f.*left-hand side"
  )
  expect_error(
    .match_outcome_formulas(c("~ y1", "y2 ~ x1"), outs, "f"),
    "f.*left-hand side"
  )
})

test_that("bootstrap intervals of both primary methods use alpha", {
  ps <- "S ~ x1 + x2 + x3 + x4 + x5"
  of <- paste(c("y1", "y2"), "~ x1 + x2 + x3 + x4 + x5")
  for (method in list(ec_ipw(ps, bootstrap = 50), ec_aipw(ps, of, bootstrap = 50))) {
    set.seed(1)
    wide <- run_primary(SyntheticDataII, method, alpha = 0.05)$results
    set.seed(1)
    narrow <- run_primary(SyntheticDataII, method, alpha = 0.5)$results
    expect_all_true(narrow$lower_CI_boot > wide$lower_CI_boot)
    expect_all_true(narrow$upper_CI_boot < wide$upper_CI_boot)
  }
})

test_that("basic and norm bootstrap intervals have their defining relations", {
  boot_ci <- function(type) {
    set.seed(7)
    run_primary(
      SyntheticDataII,
      ec_ipw("S ~ x1 + x2 + x3 + x4 + x5", bootstrap = 50, bootstrap_ci_type = type)
    )$results
  }
  perc <- boot_ci("perc")
  basic <- boot_ci("basic")
  norm <- boot_ci("norm")
  tau <- perc$point_estimates
  expect_equal(basic$lower_CI_boot, 2 * tau - perc$upper_CI_boot)
  expect_equal(basic$upper_CI_boot, 2 * tau - perc$lower_CI_boot)
  expect_equal(
    norm$upper_CI_boot - norm$lower_CI_boot,
    2 * qnorm(0.975) * norm$standard_deviation
  )
})

test_that(".run_bootstrap() norm interval is the bias-corrected normal interval", {
  d <- SyntheticDataII
  stat <- function(data, indices) mean(data$y1[indices])
  set.seed(11)
  out <- .run_bootstrap(d, stat,
    n_estimates = 1, bootstrap = 200,
    bootstrap_ci_type = "norm", alpha = 0.1
  )
  set.seed(11)
  b <- boot::boot(d, stat, R = 200, strata = as.integer(interaction(d$S, d$A, drop = TRUE)))
  centre <- 2 * b$t0 - mean(b$t)
  half <- qnorm(0.95) * sd(b$t)
  expect_equal(c(out$lower_ci, out$upper_ci), centre + c(-half, half))
  expect_equal(out$sd_boot, sd(b$t))
})

test_that(".run_bootstrap() keeps the S x A group sizes in every replicate", {
  stat <- function(data, indices) {
    c(mean(data$y1[indices]), sum(data$S[indices]), sum(data$A[indices]))
  }
  set.seed(3)
  out <- suppressWarnings(.run_bootstrap(SyntheticDataII, stat,
    n_estimates = 1, bootstrap = 30,
    bootstrap_ci_type = "perc", alpha = 0.05
  ))
  expect_equal(out$sd_boot[2:3], c(0, 0))
})

test_that(".run_bootstrap returns boot.ci intervals at level 1 - alpha", {
  df <- make_ole_data(n_time = 3)
  stat <- function(data, indices) mean(data$y3[indices])
  set.seed(7)
  res <- .run_bootstrap(df, stat, 1, 99, "perc", alpha = 0.2)
  set.seed(7)
  b <- boot::boot(df, stat, R = 99, strata = as.integer(interaction(df$S, df$A, drop = TRUE)))
  ci <- boot::boot.ci(b, conf = 0.8, type = "perc")$percent[4:5]
  expect_equal(c(res$lower_ci, res$upper_ci), ci)
})

test_that("OLE constructors accept every supported CI type and reject others", {
  f <- "y1 ~ x1"
  constructors <- list(
    \(type) did_ec_ipw("S ~ x1", bootstrap_ci_type = type),
    \(type) did_ec_aipw("S ~ x1", outcome_formula = f, bootstrap_ci_type = type),
    \(type) did_ec_or(f, f, f, bootstrap_ci_type = type),
    \(type) scm(bootstrap_ci_type = type)
  )
  for (make in constructors) {
    for (type in c("perc", "bca", "norm", "basic")) {
      expect_identical(make(type)@bootstrap_ci_type, type)
    }
    expect_error(make("stud"))
  }
})
