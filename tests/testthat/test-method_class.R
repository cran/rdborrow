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

test_that("bootstrap_ci_type rejects studentized intervals", {
  expect_error(
    ec_ipw("S ~ x1", bootstrap = 100, bootstrap_ci_type = "stud")
  )
})
