test_that("run_analysis dispatches ec_ipw correctly", {
  method <- ec_ipw(ps_formula = "S ~ x1 + x2 + x3 + x4 + x5")
  analysis <- setup_analysis_primary(
    data = SyntheticData,
    trial_status_col_name = "S",
    treatment_col_name = "A",
    outcome_col_name = c("y1", "y2"),
    covariates_col_name = c("x1", "x2", "x3", "x4", "x5"),
    method_weighting_obj = method
  )
  res <- run_analysis(analysis)

  expect_type(res, "list")
  expect_named(res, c("results", "borrow_weight"))
  expect_s3_class(res$results, "data.frame")
  expect_equal(nrow(res$results), 2)
  expect_true(all(c("point_estimates", "standard_deviation") %in% names(res$results)))
})

test_that("run_analysis dispatches ec_aipw correctly", {
  method <- ec_aipw(
    ps_formula = "S ~ x1 + x2 + x3 + x4 + x5",
    outcome_formula = c("y1 ~ x1 + x2 + x3 + x4 + x5", "y2 ~ x1 + x2 + x3 + x4 + x5")
  )
  analysis <- setup_analysis_primary(
    data = SyntheticData,
    trial_status_col_name = "S",
    treatment_col_name = "A",
    outcome_col_name = c("y1", "y2"),
    covariates_col_name = c("x1", "x2", "x3", "x4", "x5"),
    method_weighting_obj = method
  )
  res <- run_analysis(analysis)

  expect_type(res, "list")
  expect_named(res, c("results", "borrow_weight"))
  expect_equal(nrow(res$results), 2)
})

test_that("run_analysis dispatches did_ec_ipw correctly", {
  method <- did_ec_ipw(
    ps_formula = "S ~ x1 + x2 + x3 + x4 + x5",
    trt_formula = "A ~ x1 + x2 + x3 + x4 + x5",
    bootstrap = 50
  )
  analysis <- setup_analysis_OLE(
    data = SyntheticData,
    trial_status_col_name = "S",
    treatment_col_name = "A",
    outcome_col_name = c("y1", "y2", "y3", "y4"),
    covariates_col_name = c("x1", "x2", "x3", "x4", "x5"),
    method_OLE_obj = method,
    T_cross = 2
  )
  res <- run_analysis(analysis)

  expect_s3_class(res, "data.frame")
  expect_true("point_estimates" %in% names(res))
  expect_equal(nrow(res), 2)
})

test_that("run_analysis dispatches did_ec_aipw correctly", {
  method <- did_ec_aipw(
    ps_formula = "S ~ x1 + x2 + x3 + x4 + x5",
    trt_formula = "A ~ x1 + x2 + x3 + x4 + x5",
    outcome_formula = c(
      "y1 ~ x1 + x2 + x3 + x4 + x5",
      "y2 ~ x1 + x2 + x3 + x4 + x5",
      "y3 ~ x1 + x2 + x3 + x4 + x5",
      "y4 ~ x1 + x2 + x3 + x4 + x5"
    ),
    bootstrap = 50
  )
  analysis <- setup_analysis_OLE(
    data = SyntheticData,
    trial_status_col_name = "S",
    treatment_col_name = "A",
    outcome_col_name = c("y1", "y2", "y3", "y4"),
    covariates_col_name = c("x1", "x2", "x3", "x4", "x5"),
    method_OLE_obj = method,
    T_cross = 2
  )
  res <- run_analysis(analysis)

  expect_s3_class(res, "data.frame")
  expect_true("point_estimates" %in% names(res))
})

test_that("run_analysis dispatches did_ec_or correctly", {
  method <- did_ec_or(
    outcome_formula_ext = c(
      "y1 ~ x1 + x2 + x3 + x4 + x5",
      "y2 ~ x1 + x2 + x3 + x4 + x5",
      "y3 ~ x1 + x2 + x3 + x4 + x5",
      "y4 ~ x1 + x2 + x3 + x4 + x5"
    ),
    outcome_formula_rct_ctrl = c(
      "y1 ~ x1 + x2 + x3 + x4 + x5",
      "y2 ~ x1 + x2 + x3 + x4 + x5",
      "y3 ~ x1 + x2 + x3 + x4 + x5",
      "y4 ~ x1 + x2 + x3 + x4 + x5"
    ),
    outcome_formula_rct_trt = c(
      "y1 ~ x1 + x2 + x3 + x4 + x5",
      "y2 ~ x1 + x2 + x3 + x4 + x5",
      "y3 ~ x1 + x2 + x3 + x4 + x5",
      "y4 ~ x1 + x2 + x3 + x4 + x5"
    ),
    bootstrap = 50
  )
  analysis <- setup_analysis_OLE(
    data = SyntheticData,
    trial_status_col_name = "S",
    treatment_col_name = "A",
    outcome_col_name = c("y1", "y2", "y3", "y4"),
    covariates_col_name = c("x1", "x2", "x3", "x4", "x5"),
    method_OLE_obj = method,
    T_cross = 2
  )
  res <- suppressWarnings(run_analysis(analysis))

  expect_s3_class(res, "data.frame")
  expect_true("point_estimates" %in% names(res))
})

test_that("run_analysis dispatches scm correctly", {
  skip_on_cran()
  skip_if_not_installed("ECOSolveR")

  method <- scm(bootstrap = 2)
  analysis <- setup_analysis_OLE(
    data = SyntheticData,
    trial_status_col_name = "S",
    treatment_col_name = "A",
    outcome_col_name = c("y1", "y2", "y3", "y4"),
    covariates_col_name = c("x1", "x2", "x3", "x4", "x5"),
    method_OLE_obj = method,
    T_cross = 2
  )
  res <- suppressWarnings(run_analysis(analysis))

  expect_s3_class(res, "data.frame")
  expect_true("point_estimates" %in% names(res))
})

test_that("outcome formulas are matched to outcomes by name, not position", {
  covs <- c("x1", "x2", "x3", "x4", "x5")
  outs <- c("y1", "y2", "y3", "y4")
  of <- paste(outs, "~ x1 + x2 + x3 + x4 + x5")
  ps <- "S ~ x1 + x2 + x3 + x4 + x5"

  fit_primary <- function(f) {
    analysis <- setup_analysis_primary(
      SyntheticData, "S", "A", c("y1", "y2"), covs, ec_aipw(ps, f)
    )
    run_analysis(analysis)$results
  }
  fit_ole <- function(method) {
    analysis <- setup_analysis_OLE(
      SyntheticData, "S", "A", outs, covs, method,
      T_cross = 2
    )
    set.seed(1)
    suppressWarnings(run_analysis(analysis))
  }

  expect_equal(fit_primary(rev(of[1:2])), fit_primary(of[1:2]))
  expect_equal(
    fit_ole(did_ec_aipw(ps, outcome_formula = rev(of), bootstrap = 2)),
    fit_ole(did_ec_aipw(ps, outcome_formula = of, bootstrap = 2))
  )
  f_ext <- paste(outs, "~ x1")
  f_ctrl <- paste(outs, "~ x2")
  f_trt <- paste(outs, "~ x1 + x2")
  expect_equal(
    fit_ole(did_ec_or(
      rev(f_ext), f_ctrl[c(2, 1, 4, 3)], f_trt[c(4, 1, 2, 3)],
      bootstrap = 2
    )),
    fit_ole(did_ec_or(f_ext, f_ctrl, f_trt, bootstrap = 2))
  )

  set.seed(1)
  boot_rev <- suppressWarnings(run_analysis(setup_analysis_primary(
    SyntheticData, "S", "A", c("y1", "y2"), covs,
    ec_aipw(ps, rev(of[1:2]), bootstrap = 20)
  )))
  set.seed(1)
  boot_ord <- suppressWarnings(run_analysis(setup_analysis_primary(
    SyntheticData, "S", "A", c("y1", "y2"), covs,
    ec_aipw(ps, of[1:2], bootstrap = 20)
  )))
  expect_equal(boot_rev, boot_ord)
})

test_that("estimate() validates and rounds T_cross for every OLE method", {
  outs <- c("y1", "y2", "y3", "y4")
  covs <- c("x1", "x2", "x3", "x4", "x5")
  of <- paste(outs, "~ x1 + x2")
  small <- rbind(
    SyntheticData[SyntheticData$S == 1 & SyntheticData$A == 1, ][1:10, ],
    SyntheticData[SyntheticData$S == 1 & SyntheticData$A == 0, ][1:10, ],
    SyntheticData[SyntheticData$S == 0, ][1:15, ]
  )
  methods <- list(
    did_ec_ipw("S ~ x1 + x2", bootstrap = 2),
    did_ec_aipw("S ~ x1 + x2", outcome_formula = of, bootstrap = 2),
    did_ec_or(of, of, of, bootstrap = 2),
    scm(bootstrap = 2)
  )
  direct <- function(method, T_cross) {
    set.seed(1)
    suppressWarnings(estimate(
      method,
      data = small, outcomes = outs, treatment = "A", trial_status = "S",
      covariates = covs, T_cross = T_cross
    ))
  }
  for (m in methods) {
    expect_equal(direct(m, 0.6 / 0.2), direct(m, 3))
    expect_error(direct(m, 0), "T_cross")
    expect_error(direct(m, 4), "T_cross")
  }
})

test_that("OLE methods return the bootstrap standard deviation", {
  outs <- c("y1", "y2", "y3", "y4")
  of <- paste(outs, "~ x1 + x2")
  small <- rbind(
    SyntheticData[SyntheticData$S == 1 & SyntheticData$A == 1, ][1:10, ],
    SyntheticData[SyntheticData$S == 1 & SyntheticData$A == 0, ][1:10, ],
    SyntheticData[SyntheticData$S == 0, ][1:15, ]
  )
  methods <- list(
    did_ec_ipw("S ~ x1 + x2", bootstrap = 5),
    did_ec_aipw("S ~ x1 + x2", outcome_formula = of, bootstrap = 5),
    did_ec_or(of, of, of, bootstrap = 5),
    scm(bootstrap = 5)
  )
  for (m in methods) {
    set.seed(1)
    res <- suppressWarnings(run_analysis(setup_analysis_OLE(
      small, "S", "A", outs, c("x1", "x2"), m,
      T_cross = 2
    )))
    expect_named(
      res,
      c("point_estimates", "standard_deviation", "lower_CI_boot", "upper_CI_boot")
    )
    expect_all_true(res$standard_deviation > 0)
  }
})

test_that("estimate() rejects alpha of 0 or 1 for every method", {
  outs <- c("y1", "y2", "y3", "y4")
  of <- paste(outs, "~ x1")
  methods <- list(
    ec_ipw("S ~ x1"),
    ec_aipw("S ~ x1", outcome_formula = of),
    did_ec_ipw("S ~ x1", bootstrap = 2),
    did_ec_aipw("S ~ x1", outcome_formula = of, bootstrap = 2),
    did_ec_or(of, of, of, bootstrap = 2),
    scm(bootstrap = 2)
  )
  for (m in methods) {
    ole <- if (is(m, "method_OLE_obj")) list(T_cross = 2)
    for (a in c(0, 1)) {
      args <- c(list(
        m,
        data = SyntheticData, outcomes = outs, treatment = "A",
        trial_status = "S", covariates = "x1", alpha = a
      ), ole)
      expect_error(do.call(estimate, args), "alpha.*between 0 and 1")
    }
  }
})

test_that("trial-status and treatment columns can have any name", {
  covs <- c("x1", "x2", "x3", "x4", "x5")
  outs <- c("y1", "y2", "y3", "y4")
  of <- paste(outs, "~ x1 + x2 + x3 + x4 + x5")
  renamed <- SyntheticData
  names(renamed)[match(c("S", "A"), names(renamed))] <- c("in_trial", "arm")

  fit_primary <- function(data, trial, trt, method) {
    analysis <- setup_analysis_primary(
      data, trial, trt, c("y1", "y2"), covs, method
    )
    run_analysis(analysis)$results$point_estimates
  }
  fit_ole <- function(data, trial, trt, method) {
    analysis <- setup_analysis_OLE(
      data, trial, trt, outs, covs, method,
      T_cross = 2
    )
    set.seed(1)
    run_analysis(analysis)$point_estimates
  }

  primary <- list(
    ec_ipw("S ~ x1 + x2 + x3 + x4 + x5"),
    ec_aipw("S ~ x1 + x2 + x3 + x4 + x5", of[1:2])
  )
  ole <- list(
    did_ec_ipw("S ~ x1 + x2", trt_formula = "A ~ x1", bootstrap = 2),
    did_ec_aipw("S ~ x1 + x2", "A ~ x1", of, bootstrap = 2)
  )

  for (m in primary) {
    expect_equal(
      fit_primary(renamed, "in_trial", "arm", m),
      fit_primary(SyntheticData, "S", "A", m)
    )
  }
  for (m in ole) {
    expect_equal(
      suppressWarnings(fit_ole(renamed, "in_trial", "arm", m)),
      suppressWarnings(fit_ole(SyntheticData, "S", "A", m))
    )
  }
})

test_that("formulas use only covariates, and outcomes as predictors warn", {
  outs <- c("y1", "y2", "y3", "y4")
  covs <- c("x1", "x2", "x3", "x4", "x5")
  of <- paste(outs, "~ x1 + x2")
  primary <- function(method) {
    run_analysis(setup_analysis_primary(
      SyntheticData, "S", "A", c("y1", "y2"), covs, method
    ))
  }
  ole <- function(method) {
    set.seed(1)
    suppressWarnings(run_analysis(setup_analysis_OLE(
      SyntheticData, "S", "A", outs, covs, method,
      T_cross = 2
    )))
  }
  assign("z_session_only", rnorm(nrow(SyntheticData)), envir = globalenv())
  on.exit(rm("z_session_only", envir = globalenv()), add = TRUE)

  expect_error(primary(ec_ipw("S ~ .")), "ps_formula.*`\\.`")
  expect_error(
    ole(did_ec_or(of, of, paste(outs, "~ ."), bootstrap = 2)),
    "outcome_formula_rct_trt.*`\\.`"
  )
  expect_warning(
    res <- primary(ec_aipw("S ~ x1", c("y1 ~ x1", "y2 ~ x1 + y1"))),
    "outcome_formula.*outcomes as predictors: y1.*bias"
  )
  expect_all_true(is.finite(res$results$point_estimates))
  expect_error(
    primary(ec_ipw("S ~ x1 + z_session_only")),
    "ps_formula.*not covariates: z_session_only"
  )
  expect_error(
    ole(did_ec_ipw("S ~ x1", "A ~ x1 + z_session_only", bootstrap = 2)),
    "trt_formula.*not covariates: z_session_only"
  )
})

test_that("run_analysis validates its arguments", {
  analysis <- setup_analysis_primary(
    SyntheticData, "S", "A", "y1", "x1", ec_ipw("S ~ x1")
  )
  expect_error(run_analysis("not an analysis"), "analysis_obj")
  expect_error(run_analysis(analysis, quiet = NA), "quiet")
})

test_that("run_analysis quiet argument suppresses output", {
  method <- ec_ipw(ps_formula = "S ~ x1 + x2 + x3 + x4 + x5")
  analysis <- setup_analysis_primary(
    data = SyntheticData,
    trial_status_col_name = "S",
    treatment_col_name = "A",
    outcome_col_name = c("y1", "y2"),
    covariates_col_name = c("x1", "x2", "x3", "x4", "x5"),
    method_weighting_obj = method
  )

  expect_silent(run_analysis(analysis, quiet = TRUE))
  expect_message(run_analysis(analysis, quiet = FALSE), "Running EC-IPW")
  expect_silent(suppressMessages(run_analysis(analysis, quiet = FALSE)))
})

test_that("run_analysis works with single outcome", {
  method <- ec_ipw(ps_formula = "S ~ x1 + x2 + x3 + x4 + x5")
  analysis <- setup_analysis_primary(
    data = SyntheticData,
    trial_status_col_name = "S",
    treatment_col_name = "A",
    outcome_col_name = "y1",
    covariates_col_name = c("x1", "x2", "x3", "x4", "x5"),
    method_weighting_obj = method
  )
  res <- run_analysis(analysis)

  expect_equal(nrow(res$results), 1)
})

test_that("run_analysis respects alpha parameter", {
  method <- ec_ipw(ps_formula = "S ~ x1 + x2 + x3 + x4 + x5")
  analysis_wide <- setup_analysis_primary(
    data = SyntheticData,
    trial_status_col_name = "S",
    treatment_col_name = "A",
    outcome_col_name = c("y1", "y2"),
    covariates_col_name = c("x1", "x2", "x3", "x4", "x5"),
    method_weighting_obj = method,
    alpha = 0.01
  )
  analysis_narrow <- setup_analysis_primary(
    data = SyntheticData,
    trial_status_col_name = "S",
    treatment_col_name = "A",
    outcome_col_name = c("y1", "y2"),
    covariates_col_name = c("x1", "x2", "x3", "x4", "x5"),
    method_weighting_obj = method,
    alpha = 0.10
  )
  res_wide <- run_analysis(analysis_wide)
  res_narrow <- run_analysis(analysis_narrow)

  ci_width_wide <- res_wide$results$upper_CI_normal - res_wide$results$lower_CI_normal
  ci_width_narrow <- res_narrow$results$upper_CI_normal - res_narrow$results$lower_CI_normal
  expect_true(all(ci_width_wide > ci_width_narrow))
})
