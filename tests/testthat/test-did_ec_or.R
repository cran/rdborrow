test_that("did_ec_or() with intercept-only models is the unadjusted DID", {
  for (T_cross in 1:4) {
    df <- make_ole_data(n_time = 5)
    f <- paste(ole_outcomes(df), "~ 1")
    method <- did_ec_or(f, f, f, bootstrap = 20)
    res <- suppressWarnings(run_analysis(ole_analysis(df, method, T_cross)))
    expect_identical(rownames(res), paste0("tau", (T_cross + 1):5))
    expect_equal(res$point_estimates, unadjusted_did(df, T_cross), tolerance = 1e-10)
  }
})

test_that("did_ec_or() standardizes each group's model to the trial covariates", {
  df <- make_ole_data()
  T_cross <- 2
  outcomes <- ole_outcomes(df)
  f_ext <- paste(outcomes, "~ x1 + x2")
  f_ctrl <- paste(outcomes, "~ x1")
  f_trt <- paste(outcomes, "~ x2")
  trial <- df[df$S == 1, ]
  fit_mean <- function(f, rows) {
    mean(predict(lm(as.formula(f), data = df[rows, ]), newdata = trial))
  }
  ext <- df$S == 0
  ctrl <- df$S == 1 & df$A == 0
  trt <- df$S == 1 & df$A == 1
  m_ext <- vapply(f_ext, fit_mean, numeric(1), rows = ext)
  m_ctrl <- vapply(f_ctrl[1:T_cross], fit_mean, numeric(1), rows = ctrl)
  m_trt <- vapply(f_trt[3:5], fit_mean, numeric(1), rows = trt)
  expected <- unname((m_trt - mean(m_ctrl)) - (m_ext[3:5] - mean(m_ext[1:T_cross])))

  d <- .build_analysis_df(df, outcomes, "A", "S", c("x1", "x2"))
  tau <- .did_ec_or_core(d, d$S, d$A, T_cross, f_ext, f_ctrl, f_trt)$tau
  expect_equal(unname(tau), expected, tolerance = 1e-10)

  method <- did_ec_or(f_ext, f_ctrl, f_trt, bootstrap = 2)
  set.seed(1)
  res <- suppressWarnings(run_analysis(ole_analysis(df, method, T_cross)))
  expect_equal(res$point_estimates, expected, tolerance = 1e-10)
})

test_that("did_ec_or() responds to outcome shifts as the DID algebra implies", {
  df <- make_ole_data()
  base <- did_tau("or", df, 2)

  ext <- df$S == 0
  shifted <- df
  shifted[ext, ole_outcomes(df)] <- shifted[ext, ole_outcomes(df)] + 7
  expect_equal(did_tau("or", shifted, 2), base, tolerance = 1e-10)

  ctrl <- df$S == 1 & df$A == 0
  shifted <- df
  shifted$y1[ctrl] <- shifted$y1[ctrl] + 4
  expect_equal(did_tau("or", shifted, 2), base - 4 / 2, tolerance = 1e-10)

  shifted <- df
  shifted$y1[ext] <- shifted$y1[ext] + 4
  expect_equal(did_tau("or", shifted, 2), base + 4 / 2, tolerance = 1e-10)

  trt <- df$S == 1 & df$A == 1
  shifted <- df
  shifted$y3[trt] <- shifted$y3[trt] + 4
  expect_equal(did_tau("or", shifted, 2), base + c(4, 0, 0), tolerance = 1e-10)
})

test_that("did_ec_or() confidence intervals honour alpha", {
  df <- make_ole_data(n_time = 4)
  f <- paste(ole_outcomes(df), "~ x1 + x2")
  method <- did_ec_or(f, f, f, bootstrap = 200)
  set.seed(3)
  wide <- run_analysis(ole_analysis(df, method, 2, alpha = 0.05))
  set.seed(3)
  narrow <- run_analysis(ole_analysis(df, method, 2, alpha = 0.5))
  expect_all_true(narrow$lower_CI_boot > wide$lower_CI_boot)
  expect_all_true(narrow$upper_CI_boot < wide$upper_CI_boot)
})
