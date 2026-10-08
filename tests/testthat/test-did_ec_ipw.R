test_that("did_ec_ipw() with intercept-only models is the unadjusted DID", {
  for (T_cross in 1:4) {
    df <- make_ole_data(n_time = 5)
    method <- did_ec_ipw(ps_formula = "S ~ 1", bootstrap = 20)
    res <- suppressWarnings(run_analysis(ole_analysis(df, method, T_cross)))
    expect_identical(rownames(res), paste0("tau", (T_cross + 1):5))
    expect_equal(res$point_estimates, unadjusted_did(df, T_cross), tolerance = 1e-10)
  }
})

test_that("did_ec_ipw() matches the Appendix B weighted-mean formula", {
  df <- make_ole_data()
  T_cross <- 2
  trial <- df$S == 1
  pi_s <- predict(glm(S ~ x1 + x2, binomial, df), type = "response")
  w0 <- pi_s / (1 - pi_s)
  pi_a <- predict(glm(A ~ x1 + x2, binomial, df[trial, ]), df, type = "response")
  trt <- trial & df$A == 1
  ctrl <- trial & df$A == 0
  ext <- !trial
  Y <- as.matrix(df[, ole_outcomes(df)])
  pre <- rowMeans(Y[, 1:T_cross])
  expected <- vapply(3:5, \(t) {
    weighted.mean(Y[trt, t], 1 / pi_a[trt]) -
      weighted.mean(pre[ctrl], 1 / (1 - pi_a[ctrl])) -
      weighted.mean(Y[ext, t] - pre[ext], w0[ext])
  }, numeric(1))

  d <- .build_analysis_df(df, ole_outcomes(df), "A", "S", c("x1", "x2"))
  tau <- .did_ec_ipw_core(
    d, Y, d$S, d$A, T_cross, "S ~ x1 + x2", "A ~ x1 + x2"
  )$tau
  expect_equal(unname(tau), expected, tolerance = 1e-10)
})

test_that("did_ec_ipw() responds to outcome shifts as the DID algebra implies", {
  df <- make_ole_data()
  base <- did_tau("ipw", df, 2)

  ext <- df$S == 0
  shifted <- df
  shifted[ext, ole_outcomes(df)] <- shifted[ext, ole_outcomes(df)] + 7
  expect_equal(did_tau("ipw", shifted, 2), base, tolerance = 1e-10)

  ctrl <- df$S == 1 & df$A == 0
  shifted <- df
  shifted$y1[ctrl] <- shifted$y1[ctrl] + 4
  expect_equal(did_tau("ipw", shifted, 2), base - 4 / 2, tolerance = 1e-10)

  shifted <- df
  shifted$y1[ext] <- shifted$y1[ext] + 4
  expect_equal(did_tau("ipw", shifted, 2), base + 4 / 2, tolerance = 1e-10)

  trt <- df$S == 1 & df$A == 1
  shifted <- df
  shifted$y3[trt] <- shifted$y3[trt] + 4
  expect_equal(did_tau("ipw", shifted, 2), base + c(4, 0, 0), tolerance = 1e-10)
})

test_that("did_ec_ipw() confidence intervals honour alpha", {
  df <- make_ole_data(n_time = 4)
  method <- did_ec_ipw(ps_formula = "S ~ x1 + x2", bootstrap = 200)
  set.seed(3)
  wide <- run_analysis(ole_analysis(df, method, 2, alpha = 0.05))
  set.seed(3)
  narrow <- run_analysis(ole_analysis(df, method, 2, alpha = 0.5))
  expect_all_true(narrow$lower_CI_boot > wide$lower_CI_boot)
  expect_all_true(narrow$upper_CI_boot < wide$upper_CI_boot)
})
