test_that("setup_analysis returns valid object", {
  obj <- setup_analysis(
    data = SyntheticData,
    trial_status_col_name = "S",
    treatment_col_name = "A",
    outcome_col_name = c("y1", "y2"),
    covariates_col_name = c("x1", "x2", "x3", "x4", "x5"),
    method_obj = setup_method(method_name = "AIPW")
  )
  expect_s4_class(obj, "analysis_obj")
  expect_identical(obj@trial_status_col_name, "S")
  expect_identical(obj@treatment_col_name, "A")
  expect_identical(obj@outcome_col_name, c("y1", "y2"))
  expect_identical(obj@covariates_col_name, c("x1", "x2", "x3", "x4", "x5"))
  expect_identical(obj@alpha, 0.05)
})

test_that("setup_analysis validates data", {
  expect_error(setup_analysis(
    data = list(),
    trial_status_col_name = "S",
    treatment_col_name = "A",
    outcome_col_name = "y1",
    covariates_col_name = "x1",
    method_obj = setup_method()
  ))
})

test_that("setup_analysis validates column names exist in data", {
  expect_error(setup_analysis(
    data = SyntheticData,
    trial_status_col_name = "not_a_col",
    treatment_col_name = "A",
    outcome_col_name = "y1",
    covariates_col_name = "x1",
    method_obj = setup_method()
  ))
  expect_error(setup_analysis(
    data = SyntheticData,
    trial_status_col_name = "S",
    treatment_col_name = "A",
    outcome_col_name = "not_a_col",
    covariates_col_name = "x1",
    method_obj = setup_method()
  ))
})

test_that("setup_analysis validates method_obj", {
  expect_error(setup_analysis(
    data = SyntheticData,
    trial_status_col_name = "S",
    treatment_col_name = "A",
    outcome_col_name = "y1",
    covariates_col_name = "x1",
    method_obj = list()
  ))
})

test_that("setup_analysis validates alpha", {
  expect_error(setup_analysis(
    data = SyntheticData,
    trial_status_col_name = "S",
    treatment_col_name = "A",
    outcome_col_name = "y1",
    covariates_col_name = "x1",
    method_obj = setup_method(),
    alpha = -0.1
  ))
  expect_error(setup_analysis(
    data = SyntheticData,
    trial_status_col_name = "S",
    treatment_col_name = "A",
    outcome_col_name = "y1",
    covariates_col_name = "x1",
    method_obj = setup_method(),
    alpha = 1.1
  ))
})

test_that("alpha must be strictly between 0 and 1", {
  outs <- c("y1", "y2", "y3", "y4")
  for (a in c(0, 1)) {
    expect_error(
      setup_analysis_primary(
        SyntheticData, "S", "A", "y1", "x1", ec_ipw("S ~ x1"),
        alpha = a
      ),
      "alpha.*between 0 and 1"
    )
    expect_error(
      setup_analysis_OLE(
        SyntheticData, "S", "A", outs, "x1",
        did_ec_ipw("S ~ x1", bootstrap = 2),
        T_cross = 2, alpha = a
      ),
      "alpha.*between 0 and 1"
    )
  }
})

test_that("trial-status and treatment columns must be numeric or logical", {
  setup <- function(data) {
    setup_analysis_primary(
      data, "S", "A", c("y1", "y2"), c("x1", "x2"),
      ec_ipw("S ~ x1 + x2")
    )
  }
  as_factor <- function(col) {
    d <- SyntheticData
    d[[col]] <- factor(d[[col]], levels = c("1", "0"))
    d
  }
  as_logical <- SyntheticData
  as_logical$A <- as_logical$A == 1

  expect_error(setup(as_factor("A")), "A.*Must be of type .numeric.")
  expect_error(setup(as_factor("S")), "S.*Must be of type .numeric.")
  expect_error(
    setup_analysis_OLE(
      as_factor("A"), "S", "A", c("y1", "y2", "y3", "y4"), c("x1", "x2"),
      did_ec_ipw("S ~ x1", trt_formula = "A ~ x1", bootstrap = 2),
      T_cross = 2
    ),
    "A.*Must be of type .numeric."
  )
  expect_s4_class(setup(as_logical), "analysis_primary_obj")
})

test_that("estimate() also rejects a factor treatment column", {
  d <- SyntheticData
  d$A <- factor(d$A, levels = c("1", "0"))
  outs <- c("y1", "y2", "y3", "y4")
  call_estimate <- function(method, ...) {
    estimate(
      method,
      data = d, outcomes = outs, treatment = "A", trial_status = "S",
      covariates = c("x1", "x2"), ...
    )
  }
  expect_error(
    call_estimate(did_ec_ipw("S ~ x1", "A ~ x1 + x2", bootstrap = 2), T_cross = 2),
    "A.*Must be of type .numeric."
  )
  expect_error(
    call_estimate(
      did_ec_aipw("S ~ x1", "A ~ x1 + x2", paste(outs, "~ x1"), bootstrap = 2),
      T_cross = 2
    ),
    "A.*Must be of type .numeric."
  )
  expect_error(
    call_estimate(ec_ipw("S ~ x1")),
    "A.*Must be of type .numeric."
  )
})

test_that("external controls coded as treated are rejected", {
  d <- SyntheticData
  d$A[d$S == 0][1:20] <- 1
  outs <- c("y1", "y2", "y3", "y4")
  msg <- "20 external controls have treatment 1"

  expect_error(
    setup_analysis_primary(d, "S", "A", outs, "x1", ec_ipw("S ~ x1")),
    msg
  )
  expect_error(
    setup_analysis_OLE(
      d, "S", "A", outs, "x1", did_ec_ipw("S ~ x1", bootstrap = 2),
      T_cross = 2
    ),
    msg
  )
  expect_error(
    estimate(
      ec_ipw("S ~ x1"),
      data = d, outcomes = outs, treatment = "A", trial_status = "S",
      covariates = "x1"
    ),
    msg
  )
})

test_that("missing values in outcomes or covariates are rejected", {
  outs <- c("y1", "y2", "y3", "y4")
  with_na <- function(col) {
    d <- SyntheticData
    d[[col]][5] <- NA
    d
  }
  setup <- function(d) {
    setup_analysis_primary(d, "S", "A", outs, "x1", ec_ipw("S ~ x1"))
  }

  expect_error(setup(with_na("y1")), "missing values.*'y1', row 5")
  expect_error(setup(with_na("x1")), "missing values.*'x1', row 5")
  expect_error(
    setup_analysis_OLE(
      with_na("y3"), "S", "A", outs, "x1",
      did_ec_ipw("S ~ x1", bootstrap = 2),
      T_cross = 2
    ),
    "missing values.*'y3', row 5"
  )
  expect_error(
    estimate(
      ec_ipw("S ~ x1"),
      data = with_na("y2"), outcomes = outs, treatment = "A",
      trial_status = "S", covariates = "x1"
    ),
    "missing values.*'y2', row 5"
  )
  expect_s4_class(setup(with_na("x2")), "analysis_primary_obj")
})

test_that("an empty trial treated or trial control group is rejected", {
  d <- SyntheticData
  outs <- c("y1", "y2", "y3", "y4")
  no_treated <- d[!(d$S == 1 & d$A == 1), ]
  no_controls <- d[!(d$S == 1 & d$A == 0), ]

  expect_error(
    setup_analysis_primary(no_treated, "S", "A", outs, "x1", ec_ipw("S ~ x1")),
    "no trial treated patients"
  )
  expect_error(
    setup_analysis_OLE(
      no_controls, "S", "A", outs, "x1", did_ec_ipw("S ~ x1", bootstrap = 2),
      T_cross = 2
    ),
    "no trial controls"
  )
  expect_error(
    estimate(
      ec_ipw("S ~ x1"),
      data = no_controls, outcomes = outs, treatment = "A",
      trial_status = "S", covariates = "x1"
    ),
    "no trial controls"
  )
})

test_that("no external controls is rejected unless the method does not use them", {
  outs <- c("y1", "y2")
  of <- paste(outs, "~ x1")
  no_external <- SyntheticData[SyntheticData$S == 1, ]
  fit <- function(data, method) {
    run_analysis(setup_analysis_primary(data, "S", "A", outs, "x1", method))
  }

  expect_error(
    setup_analysis_OLE(
      no_external, "S", "A", c(outs, "y3"), "x1",
      did_ec_ipw("S ~ x1", bootstrap = 2),
      T_cross = 2
    ),
    "no external controls"
  )
  expect_error(fit(no_external, ec_ipw("S ~ x1")), "no external controls")
  expect_error(
    fit(no_external, ec_aipw("S ~ x1", of, weight = 0)),
    "no external controls"
  )
  expect_equal(
    suppressWarnings(fit(no_external, ec_ipw("S ~ x1", weight = 0))),
    fit(SyntheticData, ec_ipw("S ~ x1", weight = 0))
  )
})

test_that("outcomes and covariates cannot use the internal names S and A", {
  covs <- c("x1", "x2")
  outcome_a <- SyntheticData
  names(outcome_a)[names(outcome_a) == "A"] <- "trt"
  outcome_a$A <- outcome_a$y1
  covariate_s <- SyntheticData
  names(covariate_s)[names(covariate_s) == "S"] <- "in_trial"
  covariate_s$S <- covariate_s$x1

  expect_error(
    setup_analysis_primary(
      outcome_a, "S", "trt", c("A", "y2"), covs, ec_ipw("S ~ x1")
    ),
    "names S and A.*A"
  )
  expect_error(
    setup_analysis_OLE(
      covariate_s, "in_trial", "A", c("y1", "y2", "y3"), c("S", "x2"),
      did_ec_ipw("in_trial ~ S", bootstrap = 2),
      T_cross = 2
    ),
    "names S and A.*S"
  )
  expect_error(
    estimate(
      ec_ipw("S ~ x1"),
      data = outcome_a, outcomes = c("A", "y2"), treatment = "trt",
      trial_status = "S", covariates = covs
    ),
    "names S and A.*A"
  )
  expect_error(
    setup_analysis_primary(
      SyntheticData, "S", "A", c("y1", "y2"), c("x1", "A"), ec_ipw("S ~ x1")
    ),
    "names S and A.*A"
  )
})

test_that("show method prints without error", {
  obj <- setup_analysis(
    data = SyntheticData,
    trial_status_col_name = "S",
    treatment_col_name = "A",
    outcome_col_name = c("y1", "y2"),
    covariates_col_name = c("x1", "x2", "x3", "x4", "x5"),
    method_obj = setup_method(method_name = "AIPW")
  )
  expect_output(show(obj), "analysis_obj")
  expect_output(show(obj), "300")
})

test_that("outcome and covariate names must not repeat", {
  outs <- c("y1", "y2")
  expect_error(
    setup_analysis_primary(SyntheticData, "S", "A", c("y1", "y1"), "x1", ec_ipw("S ~ x1")),
    "repeated: y1"
  )
  expect_error(
    setup_analysis_primary(SyntheticData, "S", "A", outs, c("x1", "x1"), ec_ipw("S ~ x1")),
    "repeated: x1"
  )
  expect_error(
    setup_analysis_primary(SyntheticData, "S", "A", outs, c("x1", "y2"), ec_ipw("S ~ x1")),
    "repeated: y2"
  )
  expect_error(
    estimate(
      ec_ipw("S ~ x1"),
      data = SyntheticData, outcomes = c("y1", "y1"), treatment = "A",
      trial_status = "S", covariates = "x1"
    ),
    "repeated: y1"
  )
})

test_that("outcome and covariate names need not be syntactic", {
  d <- SyntheticData
  names(d)[names(d) == "y1"] <- "week 12"
  names(d)[names(d) == "x1"] <- "SMA type"
  fit <- function(data, outs, covs, method) {
    run_analysis(setup_analysis_primary(data, "S", "A", outs, covs, method))
  }
  expect_equal(
    fit(d, c("week 12", "y2"), "SMA type", ec_ipw("S ~ `SMA type`")),
    fit(SyntheticData, c("y1", "y2"), "x1", ec_ipw("S ~ x1")),
    ignore_attr = TRUE
  )
  expect_equal(
    fit(
      d, c("week 12", "y2"), "SMA type",
      ec_aipw("S ~ `SMA type`", c("`week 12` ~ `SMA type`", "y2 ~ `SMA type`"))
    ),
    fit(
      SyntheticData, c("y1", "y2"), "x1",
      ec_aipw("S ~ x1", c("y1 ~ x1", "y2 ~ x1"))
    ),
    ignore_attr = TRUE
  )
})
