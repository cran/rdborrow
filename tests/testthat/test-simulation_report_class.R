test_that("setup_simulation_report creates valid object", {
  report <- setup_simulation_report(
    method_description = c("IPW", "AIPW"),
    bias = c(0.1, 0.05),
    variance = c(0.5, 0.3),
    mse = c(0.51, 0.3025),
    coverage = c(0.95, 0.93)
  )

  expect_s4_class(report, "simulation_report_obj")
  expect_equal(report@method_description, c("IPW", "AIPW"))
  expect_equal(report@bias, c(0.1, 0.05))
  expect_equal(report@variance, c(0.5, 0.3))
  expect_equal(report@mse, c(0.51, 0.3025))
  expect_equal(report@coverage, c(0.95, 0.93))
  expect_length(report@type_I_error, 0)
  expect_length(report@power, 0)
})

test_that("setup_simulation_report stores type_I_error and power", {
  report <- setup_simulation_report(
    method_description = "IPW",
    bias = 0.1,
    variance = 0.5,
    mse = 0.51,
    coverage = 0.95,
    type_I_error = 0.05,
    power = 0.8
  )

  expect_equal(report@type_I_error, 0.05)
  expect_equal(report@power, 0.8)
})

test_that("setup_simulation_report works with single method", {
  report <- setup_simulation_report(
    method_description = "SCM",
    bias = 0.02,
    variance = 0.1,
    mse = 0.1004,
    coverage = 0.94
  )

  expect_length(report@method_description, 1)
  expect_length(report@bias, 1)
})

test_that("show method prints data frame", {
  report <- setup_simulation_report(
    method_description = c("IPW", "AIPW"),
    bias = c(0.1, 0.05),
    variance = c(0.5, 0.3),
    mse = c(0.51, 0.3025),
    coverage = c(0.95, 0.93)
  )

  expect_output(show(report), "method_description")
  expect_output(show(report), "bias")
})

test_that("show method includes type_I_error and power when present", {
  report <- setup_simulation_report(
    method_description = "IPW",
    bias = 0.1,
    variance = 0.5,
    mse = 0.51,
    coverage = 0.95,
    type_I_error = 0.05,
    power = 0.8
  )

  expect_output(show(report), "type_I_error")
  expect_output(show(report), "power")
})

test_that("show method omits type_I_error and power when empty", {
  report <- setup_simulation_report(
    method_description = "IPW",
    bias = 0.1,
    variance = 0.5,
    mse = 0.51,
    coverage = 0.95
  )

  output <- capture.output(show(report))
  expect_false(any(grepl("type_I_error", output)))
  expect_false(any(grepl("power", output)))
})
