test_that("setup_bootstrap is deprecated", {
  expect_error(
    suppressWarnings(setup_bootstrap()),
    "no longer functional"
  )
})
