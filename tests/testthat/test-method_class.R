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
