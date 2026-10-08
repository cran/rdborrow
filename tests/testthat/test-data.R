test_that("SyntheticDataII is unbalanced and has the SyntheticData columns", {
  d <- SyntheticDataII
  expect_named(d, names(SyntheticData))
  expect_identical(
    as.vector(table(factor(paste(d$S, d$A), c("1 1", "1 0", "0 0")))),
    c(160L, 80L, 140L)
  )
  expect_identical(unique(d$T_cross), 2)
  expect_identical(sum(is.na(d)), 0L)
})

test_that("SyntheticDataII has good overlap in the participation model", {
  d <- SyntheticDataII
  w00 <- .ec_weights(d, "S ~ x1 + x2 + x3 + x4 + x5", d$S)$w00[d$S == 0]
  expect_lt(max(w00) / mean(w00), 10)
})
