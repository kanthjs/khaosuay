test_that("wash_rice removes NA values", {
  df <- data.frame(
    x = c(1, 2, NA, 4),
    y = c("a", "b", "c", NA)
  )
  cleaned <- wash_rice(df)
  expect_equal(nrow(cleaned), 2)
  expect_false(any(is.na(cleaned)))
})
