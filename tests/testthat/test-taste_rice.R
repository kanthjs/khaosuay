# test-taste_rice.R — ทดสอบ taste_rice()

test_that("taste_rice returns a tasted_rice object with correct structure", {
  set.seed(2526)
  df <- data.frame(
    treatment = rep(c("A", "B", "C"), each = 10),
    rep       = rep(1:10, times = 3),
    yield     = c(rnorm(10, 500, 20), rnorm(10, 600, 20), rnorm(10, 550, 20)),
    stringsAsFactors = FALSE
  )
  washed <- wash_rice(df, verbose = FALSE)
  result <- taste_rice(washed, response = "yield",
                       mode = "model", plot = FALSE, verbose = FALSE)

  expect_s3_class(result, "tasted_rice")
  expect_true(is.data.frame(result$results))
  expect_true(is.list(result$details))
  expect_true("final_class" %in% names(result$results))
  expect_true("recommendation" %in% names(result$results))
})

test_that("taste_rice final_class is one of the three valid classifications", {
  set.seed(2526)
  df <- data.frame(
    treatment = rep(c("RD6", "RD15", "KDML105", "PTT1"), each = 10),
    rep       = rep(1:10, times = 4),
    yield     = c(rnorm(10, 620, 20), rnorm(10, 710, 20),
                  rnorm(10, 830, 20), rnorm(10, 560, 20)),
    stringsAsFactors = FALSE
  )
  washed <- wash_rice(df, verbose = FALSE)
  result <- taste_rice(washed, response = "yield",
                       mode = "model", plot = FALSE, verbose = FALSE)

  valid_classes <- c("Normal + Equal Variance", "Normal + Unequal Variance", "Non-normal")
  expect_true(result$results$final_class %in% valid_classes)
})

test_that("taste_rice classifies clearly skewed data as Non-normal", {
  set.seed(99)
  df <- data.frame(
    treatment = rep(c("A", "B", "C"), each = 20),
    severity  = c(rexp(20, 0.1), rexp(20, 0.05), rexp(20, 0.2)),
    stringsAsFactors = FALSE
  )
  washed <- wash_rice(df, verbose = FALSE)
  result <- taste_rice(washed, response = "severity",
                       mode = "model", plot = FALSE, verbose = FALSE)

  expect_equal(result$results$final_class, "Non-normal")
})

test_that("taste_rice handles multiple response variables", {
  set.seed(123)
  df <- data.frame(
    treatment = rep(c("V1", "V2", "V3"), each = 10),
    rep       = rep(1:10, times = 3),
    yield     = c(rnorm(10, 600, 25), rnorm(10, 650, 25), rnorm(10, 580, 25)),
    severity  = c(rexp(10, 0.08), rexp(10, 0.05), rexp(10, 0.1)),
    stringsAsFactors = FALSE
  )
  washed <- wash_rice(df, verbose = FALSE)
  result <- taste_rice(washed, response = c("yield", "severity"),
                       mode = "model", plot = FALSE, verbose = FALSE)

  expect_equal(nrow(result$results), 2)
  expect_true(all(c("yield", "severity") %in% result$results$response))
})

test_that("taste_rice provides $models when mode includes model", {
  set.seed(2526)
  df <- data.frame(
    treatment = rep(c("A", "B", "C"), each = 8),
    rep       = rep(1:8, times = 3),
    yield     = c(rnorm(8, 500, 20), rnorm(8, 700, 20), rnorm(8, 600, 20)),
    stringsAsFactors = FALSE
  )
  washed <- wash_rice(df, verbose = FALSE)
  result <- taste_rice(washed, response = "yield",
                       mode = "model", plot = FALSE, verbose = FALSE)

  expect_false(is.null(result$models))
})

test_that("taste_rice $details contains per-response test info", {
  set.seed(42)
  df <- data.frame(
    treatment = rep(c("A", "B", "C"), each = 10),
    yield     = c(rnorm(10, 500, 20), rnorm(10, 600, 20), rnorm(10, 550, 20)),
    stringsAsFactors = FALSE
  )
  washed <- wash_rice(df, verbose = FALSE)
  result <- taste_rice(washed, response = "yield",
                       mode = "model", plot = FALSE, verbose = FALSE)

  expect_true("yield" %in% names(result$details))
  expect_false(is.null(result$details$yield$final_class))
})

test_that("taste_rice two-group data gives t-test recommendation", {
  set.seed(555)
  df <- data.frame(
    treatment = rep(c("Treated", "Control"), each = 12),
    yield     = c(rnorm(12, 650, 30), rnorm(12, 580, 30)),
    stringsAsFactors = FALSE
  )
  washed <- wash_rice(df, verbose = FALSE)
  result <- taste_rice(washed, response = "yield",
                       mode = "model", plot = FALSE, verbose = FALSE)

  expect_s3_class(result, "tasted_rice")
  expect_equal(nrow(result$results), 1)
})

test_that("taste_rice with block argument works for RCBD data", {
  set.seed(2526)
  df <- data.frame(
    treatment = rep(c("RD6", "RD15", "KDML105", "PTT1"), each = 5),
    rep       = rep(1:5, times = 4),
    yield     = c(rnorm(5, 620, 20), rnorm(5, 710, 20),
                  rnorm(5, 830, 20), rnorm(5, 560, 20)),
    stringsAsFactors = FALSE
  )
  washed <- wash_rice(df, verbose = FALSE)
  result <- taste_rice(washed, response = "yield", block = "rep",
                       mode = "model", plot = FALSE, verbose = FALSE)

  expect_s3_class(result, "tasted_rice")
  expect_gt(nrow(result$results), 0)
})
