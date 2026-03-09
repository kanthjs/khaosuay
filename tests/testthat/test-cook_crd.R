# test-cook_crd.R — ทดสอบ cook_crd()

test_that("cook_crd returns a cooked_rice object with correct structure", {
  set.seed(2526)
  df <- data.frame(
    treatment = rep(c("RD6", "KDML105", "PTT1"), each = 8),
    yield     = c(rnorm(8, 620, 25), rnorm(8, 830, 25), rnorm(8, 560, 25)),
    stringsAsFactors = FALSE
  )
  washed <- wash_rice(df, verbose = FALSE)
  tasted <- taste_rice(washed, response = "yield",
                       mode = "model", plot = FALSE, verbose = FALSE)
  result <- cook_crd(washed, response = "yield", tasted = tasted)

  expect_s3_class(result, "cooked_rice")
  expect_true(is.list(result$results))
  expect_true("yield" %in% names(result$results))
  expect_true(is.data.frame(result$results$yield$summary_table))
})

test_that("cook_crd Normal+Equal produces parametric ANOVA with Tukey", {
  set.seed(2526)
  df <- data.frame(
    treatment = rep(c("RD6", "KDML105", "PTT1"), each = 8),
    yield     = c(rnorm(8, 620, 25), rnorm(8, 830, 25), rnorm(8, 560, 25)),
    stringsAsFactors = FALSE
  )
  washed <- wash_rice(df, verbose = FALSE)
  tasted <- taste_rice(washed, response = "yield",
                       mode = "model", plot = FALSE, verbose = FALSE)
  result <- cook_crd(washed, response = "yield", tasted = tasted,
                     posthoc = "tukey")

  expect_true(!is.null(result$results$yield$anova_table))
  expect_true(!is.null(result$results$yield$cv_percent))
  expect_type(result$results$yield$cv_percent, "double")
})

test_that("cook_crd Non-normal uses Kruskal-Wallis", {
  set.seed(99)
  df <- data.frame(
    treatment = rep(c("Control", "Spray_A", "Spray_B"), each = 15),
    severity  = c(rexp(15, 0.05), rexp(15, 0.15), rexp(15, 0.3)),
    stringsAsFactors = FALSE
  )
  washed <- wash_rice(df, verbose = FALSE)
  tasted <- taste_rice(washed, response = "severity",
                       mode = "model", plot = FALSE, verbose = FALSE)
  result <- cook_crd(washed, response = "severity", tasted = tasted)

  expect_s3_class(result, "cooked_rice")
  expect_true(grepl("Kruskal", result$results$severity$test_name,
                    ignore.case = TRUE))
})

test_that("cook_crd summary_table has required columns", {
  set.seed(2526)
  df <- data.frame(
    treatment = rep(c("A", "B", "C"), each = 8),
    yield     = c(rnorm(8, 500, 20), rnorm(8, 700, 20), rnorm(8, 600, 20)),
    stringsAsFactors = FALSE
  )
  washed <- wash_rice(df, verbose = FALSE)
  tasted <- taste_rice(washed, response = "yield",
                       mode = "model", plot = FALSE, verbose = FALSE)
  result <- cook_crd(washed, response = "yield", tasted = tasted,
                     posthoc = "tukey")

  tbl <- result$results$yield$summary_table
  expect_true(all(c("treatment", "n", "mean", "sd") %in% names(tbl)))
  expect_equal(nrow(tbl), 3)
})

test_that("cook_crd does not perform post-hoc when p > 0.05", {
  set.seed(777)
  df <- data.frame(
    treatment = rep(c("A", "B", "C"), each = 8),
    yield     = rnorm(24, 600, 30),
    stringsAsFactors = FALSE
  )
  washed <- wash_rice(df, verbose = FALSE)
  tasted <- taste_rice(washed, response = "yield",
                       mode = "model", plot = FALSE, verbose = FALSE)
  result <- cook_crd(washed, response = "yield", tasted = tasted)

  expect_false(isTRUE(result$results$yield$posthoc_needed))
})

test_that("cook_crd works with tasted = NULL (auto-check fallback)", {
  set.seed(2526)
  df <- data.frame(
    treatment = rep(c("A", "B", "C"), each = 8),
    yield     = c(rnorm(8, 500, 20), rnorm(8, 700, 20), rnorm(8, 600, 20)),
    stringsAsFactors = FALSE
  )
  washed <- wash_rice(df, verbose = FALSE)
  result <- cook_crd(washed, response = "yield")

  expect_s3_class(result, "cooked_rice")
  expect_false(is.null(result$results$yield$assumption$source))
})

test_that("cook_crd supports multiple response variables", {
  set.seed(2526)
  df <- data.frame(
    treatment = rep(c("RD6", "KDML105", "PTT1"), each = 8),
    yield     = c(rnorm(8, 620, 25), rnorm(8, 830, 25), rnorm(8, 560, 25)),
    severity  = c(rnorm(8, 12, 3),  rnorm(8, 25, 3),  rnorm(8, 18, 3)),
    stringsAsFactors = FALSE
  )
  washed <- wash_rice(df, verbose = FALSE)
  tasted <- taste_rice(washed, response = c("yield", "severity"),
                       mode = "model", plot = FALSE, verbose = FALSE)
  result <- cook_crd(washed, response = c("yield", "severity"),
                     tasted = tasted, posthoc = "tukey")

  expect_true(all(c("yield", "severity") %in% names(result$results)))
})

test_that("cook_crd factorial design includes anova_table and p_values", {
  set.seed(42)
  df <- data.frame(
    variety    = rep(rep(c("RD6", "KDML105"), each = 5), times = 3),
    fertilizer = rep(c("None", "NPK", "Organic"), each = 10),
    yield      = c(
      rnorm(5, 500, 20), rnorm(5, 700, 20),
      rnorm(5, 600, 20), rnorm(5, 850, 20),
      rnorm(5, 550, 20), rnorm(5, 780, 20)
    ),
    stringsAsFactors = FALSE
  )
  washed <- wash_rice(df, verbose = FALSE)
  tasted <- taste_rice(washed, response = "yield", treatment = "variety",
                       mode = "model", plot = FALSE, verbose = FALSE)
  result <- cook_crd(washed,
    response = "yield",
    factors  = c("variety", "fertilizer"),
    tasted   = tasted,
    posthoc  = "tukey"
  )

  expect_s3_class(result, "cooked_rice")
  expect_false(is.null(result$results$yield$anova_table))
  expect_false(is.null(result$results$yield$p_values))
})

test_that("cook_crd post-hoc methods tukey/lsd/duncan all work", {
  set.seed(2526)
  df <- data.frame(
    treatment = rep(c("A", "B", "C"), each = 8),
    yield     = c(rnorm(8, 500, 20), rnorm(8, 700, 20), rnorm(8, 600, 20)),
    stringsAsFactors = FALSE
  )
  washed <- wash_rice(df, verbose = FALSE)
  tasted <- taste_rice(washed, response = "yield",
                       mode = "model", plot = FALSE, verbose = FALSE)

  skip_if_not_installed("agricolae")
  for (method in c("tukey", "lsd", "duncan")) {
    result <- cook_crd(washed, response = "yield", tasted = tasted,
                       posthoc = method, verbose = FALSE)
    expect_s3_class(result, "cooked_rice",
                    label = paste("cook_crd posthoc =", method))
  }
})
