# test-cook_rcbd.R — ทดสอบ cook_rcbd()

test_that("cook_rcbd returns a cooked_rice object with correct structure", {
  set.seed(2526)
  df <- data.frame(
    treatment = rep(c("RD6", "RD15", "KDML105", "PTT1"), each = 5),
    rep = rep(1:5, times = 4),
    yield = c(
      rnorm(5, 620, 20), rnorm(5, 710, 20),
      rnorm(5, 830, 20), rnorm(5, 560, 20)
    ),
    stringsAsFactors = FALSE
  )
  washed <- wash_rice(df, verbose = FALSE)
  tasted <- taste_rice(washed,
    response = "yield", block = "rep",
    mode = "model", plot = FALSE, verbose = FALSE
  )
  result <- cook_rcbd(washed,
    response = "yield", block = "rep",
    tasted = tasted, posthoc = "tukey"
  )

  expect_s3_class(result, "cooked_rice")
  expect_true(is.list(result$results))
  expect_true("yield" %in% names(result$results))
  expect_true(is.data.frame(result$results$yield$summary_table))
})

test_that("cook_rcbd summary_table has required columns and correct row count", {
  set.seed(2526)
  df <- data.frame(
    treatment = rep(c("RD6", "RD15", "KDML105", "PTT1"), each = 5),
    rep = rep(1:5, times = 4),
    yield = c(
      rnorm(5, 620, 20), rnorm(5, 710, 20),
      rnorm(5, 830, 20), rnorm(5, 560, 20)
    ),
    stringsAsFactors = FALSE
  )
  washed <- wash_rice(df, verbose = FALSE)
  tasted <- taste_rice(washed,
    response = "yield", block = "rep",
    mode = "model", plot = FALSE, verbose = FALSE
  )
  result <- cook_rcbd(washed,
    response = "yield", block = "rep",
    tasted = tasted, posthoc = "tukey"
  )

  tbl <- result$results$yield$summary_table
  expect_true(all(c("treatment", "n", "mean", "sd") %in% names(tbl)))
  expect_equal(nrow(tbl), 4)
})

test_that("cook_rcbd provides cv_percent", {
  set.seed(2526)
  df <- data.frame(
    treatment = rep(c("A", "B", "C"), each = 5),
    rep = rep(1:5, times = 3),
    yield = c(rnorm(5, 500, 20), rnorm(5, 700, 20), rnorm(5, 600, 20)),
    stringsAsFactors = FALSE
  )
  washed <- wash_rice(df, verbose = FALSE)
  tasted <- taste_rice(washed,
    response = "yield", block = "rep",
    mode = "model", plot = FALSE, verbose = FALSE
  )
  result <- cook_rcbd(washed,
    response = "yield", block = "rep",
    tasted = tasted
  )

  expect_false(is.null(result$results$yield$cv_percent))
  expect_type(result$results$yield$cv_percent, "double")
})

test_that("cook_rcbd Non-normal uses Friedman test", {
  set.seed(99)
  df <- data.frame(
    treatment = rep(c("Control", "Fung_A", "Fung_B"), each = 10),
    rep = rep(1:10, times = 3),
    severity = c(rexp(10, 0.02), rexp(10, 0.5), rexp(10, 1)),
    stringsAsFactors = FALSE
  )
  washed <- wash_rice(df, verbose = FALSE)
  tasted <- taste_rice(washed,
    response = "severity", block = "rep",
    mode = "model", plot = FALSE, verbose = FALSE
  )
  result <- cook_rcbd(washed,
    response = "severity", block = "rep",
    tasted = tasted
  )

  expect_s3_class(result, "cooked_rice")
  expect_true(grepl("Friedman|Kruskal", result$results$severity$test_name,
    ignore.case = TRUE
  ))
})

test_that("cook_rcbd does not perform post-hoc when p > 0.05", {
  set.seed(777)
  df <- data.frame(
    treatment = rep(c("X", "Y", "Z"), each = 6),
    rep = rep(1:6, times = 3),
    yield = rnorm(18, 600, 30),
    stringsAsFactors = FALSE
  )
  washed <- wash_rice(df, verbose = FALSE)
  tasted <- taste_rice(washed,
    response = "yield", block = "rep",
    mode = "model", plot = FALSE, verbose = FALSE
  )
  result <- cook_rcbd(washed,
    response = "yield", block = "rep",
    tasted = tasted
  )

  expect_false(isTRUE(result$results$yield$posthoc_needed))
})

test_that("cook_rcbd works without tasted (auto-check fallback)", {
  set.seed(2526)
  df <- data.frame(
    treatment = rep(c("A", "B", "C"), each = 5),
    rep = rep(1:5, times = 3),
    yield = c(rnorm(5, 500, 20), rnorm(5, 700, 20), rnorm(5, 600, 20)),
    stringsAsFactors = FALSE
  )
  washed <- wash_rice(df, verbose = FALSE)
  result <- cook_rcbd(washed, response = "yield", block = "rep")

  expect_s3_class(result, "cooked_rice")
  expect_false(is.null(result$results$yield$assumption$source))
})

test_that("cook_rcbd handles multiple response variables", {
  set.seed(2526)
  df <- data.frame(
    treatment = rep(c("RD6", "RD15", "KDML105", "PTT1"), each = 5),
    rep = rep(1:5, times = 4),
    yield = c(
      rnorm(5, 620, 20), rnorm(5, 710, 20),
      rnorm(5, 830, 20), rnorm(5, 560, 20)
    ),
    severity = c(
      rnorm(5, 12, 3), rnorm(5, 6, 3),
      rnorm(5, 25, 3), rnorm(5, 18, 3)
    ),
    stringsAsFactors = FALSE
  )
  washed <- wash_rice(df, verbose = FALSE)
  tasted <- taste_rice(washed,
    response = c("yield", "severity"),
    block = "rep", mode = "model",
    plot = FALSE, verbose = FALSE
  )
  result <- cook_rcbd(washed,
    response = c("yield", "severity"),
    block = "rep", tasted = tasted, posthoc = "tukey"
  )

  expect_true(all(c("yield", "severity") %in% names(result$results)))
})

test_that("cook_rcbd factorial design includes anova_table and p_values", {
  set.seed(123)
  df <- expand.grid(
    variety = c("RD6", "KDML105"),
    chemical = c("Control", "Fung_A", "Fung_B"),
    rep = 1:4,
    stringsAsFactors = FALSE
  )
  df$yield <- with(df, {
    v <- ifelse(variety == "KDML105", 180, 0)
    ch <- ifelse(chemical == "Fung_A", 50, ifelse(chemical == "Fung_B", 80, 0))
    500 + v + ch + rnorm(nrow(df), 0, 20)
  })
  washed <- wash_rice(df, verbose = FALSE)
  tasted <- taste_rice(washed,
    response = "yield", treatment = c("variety", "chemical"),
    block = "rep", mode = "model",
    plot = FALSE, verbose = FALSE
  )
  result <- cook_rcbd(washed,
    response = "yield",
    factors  = c("variety", "chemical"),
    block    = "rep",
    tasted   = tasted,
    posthoc  = "tukey"
  )

  expect_s3_class(result, "cooked_rice")
  expect_false(is.null(result$results$yield$anova_table))
  expect_false(is.null(result$results$yield$p_values))
})

test_that("cook_rcbd post-hoc methods tukey/lsd/duncan all work", {
  set.seed(2526)
  df <- data.frame(
    treatment = rep(c("A", "B", "C"), each = 5),
    rep = rep(1:5, times = 3),
    yield = c(rnorm(5, 500, 20), rnorm(5, 700, 20), rnorm(5, 600, 20)),
    stringsAsFactors = FALSE
  )
  washed <- wash_rice(df, verbose = FALSE)
  tasted <- taste_rice(washed,
    response = "yield", block = "rep",
    mode = "model", plot = FALSE, verbose = FALSE
  )

  skip_if_not_installed("agricolae")
  for (method in c("tukey", "lsd", "duncan")) {
    result <- cook_rcbd(washed,
      response = "yield", block = "rep",
      tasted = tasted, posthoc = method, verbose = FALSE
    )
    expect_s3_class(result, "cooked_rice")
  }
})

test_that("cook_rcbd model slot is accessible for further analysis", {
  set.seed(2526)
  df <- data.frame(
    treatment = rep(c("A", "B", "C"), each = 5),
    rep = rep(1:5, times = 3),
    yield = c(rnorm(5, 500, 20), rnorm(5, 700, 20), rnorm(5, 600, 20)),
    stringsAsFactors = FALSE
  )
  washed <- wash_rice(df, verbose = FALSE)
  tasted <- taste_rice(washed,
    response = "yield", block = "rep",
    mode = "model", plot = FALSE, verbose = FALSE
  )
  result <- cook_rcbd(washed,
    response = "yield", block = "rep",
    tasted = tasted, posthoc = "tukey"
  )

  expect_false(is.null(result$results$yield$model))
  expect_false(is.null(result$results$yield$mse))
  expect_false(is.null(result$results$yield$df_error))
})
