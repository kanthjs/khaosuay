# test-wash_rice.R — ทดสอบ wash_rice()

test_that("wash_rice returns a washed_rice object with correct structure", {
  df <- data.frame(
    Treatment = c("A", "A", "B", "B"),
    Rep       = c(1, 2, 1, 2),
    Yield     = c(500, 520, 480, 490),
    stringsAsFactors = FALSE
  )
  result <- wash_rice(df, verbose = FALSE)

  expect_s3_class(result, "washed_rice")
  expect_true(is.data.frame(result$data))
  expect_true(is.list(result$report))
  expect_true(is.data.frame(result$flagged))
  expect_true(is.data.frame(result$column_map))
  expect_gt(nrow(result$data), 0)
})

test_that("wash_rice cleans messy English column names (spaces, dots, caps)", {
  set.seed(2526)
  test_rcbd <- data.frame(
    "Trt"        = rep(c("RD6", "RD15", "KDML105", "PTT1"), each = 4),
    "Block."     = rep(1:4, times = 4),
    "Yield..kg." = c(620, 580, 610, 595, 710, 685, 720, 700,
                     830, 810, 850, 825, 560, 540, 570, 555),
    "Severity"   = c(12, 15, 10, 14, 5, 8, 6, 7,
                     25, 22, 28, 3, 18, 20, 15, 17),
    stringsAsFactors = FALSE
  )
  result <- wash_rice(test_rcbd, verbose = FALSE)

  expect_s3_class(result, "washed_rice")
  expect_true(is.data.frame(result$data))
  expect_gt(nrow(result$data), 0)
})

test_that("wash_rice removes duplicates when remove_duplicates = TRUE", {
  df <- data.frame(
    treatment = c("A", "A", "A", "B", "B"),
    rep       = c(1, 2, 1, 1, 2),
    yield     = c(500, 520, 500, 480, 490),
    stringsAsFactors = FALSE
  )
  result <- wash_rice(df, remove_duplicates = TRUE, verbose = FALSE)

  expect_lt(nrow(result$data), nrow(df))
})

test_that("wash_rice flags out-of-range values", {
  df <- data.frame(
    treatment = rep(c("A", "B"), each = 4),
    rep       = rep(1:4, 2),
    severity  = c(10, 15, 110, 5, 8, -3, 20, 12),
    stringsAsFactors = FALSE
  )
  result <- wash_rice(df,
    valid_ranges = list(severity = c(0, 100)),
    verbose = FALSE
  )

  expect_gt(nrow(result$flagged), 0)
})

test_that("wash_rice design_check reports missing combinations for unbalanced design", {
  df <- data.frame(
    treatment = c("A", "A", "A", "B", "B", "C", "C", "C"),
    rep       = c(1, 2, 3, 1, 2, 1, 2, 3),
    yield     = c(500, 520, 510, 480, 490, 550, 560, 540),
    stringsAsFactors = FALSE
  )
  result <- wash_rice(df, design_check = TRUE, verbose = FALSE)

  expect_false(is.null(result$report$missing_combinations))
})

test_that("wash_rice respects explicit treatment_col and rep_col", {
  df <- data.frame(
    Formula   = rep(c("F1", "F2", "F3"), each = 3),
    Round     = rep(1:3, times = 3),
    Yield_rai = c(580, 560, 590, 520, 510, 530, 650, 640, 660),
    stringsAsFactors = FALSE
  )
  result <- wash_rice(df,
    treatment_col = "Formula",
    rep_col       = "Round",
    verbose = FALSE
  )

  expect_s3_class(result, "washed_rice")
  expect_true(is.data.frame(result$data))
})

test_that("wash_rice handles Thai column names via alias dictionary", {
  df <- data.frame(
    "\u0e2a\u0e32\u0e22\u0e1e\u0e31\u0e19\u0e18\u0e38\u0e4c" =
      rep(c("\u0e01\u0e026", "\u0e01\u0e0215", "\u0e02\u0e32\u0e27\u0e14\u0e2d\u0e01\u0e21\u0e30\u0e25\u0e34105"), each = 3),
    "\u0e0b\u0e49\u0e33" = rep(1:3, times = 3),
    "\u0e1c\u0e25\u0e1c\u0e25\u0e34\u0e15" = c(620, 580, 610, 710, 685, 700, 830, 810, 850),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  result <- wash_rice(df, verbose = FALSE)

  expect_s3_class(result, "washed_rice")
  expect_true(is.data.frame(result$data))
})

test_that("wash_rice handles numeric character columns (e.g. '720', '2,500')", {
  df <- data.frame(
    treatment = rep(c("A", "B"), each = 3),
    rep       = rep(1:3, 2),
    yield     = c("620", "580", "2,500", "710", "685", "720"),
    stringsAsFactors = FALSE
  )
  result <- wash_rice(df, verbose = FALSE)

  expect_s3_class(result, "washed_rice")
  expect_type(result$data$yield, "double")
})

test_that("wash_rice converts empty strings to NA", {
  df <- data.frame(
    treatment   = rep(c("A", "B", "C"), each = 3),
    rep         = rep(1:3, 3),
    yield       = c(620, 580, 610, 710, "", 700, 830, 810, 850),
    stringsAsFactors = FALSE
  )
  result <- wash_rice(df, verbose = FALSE)

  expect_true(any(is.na(result$data$yield)))
})
