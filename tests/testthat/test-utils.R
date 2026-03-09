# test-utils.R — ทดสอบ internal helpers ใน utils.R

# ─────────────────────────────────────────────────────────────────────
# .extract_df()
# ─────────────────────────────────────────────────────────────────────

test_that(".extract_df returns data.frame from plain data.frame", {
  df <- data.frame(x = 1:3)
  expect_true(is.data.frame(khaosuay:::.extract_df(df)))
})

test_that(".extract_df extracts $data from washed_rice object", {
  washed <- wash_rice(
    data.frame(treatment = c("A", "A", "B", "B"),
               rep = c(1, 2, 1, 2),
               y = c(10, 12, 20, 22)),
    verbose = FALSE
  )
  result <- khaosuay:::.extract_df(washed)
  expect_true(is.data.frame(result))
  expect_true("treatment" %in% names(result))
})

# ─────────────────────────────────────────────────────────────────────
# .class_to_path()
# ─────────────────────────────────────────────────────────────────────

test_that(".class_to_path maps classifications to correct paths", {
  expect_equal(khaosuay:::.class_to_path("Normal + Equal Variance"),   "parametric")
  expect_equal(khaosuay:::.class_to_path("Normal + Unequal Variance"), "welch")
  expect_equal(khaosuay:::.class_to_path("Non-normal"),                "non-parametric")
})

test_that(".class_to_path falls back to parametric for unknown input", {
  expect_equal(khaosuay:::.class_to_path("something_else"), "parametric")
})

# ─────────────────────────────────────────────────────────────────────
# .get_classification()
# ─────────────────────────────────────────────────────────────────────

test_that(".get_classification returns non-null for existing response", {
  set.seed(2526)
  df <- data.frame(
    treatment = rep(c("A", "B", "C"), each = 10),
    yield     = c(rnorm(10, 500, 20), rnorm(10, 600, 20), rnorm(10, 550, 20)),
    stringsAsFactors = FALSE
  )
  washed <- wash_rice(df, verbose = FALSE)
  tasted <- taste_rice(washed, response = "yield",
                       mode = "model", plot = FALSE, verbose = FALSE)

  result <- khaosuay:::.get_classification(tasted, "yield")
  expect_false(is.null(result))
  expect_type(result, "character")
})

test_that(".get_classification returns NULL for missing response", {
  set.seed(2526)
  df <- data.frame(
    treatment = rep(c("A", "B", "C"), each = 10),
    yield     = c(rnorm(10, 500, 20), rnorm(10, 600, 20), rnorm(10, 550, 20)),
    stringsAsFactors = FALSE
  )
  washed <- wash_rice(df, verbose = FALSE)
  tasted <- taste_rice(washed, response = "yield",
                       mode = "model", plot = FALSE, verbose = FALSE)

  expect_null(khaosuay:::.get_classification(tasted, "nonexistent"))
})

test_that(".get_classification returns NULL when tasted is NULL", {
  expect_null(khaosuay:::.get_classification(NULL, "yield"))
})

test_that(".get_classification warns and returns NULL for non-tasted_rice input", {
  expect_warning(
    result <- khaosuay:::.get_classification(list(a = 1), "yield"),
    regexp = "tasted_rice"
  )
  expect_null(result)
})

# ─────────────────────────────────────────────────────────────────────
# .quick_assumption()
# ─────────────────────────────────────────────────────────────────────

test_that(".quick_assumption returns valid class for normal data", {
  set.seed(1)
  res <- rnorm(50)
  grp <- rep(c("A", "B"), each = 25)
  qa  <- khaosuay:::.quick_assumption(res, grp)

  expect_true(qa$class %in% c("Normal + Equal Variance",
                               "Normal + Unequal Variance",
                               "Non-normal", "N/A"))
  expect_true(qa$path %in% c("parametric", "welch", "non-parametric"))
  expect_false(is.na(qa$shapiro_p))
})

test_that(".quick_assumption classifies clearly skewed data as Non-normal", {
  set.seed(1)
  res <- rexp(50, 0.1)
  grp <- rep(c("A", "B"), each = 25)
  qa  <- khaosuay:::.quick_assumption(res, grp)

  expect_equal(qa$class, "Non-normal")
  expect_true(is.na(qa$levene_p))  # Levene ข้ามเมื่อ non-normal
})

test_that(".quick_assumption returns N/A when n < 3", {
  qa <- khaosuay:::.quick_assumption(c(1, 2), c("A", "B"))
  expect_equal(qa$class, "N/A")
})

test_that(".quick_assumption returns N/A when sd = 0", {
  qa <- khaosuay:::.quick_assumption(rep(5, 10), rep(c("A", "B"), each = 5))
  expect_equal(qa$class, "N/A")
})

test_that(".quick_assumption handles NA values in residuals", {
  set.seed(1)
  res <- c(rnorm(20), NA, NA)
  grp <- rep(c("A", "B"), length.out = 22)
  qa  <- khaosuay:::.quick_assumption(res, grp)

  expect_type(qa$class, "character")
})

# ─────────────────────────────────────────────────────────────────────
# .make_summary_table()
# ─────────────────────────────────────────────────────────────────────

test_that(".make_summary_table returns correct row count and columns", {
  df <- data.frame(
    trt = rep(c("A", "B", "C"), each = 5),
    y   = c(10, 12, 11, 13, 14, 20, 22, 21, 23, 24, 15, 17, 16, 18, 19),
    stringsAsFactors = FALSE
  )
  tbl <- khaosuay:::.make_summary_table(df, "y", "trt")

  expect_equal(nrow(tbl), 3)
  expect_true(all(c("mean", "sd", "se") %in% names(tbl)))
})

test_that(".make_summary_table has no group column when letters not provided", {
  df <- data.frame(
    trt = rep(c("A", "B"), each = 5),
    y   = c(10, 12, 11, 13, 14, 20, 22, 21, 23, 24),
    stringsAsFactors = FALSE
  )
  tbl <- khaosuay:::.make_summary_table(df, "y", "trt")

  expect_false("group" %in% names(tbl))
})

test_that(".make_summary_table adds group and mean_group columns when letters provided", {
  df <- data.frame(
    trt = rep(c("A", "B", "C"), each = 5),
    y   = c(10, 12, 11, 13, 14, 20, 22, 21, 23, 24, 15, 17, 16, 18, 19),
    stringsAsFactors = FALSE
  )
  ltrs <- c(A = "b", B = "a", C = "ab")
  tbl  <- khaosuay:::.make_summary_table(df, "y", "trt", ltrs)

  expect_true("group" %in% names(tbl))
  expect_true("mean_group" %in% names(tbl))
  expect_equal(tbl$group[tbl$treatment == "A"], "b")
})

test_that(".make_summary_table handles n=1 per group without error", {
  df <- data.frame(
    trt = c("A", "B", "C"),
    y   = c(10, 20, 15),
    stringsAsFactors = FALSE
  )
  tbl <- khaosuay:::.make_summary_table(df, "y", "trt")

  expect_equal(nrow(tbl), 3)
})

# ─────────────────────────────────────────────────────────────────────
# .run_posthoc_parametric()
# ─────────────────────────────────────────────────────────────────────

test_that(".run_posthoc_parametric Tukey returns correct structure", {
  skip_if_not_installed("agricolae")

  set.seed(2526)
  df <- data.frame(
    treatment = factor(rep(c("A", "B", "C"), each = 8)),
    yield     = c(rnorm(8, 500, 20), rnorm(8, 700, 20), rnorm(8, 600, 20))
  )
  mdl  <- aov(yield ~ treatment, data = df)
  aov_s <- summary(mdl)[[1]]
  df_e  <- aov_s$Df[nrow(aov_s)]
  ms_e  <- aov_s$`Mean Sq`[nrow(aov_s)]

  ph <- khaosuay:::.run_posthoc_parametric(mdl, "treatment", df_e, ms_e,
                                            "tukey", 0.05)
  expect_equal(ph$name, "Tukey's HSD")
  expect_length(ph$letters, 3)
  expect_type(ph$letters, "character")
})

test_that(".run_posthoc_parametric LSD has name containing 'LSD'", {
  skip_if_not_installed("agricolae")

  set.seed(2526)
  df <- data.frame(
    treatment = factor(rep(c("A", "B", "C"), each = 8)),
    yield     = c(rnorm(8, 500, 20), rnorm(8, 700, 20), rnorm(8, 600, 20))
  )
  mdl  <- aov(yield ~ treatment, data = df)
  aov_s <- summary(mdl)[[1]]
  df_e  <- aov_s$Df[nrow(aov_s)]
  ms_e  <- aov_s$`Mean Sq`[nrow(aov_s)]

  ph <- khaosuay:::.run_posthoc_parametric(mdl, "treatment", df_e, ms_e,
                                            "lsd", 0.05)
  expect_true(grepl("LSD", ph$name))
})

test_that(".run_posthoc_parametric Duncan has name containing 'Duncan'", {
  skip_if_not_installed("agricolae")

  set.seed(2526)
  df <- data.frame(
    treatment = factor(rep(c("A", "B", "C"), each = 8)),
    yield     = c(rnorm(8, 500, 20), rnorm(8, 700, 20), rnorm(8, 600, 20))
  )
  mdl  <- aov(yield ~ treatment, data = df)
  aov_s <- summary(mdl)[[1]]
  df_e  <- aov_s$Df[nrow(aov_s)]
  ms_e  <- aov_s$`Mean Sq`[nrow(aov_s)]

  ph <- khaosuay:::.run_posthoc_parametric(mdl, "treatment", df_e, ms_e,
                                            "duncan", 0.05)
  expect_true(grepl("Duncan", ph$name))
})

# ─────────────────────────────────────────────────────────────────────
# .run_posthoc_nonparametric()
# ─────────────────────────────────────────────────────────────────────

test_that(".run_posthoc_nonparametric returns correct structure", {
  skip_if_not_installed("agricolae")

  set.seed(99)
  y_np <- c(rexp(15, 0.05), rexp(15, 0.15), rexp(15, 0.3))
  g_np <- rep(c("A", "B", "C"), each = 15)

  ph <- khaosuay:::.run_posthoc_nonparametric(y_np, g_np, 0.05)
  expect_true(grepl("Kruskal", ph$name, ignore.case = TRUE))
  expect_length(ph$letters, 3)
  expect_false(is.null(ph$groups))
})

# ─────────────────────────────────────────────────────────────────────
# .check_agricolae()
# ─────────────────────────────────────────────────────────────────────

test_that(".check_agricolae passes silently when agricolae is installed", {
  skip_if_not_installed("agricolae")
  expect_no_error(khaosuay:::.check_agricolae())
})

test_that(".check_agricolae throws an error when agricolae is missing", {
  skip_if(requireNamespace("agricolae", quietly = TRUE),
          "agricolae is installed — cannot test missing-package path")
  expect_error(khaosuay:::.check_agricolae())
})
