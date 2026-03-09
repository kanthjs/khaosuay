# test-cook_split.R — ทดสอบ cook_split()

test_that("cook_split returns a cooked_rice object with correct structure", {
  set.seed(2526)
  df <- expand.grid(
    variety    = c("RD6", "KDML105"),
    fertilizer = c("None", "NPK", "Organic"),
    rep        = 1:4,
    stringsAsFactors = FALSE
  )
  df$yield <- with(df, {
    v_eff <- ifelse(variety == "KDML105", 180, 0)
    f_eff <- ifelse(fertilizer == "NPK", 80, ifelse(fertilizer == "Organic", 50, 0))
    500 + v_eff + f_eff + rnorm(nrow(df), 0, 20)
  })
  washed <- wash_rice(df, verbose = FALSE)
  result <- cook_split(washed,
    response  = "yield",
    main_plot = "variety",
    sub_plot  = "fertilizer",
    block     = "rep",
    posthoc   = "tukey"
  )

  expect_s3_class(result, "cooked_rice")
  expect_true(is.list(result$results))
  expect_true("yield" %in% names(result$results))
})

test_that("cook_split provides anova_table and error_terms", {
  set.seed(2526)
  df <- expand.grid(
    variety    = c("RD6", "KDML105"),
    fertilizer = c("None", "NPK", "Organic"),
    rep        = 1:4,
    stringsAsFactors = FALSE
  )
  df$yield <- with(df, {
    v_eff <- ifelse(variety == "KDML105", 180, 0)
    f_eff <- ifelse(fertilizer == "NPK", 80, ifelse(fertilizer == "Organic", 50, 0))
    500 + v_eff + f_eff + rnorm(nrow(df), 0, 20)
  })
  washed <- wash_rice(df, verbose = FALSE)
  result <- cook_split(washed,
    response  = "yield",
    main_plot = "variety",
    sub_plot  = "fertilizer",
    block     = "rep"
  )

  expect_false(is.null(result$results$yield$anova_table))
  expect_false(is.null(result$results$yield$error_terms))
})

test_that("cook_split provides summary_main and summary_sub tables", {
  set.seed(2526)
  df <- expand.grid(
    variety    = c("RD6", "KDML105"),
    fertilizer = c("None", "NPK", "Organic"),
    rep        = 1:4,
    stringsAsFactors = FALSE
  )
  df$yield <- with(df, {
    v_eff <- ifelse(variety == "KDML105", 180, 0)
    f_eff <- ifelse(fertilizer == "NPK", 80, ifelse(fertilizer == "Organic", 50, 0))
    500 + v_eff + f_eff + rnorm(nrow(df), 0, 20)
  })
  washed <- wash_rice(df, verbose = FALSE)
  result <- cook_split(washed,
    response  = "yield",
    main_plot = "variety",
    sub_plot  = "fertilizer",
    block     = "rep",
    posthoc   = "tukey"
  )

  expect_true(is.data.frame(result$results$yield$summary_main))
  expect_true(is.data.frame(result$results$yield$summary_sub))
  expect_true(all(c("treatment", "n", "mean", "sd") %in%
                    names(result$results$yield$summary_main)))
})

test_that("cook_split skips sub-plot post-hoc when sub-plot not significant", {
  set.seed(42)
  df <- expand.grid(
    tillage  = c("Conventional", "Zero_till", "Strip_till"),
    nitrogen = c("Low", "High"),
    rep      = 1:4,
    stringsAsFactors = FALSE
  )
  df$yield <- with(df, {
    t_eff <- ifelse(tillage == "Zero_till", 60,
                    ifelse(tillage == "Strip_till", 40, 0))
    500 + t_eff + rnorm(nrow(df), 0, 25)
  })
  washed <- wash_rice(df, verbose = FALSE)
  result <- cook_split(washed,
    response  = "yield",
    main_plot = "tillage",
    sub_plot  = "nitrogen",
    block     = "rep"
  )

  expect_s3_class(result, "cooked_rice")
  # ถ้า nitrogen ไม่มีนัย → posthoc_sub ควรเป็น NULL
  expect_true(is.null(result$results$yield$posthoc_sub))
})

test_that("cook_split accepts pre-computed tasted object", {
  set.seed(2526)
  df <- expand.grid(
    variety    = c("RD6", "KDML105"),
    fertilizer = c("None", "NPK", "Organic"),
    rep        = 1:4,
    stringsAsFactors = FALSE
  )
  df$yield <- with(df, {
    v_eff <- ifelse(variety == "KDML105", 180, 0)
    f_eff <- ifelse(fertilizer == "NPK", 80, ifelse(fertilizer == "Organic", 50, 0))
    500 + v_eff + f_eff + rnorm(nrow(df), 0, 20)
  })
  washed <- wash_rice(df, verbose = FALSE)
  tasted_obj <- taste_rice(washed, response = "yield",
                           treatment = "variety", block = "rep",
                           mode = "model", plot = FALSE, verbose = FALSE)
  result <- cook_split(washed,
    response  = "yield",
    main_plot = "variety",
    sub_plot  = "fertilizer",
    block     = "rep",
    tasted    = tasted_obj
  )

  expect_s3_class(result, "cooked_rice")
})

test_that("cook_split Non-normal data still returns cooked_rice", {
  set.seed(99)
  df <- expand.grid(
    spray = c("None", "Bio", "Chemical"),
    dose  = c("Low", "High"),
    rep   = 1:5,
    stringsAsFactors = FALSE
  )
  df$severity <- with(df, {
    base <- ifelse(spray == "None", 40, ifelse(spray == "Bio", 15, 5))
    rexp(nrow(df), rate = 1 / base)
  })
  washed <- wash_rice(df, verbose = FALSE)
  result <- cook_split(washed,
    response  = "severity",
    main_plot = "spray",
    sub_plot  = "dose",
    block     = "rep"
  )

  expect_s3_class(result, "cooked_rice")
})

test_that("cook_split handles multiple response variables", {
  set.seed(2526)
  df <- expand.grid(
    variety    = c("RD6", "KDML105"),
    fertilizer = c("None", "NPK", "Organic"),
    rep        = 1:4,
    stringsAsFactors = FALSE
  )
  df$yield <- with(df, {
    500 + ifelse(variety == "KDML105", 180, 0) + rnorm(nrow(df), 0, 20)
  })
  df$severity <- with(df, {
    pmax(0, 20 + ifelse(variety == "RD6", 10, 0) + rnorm(nrow(df), 0, 4))
  })
  washed <- wash_rice(df, verbose = FALSE)
  result <- cook_split(washed,
    response  = c("yield", "severity"),
    main_plot = "variety",
    sub_plot  = "fertilizer",
    block     = "rep",
    posthoc   = "tukey"
  )

  expect_true(all(c("yield", "severity") %in% names(result$results)))
})
