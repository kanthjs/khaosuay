test_that("top_with_sig returns groups data.frame", {
  # dummy data for RCBD
  df <- expand.grid(
    nitro = factor(1:4),
    rep = factor(1:3)
  )
  df$yield <- rnorm(nrow(df))
  
  model <- cook_rcbd(df, "yield", "nitro", "rep")
  groups <- top_with_sig(model, "nitro")
  
  expect_s3_class(groups, "data.frame")
  expect_true("groups" %in% colnames(groups))
})
