test_that("cook_rcbd returns an aov object", {
  # dummy data for RCBD
  df <- expand.grid(
    nitro = factor(1:4),
    rep = factor(1:3)
  )
  df$yield <- rnorm(nrow(df))
  
  model <- cook_rcbd(df, "yield", "nitro", "rep")
  
  expect_s3_class(model, "aov")
  expect_s3_class(model, "lm")
})
