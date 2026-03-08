test_that("serve_plate returns a ggplot object", {
  df <- data.frame(
    x = factor(c("A", "B", "C")),
    y = c(10, 15, 12)
  )
  p <- serve_plate(df, "x", "y")
  expect_s3_class(p, "ggplot")
})

test_that("serve_plate respects font_family parameter", {
  df <- data.frame(
    x = factor(c("A", "B", "C")),
    y = c(10, 15, 12)
  )
  p <- serve_plate(df, "x", "y", font_family = "sans")
  expect_s3_class(p, "ggplot")
  expect_identical(p$theme$text$family, "sans")
})

test_that("serve_plate uses custom title", {
  df <- data.frame(x = factor("A"), y = 10)
  p <- serve_plate(df, "x", "y", title = "ทดสอบ")
  expect_identical(p$labels$title, "ทดสอบ")
})
