test_that("setup_thai_font returns font name invisibly", {
  skip_if_not_installed("showtext")
  skip_if_not_installed("sysfonts")

  result <- setup_thai_font("Sarabun")
  expect_identical(result, "Sarabun")
})

test_that("setup_thai_font errors without showtext", {
  skip_if(requireNamespace("showtext", quietly = TRUE))
  expect_error(setup_thai_font(), "showtext")
})
