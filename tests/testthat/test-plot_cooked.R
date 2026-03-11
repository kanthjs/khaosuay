# test-plot_cooked.R — ทดสอบ plot_cooked()

# Helper: สร้าง cooked_rice objects สำหรับใช้ซ้ำในหลาย tests

make_crd_cooked <- function(seed = 2526, n = 4) {
  set.seed(seed)
  df <- data.frame(
    treatment = rep(c("RD6", "KDML105", "PTT1"), each = n),
    yield     = c(rnorm(n, 620, 20), rnorm(n, 830, 20), rnorm(n, 560, 20)),
    stringsAsFactors = FALSE
  )
  washed <- wash_rice(df, verbose = FALSE)
  tasted <- taste_rice(washed, response = "yield",
                       mode = "model", plot = FALSE, verbose = FALSE)
  cook_crd(washed, response = "yield", tasted = tasted, verbose = FALSE)
}

make_rcbd_cooked <- function(seed = 2526) {
  set.seed(seed)
  df <- data.frame(
    treatment = rep(c("A", "B", "C", "D"), each = 4),
    rep       = rep(1:4, times = 4),
    yield     = c(rnorm(4, 550, 15), rnorm(4, 620, 15),
                  rnorm(4, 680, 15), rnorm(4, 700, 15)),
    stringsAsFactors = FALSE
  )
  washed <- wash_rice(df, verbose = FALSE)
  tasted <- taste_rice(washed, response = "yield", block = "rep",
                       mode = "model", plot = FALSE, verbose = FALSE)
  cook_rcbd(washed, response = "yield", block = "rep",
            tasted = tasted, verbose = FALSE)
}

# โครงสร้างผลลัพธ์

test_that("plot_cooked returns invisible list of ggplot objects", {
  skip_if_not_installed("ggplot2")
  cooked <- make_crd_cooked()

  plots <- plot_cooked(cooked)

  expect_true(is.list(plots))
  expect_true("yield" %in% names(plots))
  expect_s3_class(plots$yield, "gg")
})

test_that("plot_cooked errors on non-cooked_rice input", {
  skip_if_not_installed("ggplot2")

  expect_error(plot_cooked(list(a = 1)), regexp = "cooked_rice")
})

test_that("plot_cooked errors when response not found in cooked object", {
  skip_if_not_installed("ggplot2")
  cooked <- make_crd_cooked()

  expect_error(plot_cooked(cooked, response = "nonexistent"),
               regexp = "nonexistent")
})

# plot_type options

test_that("plot_cooked auto → bar when n per group ≤ 4", {
  skip_if_not_installed("ggplot2")
  cooked <- make_crd_cooked(n = 4)

  plots <- plot_cooked(cooked, plot_type = "auto")

  expect_s3_class(plots$yield, "gg")
  # ตรวจว่ามี geom_col layer (bar)
  layer_classes <- sapply(plots$yield$layers, function(l)
    class(l$geom)[1])
  expect_true("GeomCol" %in% layer_classes)
})

test_that("plot_cooked auto → box when n per group > 4", {
  skip_if_not_installed("ggplot2")
  cooked <- make_crd_cooked(n = 10)

  plots <- plot_cooked(cooked, plot_type = "auto")

  expect_s3_class(plots$yield, "gg")
  layer_classes <- sapply(plots$yield$layers, function(l)
    class(l$geom)[1])
  expect_true("GeomBoxplot" %in% layer_classes)
})

test_that("plot_cooked plot_type = 'bar' forces bar even for large n", {
  skip_if_not_installed("ggplot2")
  cooked <- make_crd_cooked(n = 10)

  plots <- plot_cooked(cooked, plot_type = "bar")

  layer_classes <- sapply(plots$yield$layers, function(l)
    class(l$geom)[1])
  expect_true("GeomCol" %in% layer_classes)
})

test_that("plot_cooked plot_type = 'box' forces boxplot even for small n", {
  skip_if_not_installed("ggplot2")
  cooked <- make_rcbd_cooked()

  plots <- plot_cooked(cooked, plot_type = "box")

  layer_classes <- sapply(plots$yield$layers, function(l)
    class(l$geom)[1])
  expect_true("GeomBoxplot" %in% layer_classes)
})

# palette options

test_that("plot_cooked runs without error for all palette options", {
  skip_if_not_installed("ggplot2")
  cooked <- make_rcbd_cooked()

  for (pal in c("viridis", "grey", "set2")) {
    plots <- plot_cooked(cooked, palette = pal)
    expect_s3_class(plots$yield, "gg")
  }
})

# show_points และ error_type

test_that("plot_cooked show_points = FALSE removes jitter layer", {
  skip_if_not_installed("ggplot2")
  cooked <- make_rcbd_cooked()

  plots_with    <- plot_cooked(cooked, show_points = TRUE)
  plots_without <- plot_cooked(cooked, show_points = FALSE)

  n_layers_with    <- length(plots_with$yield$layers)
  n_layers_without <- length(plots_without$yield$layers)

  expect_lt(n_layers_without, n_layers_with)
})

test_that("plot_cooked error_type = 'sd' produces a valid ggplot", {
  skip_if_not_installed("ggplot2")
  cooked <- make_rcbd_cooked()

  plots <- plot_cooked(cooked, error_type = "sd")
  expect_s3_class(plots$yield, "gg")
})

# custom labels

test_that("plot_cooked applies custom x_label and y_label", {
  skip_if_not_installed("ggplot2")
  cooked <- make_rcbd_cooked()

  plots <- plot_cooked(cooked,
                       y_label = "Yield (kg/rai)",
                       x_label = "Treatment")

  expect_equal(plots$yield$labels$y, "Yield (kg/rai)")
  expect_equal(plots$yield$labels$x, "Treatment")
})

test_that("plot_cooked applies custom title", {
  skip_if_not_installed("ggplot2")
  cooked <- make_rcbd_cooked()

  plots <- plot_cooked(cooked, title = "My Chart")
  expect_equal(plots$yield$labels$title, "My Chart")
})

# multiple response variables

test_that("plot_cooked handles multiple responses and returns one plot each", {
  skip_if_not_installed("ggplot2")
  set.seed(2526)
  df <- data.frame(
    treatment = rep(c("RD6", "RD15", "KDML105"), each = 5),
    rep       = rep(1:5, times = 3),
    yield     = c(rnorm(5, 620, 20), rnorm(5, 710, 20), rnorm(5, 830, 20)),
    severity  = c(rnorm(5, 25, 3),  rnorm(5, 15, 3),  rnorm(5, 8, 3)),
    stringsAsFactors = FALSE
  )
  washed <- wash_rice(df, verbose = FALSE)
  tasted <- taste_rice(washed, response = c("yield", "severity"),
                       block = "rep", mode = "model",
                       plot = FALSE, verbose = FALSE)
  cooked <- cook_rcbd(washed, response = c("yield", "severity"),
                      block = "rep", tasted = tasted, verbose = FALSE)

  plots <- plot_cooked(cooked)

  expect_true(all(c("yield", "severity") %in% names(plots)))
  expect_s3_class(plots$yield,    "gg")
  expect_s3_class(plots$severity, "gg")
})

test_that("plot_cooked response argument filters to specific response only", {
  skip_if_not_installed("ggplot2")
  set.seed(2526)
  df <- data.frame(
    treatment = rep(c("A", "B", "C"), each = 5),
    rep       = rep(1:5, times = 3),
    yield     = c(rnorm(5, 500, 20), rnorm(5, 700, 20), rnorm(5, 600, 20)),
    severity  = c(rnorm(5, 10, 3),  rnorm(5, 20, 3),  rnorm(5, 15, 3)),
    stringsAsFactors = FALSE
  )
  washed <- wash_rice(df, verbose = FALSE)
  tasted <- taste_rice(washed, response = c("yield", "severity"),
                       block = "rep", mode = "model",
                       plot = FALSE, verbose = FALSE)
  cooked <- cook_rcbd(washed, response = c("yield", "severity"),
                      block = "rep", tasted = tasted, verbose = FALSE)

  plots <- plot_cooked(cooked, response = "yield")

  expect_true("yield" %in% names(plots))
  expect_false("severity" %in% names(plots))
})

# no significant difference (no letters)

test_that("plot_cooked works when p > 0.05 (no group letters)", {
  skip_if_not_installed("ggplot2")
  set.seed(777)
  df <- data.frame(
    treatment = rep(c("X", "Y", "Z"), each = 5),
    rep       = rep(1:5, times = 3),
    yield     = rnorm(15, 600, 30),
    stringsAsFactors = FALSE
  )
  washed <- wash_rice(df, verbose = FALSE)
  tasted <- taste_rice(washed, response = "yield", block = "rep",
                       mode = "model", plot = FALSE, verbose = FALSE)
  cooked <- cook_rcbd(washed, response = "yield", block = "rep",
                      tasted = tasted, verbose = FALSE)

  plots <- plot_cooked(cooked)

  expect_s3_class(plots$yield, "gg")
})

# factorial CRD — grouped bar

test_that("plot_cooked handles factorial CRD (grouped bar)", {
  skip_if_not_installed("ggplot2")
  set.seed(42)
  df <- data.frame(
    variety    = rep(rep(c("RD6", "KDML105"), each = 5), times = 3),
    fertilizer = rep(c("None", "NPK", "Organic"), each = 10),
    yield      = c(rnorm(5, 500, 20), rnorm(5, 700, 20),
                   rnorm(5, 600, 20), rnorm(5, 850, 20),
                   rnorm(5, 550, 20), rnorm(5, 780, 20)),
    stringsAsFactors = FALSE
  )
  washed <- wash_rice(df, verbose = FALSE)
  tasted <- taste_rice(washed, response = "yield", treatment = "variety",
                       mode = "model", plot = FALSE, verbose = FALSE)
  cooked <- cook_crd(washed, response = "yield",
                     factors = c("variety", "fertilizer"),
                     tasted = tasted, verbose = FALSE)

  plots <- plot_cooked(cooked, palette = "set2")

  expect_s3_class(plots$yield, "gg")
})

# Split-plot

test_that("plot_cooked handles split-plot cooked_rice", {
  skip_if_not_installed("ggplot2")
  set.seed(123)
  df <- expand.grid(
    variety  = c("RD6", "KDML105", "PTT1"),
    chemical = c("Control", "Propiconazole", "Tricyclazole"),
    rep      = 1:4,
    stringsAsFactors = FALSE
  )
  df$yield <- with(df, {
    v <- ifelse(variety == "RD6", 600, ifelse(variety == "KDML105", 750, 800))
    ch <- ifelse(chemical == "Propiconazole", 40,
                 ifelse(chemical == "Tricyclazole", 60, 0))
    v + ch + rnorm(nrow(df), 0, 25)
  })
  washed <- wash_rice(df, verbose = FALSE)
  tasted <- taste_rice(washed, response = "yield", treatment = "variety",
                       block = "rep", mode = "model",
                       plot = FALSE, verbose = FALSE)
  cooked <- cook_split(washed, response = "yield",
                       main_plot = "variety", sub_plot = "chemical",
                       block = "rep", tasted = tasted, verbose = FALSE)

  plots <- plot_cooked(cooked)

  expect_s3_class(plots$yield, "gg")
})

# save_path — ตรวจว่าบันทึกไฟล์ได้จริง

test_that("plot_cooked saves PNG file to disk when save_path provided", {
  skip_if_not_installed("ggplot2")
  cooked <- make_rcbd_cooked()

  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp))

  plot_cooked(cooked, save_path = tmp, save_dpi = 72)

  expect_true(file.exists(tmp))
  expect_gt(file.size(tmp), 0)
})
