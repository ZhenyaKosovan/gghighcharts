test_that("default tooltip has shared = TRUE", {
  p <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) +
    ggplot2::geom_point()
  hc <- hc_from_ggplot(p)
  opts <- hc$x$hc_opts

  expect_true(opts$tooltip$shared)
})

test_that("tooltip_shared = FALSE disables shared tooltip", {
  p <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) +
    ggplot2::geom_point()
  hc <- hc_from_ggplot(p, tooltip_shared = FALSE)
  opts <- hc$x$hc_opts

  expect_false(opts$tooltip$shared)
})

test_that("tooltip_format sets pointFormat", {
  p <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) +
    ggplot2::geom_point()
  fmt <- "<b>{series.name}</b>: {point.y:.1f}"
  hc <- hc_from_ggplot(p, tooltip_format = fmt)
  opts <- hc$x$hc_opts

  expect_equal(opts$tooltip$pointFormat, fmt)
})

test_that("tooltip_formatter sets JS formatter and disables shared", {
  p <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) +
    ggplot2::geom_point()
  js_fn <- htmlwidgets::JS("function() { return this.point.y; }")
  hc <- hc_from_ggplot(p, tooltip_formatter = js_fn)
  opts <- hc$x$hc_opts

  expect_true(inherits(opts$tooltip$formatter, "JS_EVAL"))
  # formatter disables shared tooltip
  expect_false(opts$tooltip$shared)
})

test_that("tooltip_formatter takes precedence over tooltip_format", {
  p <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) +
    ggplot2::geom_point()
  fmt <- "<b>{point.y}</b>"
  js_fn <- htmlwidgets::JS("function() { return 'custom'; }")
  hc <- hc_from_ggplot(p, tooltip_format = fmt, tooltip_formatter = js_fn)
  opts <- hc$x$hc_opts

  # formatter wins
  expect_true(inherits(opts$tooltip$formatter, "JS_EVAL"))
  expect_null(opts$tooltip$pointFormat)
})

test_that("aes(text = ...) produces per-point text property", {
  df <- data.frame(
    x = 1:5,
    y = c(10, 20, 30, 40, 50),
    info = c("low", "medium", "high", "very high", "extreme")
  )
  p <- ggplot2::ggplot(df, ggplot2::aes(x, y, text = info)) +
    ggplot2::geom_point()
  hc <- hc_from_ggplot(p)
  opts <- hc$x$hc_opts

  # All points should carry a text property
  texts <- vapply(opts$series[[1]]$data, function(pt) pt$text %||% NA_character_, character(1))
  expect_true(all(!is.na(texts)))
  expect_true("low" %in% texts)
  expect_true("extreme" %in% texts)
})

test_that("aes(text = ...) auto-sets pointFormat with {point.text}", {
  df <- data.frame(x = 1:3, y = 1:3, info = c("a", "b", "c"))
  p <- ggplot2::ggplot(df, ggplot2::aes(x, y, text = info)) +
    ggplot2::geom_point()
  hc <- hc_from_ggplot(p)
  opts <- hc$x$hc_opts

  expect_true(grepl("point.text", opts$tooltip$pointFormat, fixed = TRUE))
})

test_that("explicit tooltip_format overrides auto text format", {
  df <- data.frame(x = 1:3, y = 1:3, info = c("a", "b", "c"))
  p <- ggplot2::ggplot(df, ggplot2::aes(x, y, text = info)) +
    ggplot2::geom_point()
  custom_fmt <- "<b>{point.y}</b>"
  hc <- hc_from_ggplot(p, tooltip_format = custom_fmt)
  opts <- hc$x$hc_opts

  expect_equal(opts$tooltip$pointFormat, custom_fmt)
})

test_that("text aesthetic does not trigger unsupported warning", {
  df <- data.frame(x = 1:3, y = 1:3, info = c("a", "b", "c"))
  p <- ggplot2::ggplot(df, ggplot2::aes(x, y, text = info)) +
    ggplot2::geom_point()

  expect_no_warning(hc_from_ggplot(p, warn_unsupported = TRUE))
})
