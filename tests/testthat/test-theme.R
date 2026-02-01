test_that("theme background colour is translated", {
  p <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) +
    ggplot2::geom_point() +
    ggplot2::theme(panel.background = ggplot2::element_rect(fill = "lightgrey"))
  hc <- hc_from_ggplot(p)
  opts <- hc$x$hc_opts

  expect_false(is.null(opts$chart$backgroundColor))
})

test_that("theme font family is translated", {
  p <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) +
    ggplot2::geom_point() +
    ggplot2::theme(text = ggplot2::element_text(family = "serif"))
  hc <- hc_from_ggplot(p)
  opts <- hc$x$hc_opts

  expect_equal(opts$chart$style$fontFamily, "serif")
})

test_that("title style is translated", {
  p <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) +
    ggplot2::geom_point() +
    ggplot2::labs(title = "Test") +
    ggplot2::theme(plot.title = ggplot2::element_text(
      colour = "red", size = 20, face = "bold"
    ))
  hc <- hc_from_ggplot(p)
  opts <- hc$x$hc_opts

  expect_equal(opts$title$style$color, "#FF0000")
  expect_equal(opts$title$style$fontSize, "20px")
  expect_equal(opts$title$style$fontWeight, "bold")
})

test_that("legend position bottom is mapped", {
  p <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg, colour = factor(cyl))) +
    ggplot2::geom_point() +
    ggplot2::theme(legend.position = "bottom")
  hc <- hc_from_ggplot(p)
  opts <- hc$x$hc_opts

  expect_equal(opts$legend$align, "center")
  expect_equal(opts$legend$verticalAlign, "bottom")
  expect_equal(opts$legend$layout, "horizontal")
})

test_that("legend position none disables legend", {
  p <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg, colour = factor(cyl))) +
    ggplot2::geom_point() +
    ggplot2::theme(legend.position = "none")
  hc <- hc_from_ggplot(p)
  opts <- hc$x$hc_opts

  expect_false(opts$legend$enabled)
})

test_that("element_blank grid lines produce zero width", {
  p <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) +
    ggplot2::geom_point() +
    ggplot2::theme(
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank()
    )
  hc <- hc_from_ggplot(p)
  opts <- hc$x$hc_opts

  expect_equal(opts$yAxis$gridLineWidth, 0)
})

test_that("text annotation is translated", {
  df <- data.frame(x = 1:5, y = 1:5)
  p <- ggplot2::ggplot(df, ggplot2::aes(x, y)) +
    ggplot2::geom_point() +
    ggplot2::annotate("text", x = 3, y = 4, label = "Hello")
  hc <- hc_from_ggplot(p)
  opts <- hc$x$hc_opts

  # Only the scatter series, not the text
  expect_equal(length(opts$series), 1)
  expect_equal(opts$series[[1]]$type, "scatter")

  # Annotation should exist
  expect_true(length(opts$annotations) >= 1)
  expect_equal(opts$annotations[[1]]$labels[[1]]$text, "Hello")
})

test_that("rect annotation produces plotBand", {
  df <- data.frame(x = 1:5, y = 1:5)
  p <- ggplot2::ggplot(df, ggplot2::aes(x, y)) +
    ggplot2::geom_point() +
    ggplot2::annotate("rect", xmin = 2, xmax = 4, ymin = 1, ymax = 5,
                      alpha = 0.2, fill = "blue")
  hc <- hc_from_ggplot(p)
  opts <- hc$x$hc_opts

  expect_equal(length(opts$series), 1)
  expect_true(length(opts$xAxis$plotBands) >= 1)
})
