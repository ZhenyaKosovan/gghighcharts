test_that("faceted plot throws error", {
  p <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) +
    ggplot2::geom_point() +
    ggplot2::facet_wrap(~cyl)
  expect_error(hc_from_ggplot(p), "Faceted plots are not supported")
})

test_that("unsupported geom throws error", {
  df <- data.frame(x = 1:3, y = 1:3, xend = 2:4, yend = 2:4)
  p <- ggplot2::ggplot(df, ggplot2::aes(x, y, xend = xend, yend = yend)) +
    ggplot2::geom_segment()
  expect_error(hc_from_ggplot(p), "Unsupported geom")
})

test_that("non-ggplot input throws error", {
  expect_error(hc_from_ggplot("not a plot"), "must be a ggplot")
  expect_error(hc_from_ggplot(42), "must be a ggplot")
})

test_that("coord_polar throws error", {
  p <- ggplot2::ggplot(mtcars, ggplot2::aes(factor(cyl))) +
    ggplot2::geom_bar() +
    ggplot2::coord_polar()
  expect_error(hc_from_ggplot(p), "Unsupported coordinate system")
})

test_that("empty plot (no layers) throws error", {
  p <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg))
  expect_error(hc_from_ggplot(p), "no layers")
})

test_that("title and subtitle overrides work", {
  p <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) +
    ggplot2::geom_point()
  hc <- hc_from_ggplot(p, title = "Custom Title", subtitle = "Custom Subtitle")

  opts <- hc$x$hc_opts
  expect_equal(opts$title$text, "Custom Title")
  expect_equal(opts$subtitle$text, "Custom Subtitle")
})
