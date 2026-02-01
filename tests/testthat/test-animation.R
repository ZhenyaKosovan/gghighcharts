test_that("animation = TRUE (default) does not set animation option", {
  p <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) +
    ggplot2::geom_point()
  hc <- hc_from_ggplot(p)
  opts <- hc$x$hc_opts

  # Default animation: no explicit plotOptions.series.animation set
  # (Highcharts uses its own default which is TRUE)
  series_opts <- opts$plotOptions$series
  expect_true(is.null(series_opts$animation) || isTRUE(series_opts$animation))
})

test_that("animation = FALSE disables animation", {
  p <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) +
    ggplot2::geom_point()
  hc <- hc_from_ggplot(p, animation = FALSE)
  opts <- hc$x$hc_opts

  expect_false(opts$plotOptions$series$animation)
})

test_that("animation = numeric sets duration", {
  p <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) +
    ggplot2::geom_point()
  hc <- hc_from_ggplot(p, animation = 2000)
  opts <- hc$x$hc_opts

  expect_equal(opts$plotOptions$series$animation$duration, 2000)
})

test_that("resolve_animation helper works correctly", {
  expect_null(gghighcharts:::resolve_animation(TRUE))
  expect_null(gghighcharts:::resolve_animation(NULL))
  expect_false(gghighcharts:::resolve_animation(FALSE))
  expect_equal(gghighcharts:::resolve_animation(500), list(duration = 500L))
})
