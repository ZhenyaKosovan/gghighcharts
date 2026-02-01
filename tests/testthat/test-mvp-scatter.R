test_that("scatter plot: numeric x/y produces scatter series", {
  p <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) +
    ggplot2::geom_point()
  hc <- hc_from_ggplot(p)

  opts <- hc$x$hc_opts
  expect_true(length(opts$series) >= 1)
  expect_equal(opts$series[[1]]$type, "scatter")
  expect_true(length(opts$series[[1]]$data) > 0)
})

test_that("scatter plot: data points match mtcars", {
  p <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) +
    ggplot2::geom_point()
  hc <- hc_from_ggplot(p)

  series_data <- hc$x$hc_opts$series[[1]]$data
  expect_equal(length(series_data), nrow(mtcars))

  # Extract x values and check they contain mtcars$wt values
  x_vals <- vapply(series_data, function(pt) pt$x, numeric(1))
  expect_true(all(sort(x_vals) == sort(mtcars$wt)))
})

test_that("scatter plot: NA in y produces null gap", {
  df <- data.frame(x = 1:5, y = c(1, 2, NA, 4, 5))
  p <- ggplot2::ggplot(df, ggplot2::aes(x, y)) + ggplot2::geom_point()
  hc <- hc_from_ggplot(p)

  series_data <- hc$x$hc_opts$series[[1]]$data
  # The NA point should have y = NULL
  y_vals <- lapply(series_data, function(pt) pt$y)
  has_null <- any(vapply(y_vals, is.null, logical(1)))
  expect_true(has_null)
})
