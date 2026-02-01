test_that("line plot: numeric x/y produces line series", {
  df <- data.frame(x = 1:10, y = cumsum(rnorm(10)))
  p <- ggplot2::ggplot(df, ggplot2::aes(x, y)) + ggplot2::geom_line()
  hc <- hc_from_ggplot(p)

  opts <- hc$x$hc_opts
  expect_true(length(opts$series) >= 1)
  expect_equal(opts$series[[1]]$type, "line")
})

test_that("multi-series line by colour", {
  df <- data.frame(
    x = rep(1:5, 2),
    y = c(1:5, 5:1),
    grp = rep(c("A", "B"), each = 5)
  )
  p <- ggplot2::ggplot(df, ggplot2::aes(x, y, colour = grp)) +
    ggplot2::geom_line()
  hc <- hc_from_ggplot(p)

  opts <- hc$x$hc_opts
  expect_equal(length(opts$series), 2)
  expect_equal(opts$series[[1]]$type, "line")
  expect_equal(opts$series[[2]]$type, "line")

  # Series should have names
  names <- vapply(opts$series, function(s) s$name, character(1))
  expect_true(all(names != ""))
})

test_that("line plot: NA in y produces null gap", {
  df <- data.frame(x = 1:5, y = c(1, 2, NA, 4, 5))
  p <- ggplot2::ggplot(df, ggplot2::aes(x, y)) + ggplot2::geom_line()
  hc <- hc_from_ggplot(p)

  series_data <- hc$x$hc_opts$series[[1]]$data
  y_vals <- lapply(series_data, function(pt) pt$y)
  has_null <- any(vapply(y_vals, is.null, logical(1)))
  expect_true(has_null)
})
