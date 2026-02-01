test_that("date x axis: converted to datetime type", {
  df <- data.frame(
    date = as.Date("2020-01-01") + 0:9,
    value = cumsum(rnorm(10))
  )
  p <- ggplot2::ggplot(df, ggplot2::aes(date, value)) + ggplot2::geom_line()
  hc <- hc_from_ggplot(p)

  opts <- hc$x$hc_opts
  expect_equal(opts$xAxis$type, "datetime")
  expect_equal(opts$series[[1]]$type, "line")

  # Data points should have x in milliseconds (large numbers)
  first_x <- opts$series[[1]]$data[[1]]$x
  expect_true(first_x > 1e12)  # ms since epoch for year 2020
})

test_that("date x axis with date_labels formatting", {
  df <- data.frame(
    date = as.Date("2020-01-01") + seq(0, 365, by = 30),
    value = rnorm(13)
  )
  p <- ggplot2::ggplot(df, ggplot2::aes(date, value)) +
    ggplot2::geom_line() +
    ggplot2::scale_x_date(date_labels = "%b %Y")
  hc <- hc_from_ggplot(p)

  opts <- hc$x$hc_opts
  expect_equal(opts$xAxis$type, "datetime")
  # Should have a label formatter (JS function)
  expect_true(!is.null(opts$xAxis$labels$formatter) ||
              !is.null(opts$xAxis$tickPositions))
})
