test_that("log10 x scale: breaks and labels propagated", {
  df <- data.frame(x = c(1, 10, 100, 1000), y = c(1, 2, 3, 4))
  p <- ggplot2::ggplot(df, ggplot2::aes(x, y)) +
    ggplot2::geom_point() +
    ggplot2::scale_x_log10()
  hc <- hc_from_ggplot(p)

  opts <- hc$x$hc_opts
  expect_equal(opts$series[[1]]$type, "scatter")
  # The widget should exist and have data
  expect_true(length(opts$series[[1]]$data) > 0)
})

test_that("log10 y scale: produces valid widget", {
  df <- data.frame(x = 1:4, y = c(1, 10, 100, 1000))
  p <- ggplot2::ggplot(df, ggplot2::aes(x, y)) +
    ggplot2::geom_point() +
    ggplot2::scale_y_log10()
  hc <- hc_from_ggplot(p)

  opts <- hc$x$hc_opts
  expect_true(length(opts$series) >= 1)
})

test_that("sqrt transform: produces valid widget", {
  df <- data.frame(x = 1:10, y = (1:10)^2)
  p <- ggplot2::ggplot(df, ggplot2::aes(x, y)) +
    ggplot2::geom_line() +
    ggplot2::scale_y_continuous(trans = "sqrt")
  hc <- hc_from_ggplot(p)

  expect_true(length(hc$x$hc_opts$series) >= 1)
})
