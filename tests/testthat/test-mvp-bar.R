test_that("bar chart: discrete x produces column series with categories", {
  df <- data.frame(
    category = c("A", "B", "C"),
    value = c(10, 20, 30)
  )
  p <- ggplot2::ggplot(df, ggplot2::aes(category, value)) +
    ggplot2::geom_col()
  hc <- hc_from_ggplot(p)

  opts <- hc$x$hc_opts
  expect_equal(opts$series[[1]]$type, "column")

  # Should have categories on x axis
  x_axis <- opts$xAxis
  expect_true(!is.null(x_axis$categories) || x_axis$type == "category")
})

test_that("grouped bar chart: multiple series", {
  df <- data.frame(
    category = rep(c("A", "B", "C"), 2),
    value = c(10, 20, 30, 15, 25, 35),
    group = rep(c("X", "Y"), each = 3)
  )
  p <- ggplot2::ggplot(df, ggplot2::aes(category, value, fill = group)) +
    ggplot2::geom_col(position = "dodge")
  hc <- hc_from_ggplot(p)

  opts <- hc$x$hc_opts
  expect_equal(length(opts$series), 2)
})
