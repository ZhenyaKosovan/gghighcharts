test_that("stacked bars use stacking = normal", {
  df <- data.frame(
    x = rep(c("A", "B"), each = 2),
    y = c(10, 20, 15, 25),
    g = rep(c("G1", "G2"), 2)
  )
  p <- ggplot2::ggplot(df, ggplot2::aes(x, y, fill = g)) +
    ggplot2::geom_col(position = "stack")
  hc <- hc_from_ggplot(p)
  opts <- hc$x$hc_opts

  expect_equal(opts$plotOptions$column$stacking, "normal")
  expect_equal(length(opts$series), 2)
})

test_that("fill bars use stacking = percent", {
  df <- data.frame(
    x = rep(c("A", "B"), each = 2),
    y = c(10, 20, 15, 25),
    g = rep(c("G1", "G2"), 2)
  )
  p <- ggplot2::ggplot(df, ggplot2::aes(x, y, fill = g)) +
    ggplot2::geom_col(position = "fill")
  hc <- hc_from_ggplot(p)
  opts <- hc$x$hc_opts

  expect_equal(opts$plotOptions$column$stacking, "percent")
})

test_that("dodged bars have no stacking", {
  df <- data.frame(
    x = rep(c("A", "B"), each = 2),
    y = c(10, 20, 15, 25),
    g = rep(c("G1", "G2"), 2)
  )
  p <- ggplot2::ggplot(df, ggplot2::aes(x, y, fill = g)) +
    ggplot2::geom_col(position = "dodge")
  hc <- hc_from_ggplot(p)
  opts <- hc$x$hc_opts

  expect_null(opts$plotOptions$column$stacking)
  expect_equal(length(opts$series), 2)
})

test_that("coord_cartesian limits are propagated", {
  df <- data.frame(x = 1:5, y = c(10, 20, 30, 40, 50))
  p <- ggplot2::ggplot(df, ggplot2::aes(x, y)) +
    ggplot2::geom_point() +
    ggplot2::coord_cartesian(ylim = c(0, 100))
  hc <- hc_from_ggplot(p)
  opts <- hc$x$hc_opts

  expect_equal(opts$yAxis$min, 0)
  expect_equal(opts$yAxis$max, 100)
})

test_that("coord_flip produces inverted chart", {
  df <- data.frame(item = c("A", "B", "C"), value = c(10, 20, 30))
  p <- ggplot2::ggplot(df, ggplot2::aes(item, value)) +
    ggplot2::geom_col() +
    ggplot2::coord_flip()
  hc <- hc_from_ggplot(p)
  opts <- hc$x$hc_opts

  expect_true(opts$chart$inverted)
  expect_equal(opts$xAxis$type, "category")
})
