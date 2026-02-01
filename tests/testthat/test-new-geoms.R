test_that("geom_area produces area series", {
  df <- data.frame(x = 1:5, y = c(3, 5, 2, 4, 6))
  p <- ggplot2::ggplot(df, ggplot2::aes(x, y)) + ggplot2::geom_area()
  hc <- hc_from_ggplot(p)
  opts <- hc$x$hc_opts

  expect_equal(opts$series[[1]]$type, "area")
  expect_equal(length(opts$series[[1]]$data), 5)
})

test_that("geom_step produces line series with step", {
  df <- data.frame(x = 1:5, y = c(3, 5, 2, 4, 6))
  p <- ggplot2::ggplot(df, ggplot2::aes(x, y)) + ggplot2::geom_step()
  hc <- hc_from_ggplot(p)
  opts <- hc$x$hc_opts

  expect_equal(opts$series[[1]]$type, "line")
  expect_equal(opts$series[[1]]$step, "left")
})

test_that("geom_smooth produces line + arearange", {
  p <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) +
    ggplot2::geom_smooth()
  suppressMessages({
    hc <- hc_from_ggplot(p)
  })
  opts <- hc$x$hc_opts

  types <- vapply(opts$series, function(s) s$type, character(1))
  expect_true("line" %in% types)
  expect_true("arearange" %in% types)
})

test_that("geom_hline adds plotLine on y-axis", {
  df <- data.frame(x = 1:5, y = 1:5)
  p <- ggplot2::ggplot(df, ggplot2::aes(x, y)) +
    ggplot2::geom_point() +
    ggplot2::geom_hline(yintercept = 3)
  hc <- hc_from_ggplot(p)
  opts <- hc$x$hc_opts

  expect_true(length(opts$yAxis$plotLines) >= 1)
  expect_equal(opts$yAxis$plotLines[[1]]$value, 3)
})

test_that("geom_vline adds plotLine on x-axis", {
  df <- data.frame(x = 1:5, y = 1:5)
  p <- ggplot2::ggplot(df, ggplot2::aes(x, y)) +
    ggplot2::geom_point() +
    ggplot2::geom_vline(xintercept = 3)
  hc <- hc_from_ggplot(p)
  opts <- hc$x$hc_opts

  expect_true(length(opts$xAxis$plotLines) >= 1)
  expect_equal(opts$xAxis$plotLines[[1]]$value, 3)
})

test_that("geom_histogram produces column series", {
  p <- ggplot2::ggplot(mtcars, ggplot2::aes(mpg)) +
    ggplot2::geom_histogram(bins = 10)
  hc <- hc_from_ggplot(p)
  opts <- hc$x$hc_opts

  expect_equal(opts$series[[1]]$type, "column")
  expect_true(length(opts$series[[1]]$data) > 0)
})

test_that("geom_bar stat count works", {
  p <- ggplot2::ggplot(mtcars, ggplot2::aes(factor(cyl))) +
    ggplot2::geom_bar()
  hc <- hc_from_ggplot(p)
  opts <- hc$x$hc_opts

  expect_equal(opts$series[[1]]$type, "column")
  expect_equal(opts$xAxis$type, "category")
})

test_that("geom_boxplot produces boxplot series", {
  p <- ggplot2::ggplot(mtcars, ggplot2::aes(factor(cyl), mpg)) +
    ggplot2::geom_boxplot()
  hc <- hc_from_ggplot(p)
  opts <- hc$x$hc_opts

  expect_equal(opts$series[[1]]$type, "boxplot")
  expect_equal(length(opts$series[[1]]$data), 3)  # 3 cylinder groups
  # Check data has correct structure
  bp <- opts$series[[1]]$data[[1]]
  expect_true(all(c("low", "q1", "median", "q3", "high") %in% names(bp)))
})

test_that("geom_errorbar produces errorbar series", {
  df <- data.frame(
    x = c("A", "B", "C"), y = c(10, 20, 15),
    ymin = c(8, 17, 12), ymax = c(12, 23, 18)
  )
  p <- ggplot2::ggplot(df, ggplot2::aes(x, y)) +
    ggplot2::geom_col() +
    ggplot2::geom_errorbar(ggplot2::aes(ymin = ymin, ymax = ymax))
  hc <- hc_from_ggplot(p)
  opts <- hc$x$hc_opts

  types <- vapply(opts$series, function(s) s$type, character(1))
  expect_true("column" %in% types)
  expect_true("errorbar" %in% types)
})

test_that("geom_point + geom_smooth combined plot works", {
  p <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) +
    ggplot2::geom_point() +
    ggplot2::geom_smooth(se = FALSE)
  suppressMessages({
    hc <- hc_from_ggplot(p)
  })
  opts <- hc$x$hc_opts

  types <- vapply(opts$series, function(s) s$type, character(1))
  expect_true("scatter" %in% types)
  expect_true("line" %in% types)
})
