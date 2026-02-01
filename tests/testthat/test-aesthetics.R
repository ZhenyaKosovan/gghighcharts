test_that("linetype is mapped to dashStyle", {
  df <- data.frame(x = 1:5, y = 1:5)
  p <- ggplot2::ggplot(df, ggplot2::aes(x, y)) +
    ggplot2::geom_line(linetype = "dashed")
  hc <- hc_from_ggplot(p)
  opts <- hc$x$hc_opts

  expect_equal(opts$series[[1]]$dashStyle, "Dash")
})

test_that("alpha transparency is mapped to opacity", {
  df <- data.frame(x = 1:5, y = 1:5)
  p <- ggplot2::ggplot(df, ggplot2::aes(x, y)) +
    ggplot2::geom_point(alpha = 0.5)
  hc <- hc_from_ggplot(p)
  opts <- hc$x$hc_opts

  expect_equal(opts$series[[1]]$opacity, 0.5)
})

test_that("point shape is mapped to marker symbol", {
  df <- data.frame(x = 1:5, y = 1:5)
  p <- ggplot2::ggplot(df, ggplot2::aes(x, y)) +
    ggplot2::geom_point(shape = 0)  # square
  hc <- hc_from_ggplot(p)
  opts <- hc$x$hc_opts

  expect_equal(opts$series[[1]]$marker$symbol, "square")
})

test_that("colour aes produces coloured series", {
  df <- data.frame(
    x = rep(1:5, 2),
    y = c(1:5, 5:1),
    g = rep(c("A", "B"), each = 5)
  )
  p <- ggplot2::ggplot(df, ggplot2::aes(x, y, colour = g)) +
    ggplot2::geom_line()
  hc <- hc_from_ggplot(p)
  opts <- hc$x$hc_opts

  expect_equal(length(opts$series), 2)
  # Each series should have a colour
  for (s in opts$series) {
    expect_false(is.null(s$color))
  }
})
