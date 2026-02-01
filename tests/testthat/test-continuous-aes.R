test_that("continuous colour produces per-point colours", {
  p <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg, colour = hp)) +
    ggplot2::geom_point()
  hc <- hc_from_ggplot(p)
  opts <- hc$x$hc_opts

  # Should be a single series (not one per unique colour)
  expect_equal(length(opts$series), 1)
  expect_equal(opts$series[[1]]$type, "scatter")

  # Each point should have a color property
  pt <- opts$series[[1]]$data[[1]]
  expect_false(is.null(pt$color))
})

test_that("continuous size produces per-point marker radius", {
  p <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg, size = hp)) +
    ggplot2::geom_point()
  hc <- hc_from_ggplot(p)
  opts <- hc$x$hc_opts

  expect_equal(length(opts$series), 1)

  # Each point should have a marker with radius
  pt <- opts$series[[1]]$data[[1]]
  expect_false(is.null(pt$marker))
  expect_true(is.numeric(pt$marker$radius))
})

test_that("both continuous colour and size work together", {
  p <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg, colour = hp, size = disp)) +
    ggplot2::geom_point()
  hc <- hc_from_ggplot(p)
  opts <- hc$x$hc_opts

  expect_equal(length(opts$series), 1)
  pt <- opts$series[[1]]$data[[1]]
  expect_false(is.null(pt$color))
  expect_false(is.null(pt$marker))
})

test_that("discrete colour still groups into separate series", {
  p <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg, colour = factor(cyl))) +
    ggplot2::geom_point()
  hc <- hc_from_ggplot(p)
  opts <- hc$x$hc_opts

  expect_equal(length(opts$series), 3)
  # Points should NOT have per-point color
  pt <- opts$series[[1]]$data[[1]]
  expect_null(pt$color)
})
