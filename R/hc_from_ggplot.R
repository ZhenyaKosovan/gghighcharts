#' Convert a ggplot2 plot to a Highcharts widget
#'
#' Translates a restricted subset of ggplot2 plots into interactive
#' highcharter widgets. Works by inspecting the built plot
#' (via [ggplot2::ggplot_build()]) rather than parsing source code.
#'
#' @section Supported chart types:
#' \itemize{
#'   \item **Line charts**: `geom_line()`, `geom_step()`
#'   \item **Scatter plots**: `geom_point()`
#'   \item **Bar/Column charts**: `geom_col()`, `geom_bar()`, `geom_histogram()`
#'   \item **Area charts**: `geom_area()`, `geom_ribbon()`
#'   \item **Statistical**: `geom_smooth()`, `geom_boxplot()`, `geom_errorbar()`
#'   \item **Reference lines**: `geom_hline()`, `geom_vline()`
#' }
#'
#' @section Supported axes:
#' \itemize{
#'   \item X axis: discrete (categorical), numeric, Date, POSIXct (datetime)
#'   \item Y axis: numeric (continuous)
#' }
#'
#' @section Unsupported features (will error):
#' \itemize{
#'   \item Facets (`facet_wrap()`, `facet_grid()`)
#'   \item Non-cartesian coordinates (`coord_polar()`, `coord_sf()`, etc.)
#'   \item Unsupported geoms (`geom_text()`, `geom_tile()`,
#'     `geom_violin()`, `geom_density()`, etc.)
#' }
#'
#' @param p A ggplot object.
#' @param title Optional character string to override the plot title.
#' @param subtitle Optional character string to override the plot subtitle.
#' @param tooltip_shared Logical; if `TRUE` (default), uses shared tooltip
#'   so all series values are shown at the same x position.
#' @param tooltip_format Optional character string for Highcharts
#'   `tooltip.pointFormat`. Overrides the default tooltip format. Use
#'   Highcharts template variables like `{point.x}`, `{point.y}`,
#'   `{series.name}`, and `{point.text}` (for custom text from
#'   `aes(text = ...)`).
#' @param tooltip_formatter Optional [htmlwidgets::JS()] function for
#'   complete tooltip control. Takes precedence over `tooltip_format`. The
#'   function receives the standard Highcharts tooltip context (`this.point`,
#'   `this.series`, etc.).
#' @param animation Controls chart animation. `TRUE` (default) enables
#'   default Highcharts animation. `FALSE` disables all animation. A numeric
#'   value sets the animation duration in milliseconds.
#' @param warn_unsupported Logical; if `TRUE` (default), emits warnings when
#'   unsupported aesthetics or features are silently dropped.
#' @param ... Reserved for future use.
#'
#' @return A [highcharter::highchart()] widget that can be printed,
#'   saved, or included in Shiny apps and R Markdown documents.
#'
#' @examples
#' \dontrun{
#' library(ggplot2)
#' library(gghighcharts)
#'
#' # Simple scatter plot
#' p <- ggplot(mtcars, aes(wt, mpg)) + geom_point()
#' hc_from_ggplot(p)
#'
#' # Multi-series line chart
#' p <- ggplot(economics_long, aes(date, value01, colour = variable)) +
#'   geom_line()
#' hc_from_ggplot(p, title = "US Economic Indicators")
#'
#' # Bar chart
#' p <- ggplot(mpg, aes(class, fill = class)) +
#'   geom_bar(stat = "count")
#' # Note: stat = "count" is NOT supported; use pre-aggregated data:
#' df <- as.data.frame(table(mpg$class))
#' p <- ggplot(df, aes(Var1, Freq)) + geom_col()
#' hc_from_ggplot(p)
#' }
#'
#' @export
hc_from_ggplot <- function(p, title = NULL, subtitle = NULL,
                           tooltip_shared = TRUE,
                           tooltip_format = NULL,
                           tooltip_formatter = NULL,
                           animation = TRUE,
                           warn_unsupported = TRUE, ...) {
  if (!inherits(p, "ggplot")) {
    rlang::abort("Input must be a ggplot object.", class = "gghighcharts_error")
  }

  # Build the plot
  b <- ggplot2::ggplot_build(p)

  # Build internal spec
  spec <- build_spec(p, b,
                     title = title,
                     subtitle = subtitle,
                     warn_unsupported = warn_unsupported)

  # Emit highcharter widget
  emit_highchart(spec,
                 tooltip_shared = tooltip_shared,
                 tooltip_format = tooltip_format,
                 tooltip_formatter = tooltip_formatter,
                 animation = animation)
}

#' Convert a ggplot to a highchart (S3 method)
#'
#' S3 method allowing `as_highchart(p)` syntax for ggplot objects.
#'
#' @param x A ggplot object
#' @param ... Additional arguments passed to [hc_from_ggplot()]
#' @return A highcharter widget
#'
#' @examples
#' \dontrun{
#' library(ggplot2)
#' p <- ggplot(mtcars, aes(wt, mpg)) + geom_point()
#' as_highchart(p)
#' }
#'
#' @export
as_highchart.ggplot <- function(x, ...) {
  hc_from_ggplot(x, ...)
}

#' Generic for as_highchart
#'
#' @param x An object to convert to a highchart
#' @param ... Additional arguments
#' @return A highcharter widget
#' @export
as_highchart <- function(x, ...) {
  UseMethod("as_highchart")
}
