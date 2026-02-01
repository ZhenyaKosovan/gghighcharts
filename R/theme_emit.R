# Translate ggplot2 theme elements to Highcharts options.
# Only a subset of theme elements are translated — the rest are ignored.

#' Extract and translate theme elements from a built ggplot
#' @noRd
extract_theme_opts <- function(b) {
  th <- tryCatch(b$plot$theme, error = function(e) list())
  if (is.null(th) || length(th) == 0) return(list())

  opts <- list()

  # --- chart.backgroundColor from panel.background ---
  bg <- safe_extract_theme(th, "panel.background", "fill")
  if (!is.null(bg)) {
    opts$chart_bg <- normalize_colour(bg)
  }

  # --- plot.background (outer background) ---
  plot_bg <- safe_extract_theme(th, "plot.background", "fill")
  if (!is.null(plot_bg)) {
    opts$plot_bg <- normalize_colour(plot_bg)
  }

  # --- Grid lines ---
  grid_major_colour <- safe_extract_theme(th, "panel.grid.major", "colour")
  grid_major_lw     <- safe_extract_theme(th, "panel.grid.major", "linewidth")
  grid_minor_colour <- safe_extract_theme(th, "panel.grid.minor", "colour")
  grid_minor_lw     <- safe_extract_theme(th, "panel.grid.minor", "linewidth")

  # Check for element_blank (grid disabled)
  grid_major_el <- safe_extract_theme_element(th, "panel.grid.major")
  grid_minor_el <- safe_extract_theme_element(th, "panel.grid.minor")

  opts$grid_major <- list(
    colour = if (is_element_blank(grid_major_el)) NULL else normalize_colour(grid_major_colour),
    width  = if (is_element_blank(grid_major_el)) 0 else if (!is.null(grid_major_lw)) grid_major_lw else NULL
  )
  opts$grid_minor <- list(
    colour = if (is_element_blank(grid_minor_el)) NULL else normalize_colour(grid_minor_colour),
    width  = if (is_element_blank(grid_minor_el)) 0 else if (!is.null(grid_minor_lw)) grid_minor_lw else NULL
  )

  # --- Axis text style ---
  axis_text_colour <- safe_extract_theme(th, "axis.text", "colour")
  axis_text_size   <- safe_extract_theme(th, "axis.text", "size")
  if (!is.null(axis_text_colour) || !is.null(axis_text_size)) {
    style <- list()
    if (!is.null(axis_text_colour)) style$color <- normalize_colour(axis_text_colour)
    if (!is.null(axis_text_size))   style$fontSize <- paste0(axis_text_size, "px")
    opts$axis_text_style <- style
  }

  # --- Title style ---
  title_colour <- safe_extract_theme(th, "plot.title", "colour")
  title_size   <- safe_extract_theme(th, "plot.title", "size")
  title_face   <- safe_extract_theme(th, "plot.title", "face")
  if (!is.null(title_colour) || !is.null(title_size) || !is.null(title_face)) {
    style <- list()
    if (!is.null(title_colour)) style$color <- normalize_colour(title_colour)
    if (!is.null(title_size))   style$fontSize <- paste0(title_size, "px")
    if (!is.null(title_face) && title_face == "bold") style$fontWeight <- "bold"
    opts$title_style <- style
  }

  # --- Global font family ---
  font_family <- safe_extract_theme(th, "text", "family")
  if (!is.null(font_family) && nchar(font_family) > 0) {
    opts$font_family <- font_family
  }

  # --- Legend position ---
  legend_pos <- tryCatch(th$legend.position, error = function(e) NULL)
  if (!is.null(legend_pos) && is.character(legend_pos)) {
    opts$legend_position <- legend_pos
  }

  opts
}

#' Apply theme options to a highcharter object
#' @noRd
apply_theme <- function(hc, theme_opts) {
  if (is.null(theme_opts) || length(theme_opts) == 0) return(hc)

  # Chart background
  chart_args <- list()
  if (!is.null(theme_opts$chart_bg)) {
    chart_args$backgroundColor <- theme_opts$chart_bg
  }
  if (!is.null(theme_opts$plot_bg)) {
    chart_args$plotBackgroundColor <- theme_opts$plot_bg
  }
  if (!is.null(theme_opts$font_family)) {
    chart_args$style <- list(fontFamily = theme_opts$font_family)
  }
  if (length(chart_args) > 0) {
    hc <- do.call(highcharter::hc_chart, c(list(hc = hc), chart_args))
  }

  # Grid lines on axes
  x_grid <- list()
  y_grid <- list()
  if (!is.null(theme_opts$grid_major)) {
    if (!is.null(theme_opts$grid_major$colour))
      y_grid$gridLineColor <- theme_opts$grid_major$colour
    if (!is.null(theme_opts$grid_major$width))
      y_grid$gridLineWidth <- theme_opts$grid_major$width
    if (!is.null(theme_opts$grid_major$colour))
      x_grid$gridLineColor <- theme_opts$grid_major$colour
  }
  if (!is.null(theme_opts$grid_minor)) {
    if (!is.null(theme_opts$grid_minor$colour))
      y_grid$minorGridLineColor <- theme_opts$grid_minor$colour
    if (!is.null(theme_opts$grid_minor$width))
      y_grid$minorGridLineWidth <- theme_opts$grid_minor$width
  }

  # Axis text style
  if (!is.null(theme_opts$axis_text_style)) {
    x_grid$labels <- list(style = theme_opts$axis_text_style)
    y_grid$labels <- list(style = theme_opts$axis_text_style)
  }

  if (length(x_grid) > 0)
    hc <- do.call(highcharter::hc_xAxis, c(list(hc = hc), x_grid))
  if (length(y_grid) > 0)
    hc <- do.call(highcharter::hc_yAxis, c(list(hc = hc), y_grid))

  # Title style
  if (!is.null(theme_opts$title_style)) {
    hc <- highcharter::hc_title(hc, style = theme_opts$title_style)
  }

  # Legend position
  if (!is.null(theme_opts$legend_position)) {
    hc <- apply_legend_position(hc, theme_opts$legend_position)
  }

  hc
}

#' Map ggplot2 legend.position to Highcharts legend options
#' @noRd
apply_legend_position <- function(hc, position) {
  legend_opts <- switch(position,
    "bottom" = list(
      align = "center",
      verticalAlign = "bottom",
      layout = "horizontal"
    ),
    "top" = list(
      align = "center",
      verticalAlign = "top",
      layout = "horizontal"
    ),
    "left" = list(
      align = "left",
      verticalAlign = "middle",
      layout = "vertical"
    ),
    "right" = list(
      align = "right",
      verticalAlign = "middle",
      layout = "vertical"
    ),
    "none" = list(enabled = FALSE),
    NULL
  )

  if (is.null(legend_opts)) return(hc)
  do.call(highcharter::hc_legend, c(list(hc = hc), legend_opts))
}

#' Safely extract a theme element property
#' @noRd
safe_extract_theme <- function(theme, element_name, property) {
  tryCatch({
    el <- theme[[element_name]]
    if (is.null(el)) return(NULL)
    if (is_element_blank(el)) return(NULL)
    val <- el[[property]]
    if (is.null(val) || (is.character(val) && nchar(val) == 0)) return(NULL)
    val
  }, error = function(e) NULL)
}

#' Safely extract the raw theme element (to check for element_blank)
#' @noRd
safe_extract_theme_element <- function(theme, element_name) {
  tryCatch(theme[[element_name]], error = function(e) NULL)
}

#' Check if a theme element is element_blank
#' @noRd
is_element_blank <- function(el) {
  inherits(el, "element_blank")
}

# --- Annotation handling ---

#' Extract annotations from ggplot layers
#'
#' Looks for GeomText, GeomLabel, GeomRect layers that are annotation-like
#' (typically added via annotate()).
#'
#' @param p The ggplot object
#' @param b The built plot
#' @return A list with text_annotations and rect_annotations
#' @noRd
extract_annotations <- function(p, b) {
  texts <- list()
  rects <- list()

  for (i in seq_along(p$layers)) {
    layer <- p$layers[[i]]
    geom_cls <- geom_class_name(layer)
    data <- b$data[[i]]

    if (geom_cls %in% c("GeomText", "GeomLabel")) {
      for (j in seq_len(nrow(data))) {
        row <- data[j, ]
        ann <- list(
          x = row$x,
          y = row$y,
          text = if ("label" %in% names(data)) as.character(row$label) else ""
        )
        colour <- if ("colour" %in% names(data)) row$colour else "black"
        ann$colour <- normalize_colour(colour)
        size <- if ("size" %in% names(data)) row$size else 3.88
        ann$fontSize <- paste0(round(size * 2.83), "px")  # pt to px approx
        texts <- c(texts, list(ann))
      }
    } else if (geom_cls == "GeomRect") {
      for (j in seq_len(nrow(data))) {
        row <- data[j, ]
        band <- list(
          xmin = row$xmin,
          xmax = row$xmax,
          ymin = row$ymin,
          ymax = row$ymax
        )
        if ("fill" %in% names(data) && !is.na(row$fill)) {
          band$colour <- normalize_colour(row$fill)
        }
        if ("alpha" %in% names(data) && !is.na(row$alpha)) {
          band$alpha <- row$alpha
        }
        rects <- c(rects, list(band))
      }
    }
  }

  list(texts = texts, rects = rects)
}

#' Apply annotations to a highcharter object
#' @noRd
apply_annotations <- function(hc, annotations, x_spec) {
  if (is.null(annotations)) return(hc)

  # Text annotations → Highcharts annotations API
  if (length(annotations$texts) > 0) {
    hc_annotations <- list()
    for (ann in annotations$texts) {
      x_val <- ann$x
      if (x_spec$type %in% c("date", "datetime")) {
        x_val <- date_to_ms(x_val)
      }
      hc_ann <- list(
        labels = list(
          list(
            point = list(x = x_val, y = ann$y, xAxis = 0, yAxis = 0),
            text = ann$text,
            style = list(
              color = ann$colour,
              fontSize = ann$fontSize
            ),
            backgroundColor = "transparent",
            borderWidth = 0
          )
        )
      )
      hc_annotations <- c(hc_annotations, list(hc_ann))
    }
    hc <- highcharter::hc_add_dependency(hc, name = "modules/annotations.js")
    hc$x$hc_opts$annotations <- hc_annotations
  }

  # Rect annotations → plotBands on x-axis
  if (length(annotations$rects) > 0) {
    x_bands <- list()
    for (band in annotations$rects) {
      from <- band$xmin
      to <- band$xmax
      if (x_spec$type %in% c("date", "datetime")) {
        from <- date_to_ms(from)
        to <- date_to_ms(to)
      }
      col <- band$colour %||% "#000000"
      alpha <- band$alpha %||% 0.2
      # Convert hex colour + alpha to rgba
      rgb_vals <- grDevices::col2rgb(col)
      rgba <- sprintf("rgba(%d,%d,%d,%.2f)", rgb_vals[1], rgb_vals[2], rgb_vals[3], alpha)

      x_bands <- c(x_bands, list(list(
        from = from,
        to = to,
        color = rgba
      )))
    }
    hc <- highcharter::hc_xAxis(hc, plotBands = x_bands)
  }

  hc
}
