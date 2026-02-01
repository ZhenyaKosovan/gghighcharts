#' Convert a normalized spec into a highcharter widget
#'
#' Takes the internal spec produced by build_spec() and creates
#' a highcharter widget with axes, series, title, and tooltip.
#'
#' @param spec The normalized spec from build_spec()
#' @param tooltip_shared Logical; use shared tooltip
#' @param tooltip_format Optional pointFormat string
#' @param tooltip_formatter Optional JS formatter function
#' @param animation Animation control: TRUE, FALSE, or numeric ms
#' @return A highcharter widget
#' @noRd
emit_highchart <- function(spec, tooltip_shared = TRUE,
                           tooltip_format = NULL,
                           tooltip_formatter = NULL,
                           animation = TRUE) {
  hc <- highcharter::highchart()

  # --- Chart-level options (e.g., inverted for coord_flip) ---
  if (isTRUE(spec$flipped)) {
    hc <- highcharter::hc_chart(hc, inverted = TRUE)
  }

  # --- Title / subtitle ---
  if (!is.null(spec$meta$title)) {
    hc <- highcharter::hc_title(hc, text = spec$meta$title)
  }
  if (!is.null(spec$meta$subtitle)) {
    hc <- highcharter::hc_subtitle(hc, text = spec$meta$subtitle)
  }

  # --- X axis ---
  hc <- apply_xaxis(hc, spec$x)

  # --- Y axis ---
  hc <- apply_yaxis(hc, spec$y)

  # --- Series and plotLines ---
  all_series <- list()
  x_plotlines <- list()
  y_plotlines <- list()

  for (layer_spec in spec$layers) {
    layer_series <- emit_series_from_layer(layer_spec, spec$x, spec$y,
                                            stacking = spec$stacking)
    for (s in layer_series) {
      if (!is.null(s$.plotlines)) {
        # This is a plotlines result, not a series
        for (pl in s$.plotlines) {
          if (identical(pl$axis, "x")) {
            pl$axis <- NULL
            x_plotlines <- c(x_plotlines, list(pl))
          } else if (identical(pl$axis, "y")) {
            pl$axis <- NULL
            y_plotlines <- c(y_plotlines, list(pl))
          }
        }
      } else {
        all_series <- c(all_series, list(s))
      }
    }
  }

  # Add regular series
  for (s in all_series) {
    # Build args dynamically — passing NULL values causes Highcharts to
    # receive JSON null which can disable features.
    args <- list(hc = hc, type = s$type, name = s$name, data = s$data)
    if (!is.null(s$color))       args$color       <- s$color
    if (!is.null(s$marker))      args$marker      <- s$marker
    if (!is.null(s$lineWidth))   args$lineWidth   <- s$lineWidth
    if (!is.null(s$dashStyle))   args$dashStyle   <- s$dashStyle
    if (!is.null(s$opacity))     args$opacity      <- s$opacity
    if (!is.null(s$step))        args$step         <- s$step
    if (!is.null(s$linkedTo))    args$linkedTo     <- s$linkedTo
    if (!is.null(s$fillOpacity)) args$fillOpacity  <- s$fillOpacity
    if (!is.null(s$pointRange))  args$pointRange   <- s$pointRange
    hc <- do.call(highcharter::hc_add_series, args)
  }

  # Apply plotLines to axes
  if (length(x_plotlines) > 0) {
    hc <- highcharter::hc_xAxis(hc, plotLines = x_plotlines)
  }
  if (length(y_plotlines) > 0) {
    hc <- highcharter::hc_yAxis(hc, plotLines = y_plotlines)
  }

  # --- Tooltip ---
  tooltip_opts <- list(shared = tooltip_shared)
  if (!is.null(tooltip_formatter)) {
    # Full JS formatter takes precedence
    tooltip_opts$formatter <- tooltip_formatter
    tooltip_opts$shared <- FALSE
  } else if (!is.null(tooltip_format)) {
    tooltip_opts$pointFormat <- tooltip_format
  } else if (isTRUE(spec$has_text_aes)) {
    # Auto-include {point.text} when aes(text = ...) is present
    tooltip_opts$pointFormat <- paste0(
      "<span style=\"color:{point.color}\">\\u25CF</span> ",
      "{series.name}: <b>{point.y}</b><br/>{point.text}"
    )
  }
  hc <- do.call(highcharter::hc_tooltip, c(list(hc = hc), tooltip_opts))

  # --- Plot options ---
  plot_opts <- list()

  # Scatter: disable line connections
  has_scatter <- any(vapply(all_series, function(s) s$type == "scatter", logical(1)))
  if (has_scatter) {
    plot_opts$scatter <- list(
      marker = list(enabled = TRUE),
      states = list(hover = list(enabled = TRUE))
    )
  }

  # Stacking for column/bar
  if (!is.null(spec$stacking)) {
    plot_opts$column <- list(stacking = spec$stacking)
  }

  # Animation control
  anim_val <- resolve_animation(animation)
  if (!is.null(anim_val)) {
    plot_opts$series <- c(plot_opts$series %||% list(), list(animation = anim_val))
  }

  if (length(plot_opts) > 0) {
    hc <- do.call(highcharter::hc_plotOptions, c(list(hc = hc), plot_opts))
  }

  # --- Axis limits from coord_cartesian ---
  if (!is.null(spec$axis_limits)) {
    if (!is.null(spec$axis_limits$x)) {
      hc <- highcharter::hc_xAxis(hc,
        min = spec$axis_limits$x[1],
        max = spec$axis_limits$x[2]
      )
    }
    if (!is.null(spec$axis_limits$y)) {
      hc <- highcharter::hc_yAxis(hc,
        min = spec$axis_limits$y[1],
        max = spec$axis_limits$y[2]
      )
    }
  }

  # --- Theme ---
  hc <- apply_theme(hc, spec$theme_opts)

  # --- Annotations ---
  hc <- apply_annotations(hc, spec$annotations, spec$x)

  hc
}

#' Apply x-axis configuration to a highcharter object
#'
#' @param hc A highcharter object
#' @param x_spec The x-axis spec
#' @return Modified highcharter object
#' @noRd
apply_xaxis <- function(hc, x_spec) {
  opts <- list()

  if (x_spec$type == "discrete") {
    opts$type <- "category"
    if (!is.null(x_spec$categories)) {
      opts$categories <- as.list(x_spec$categories)
    }
  } else if (x_spec$type %in% c("date", "datetime")) {
    opts$type <- "datetime"

    # Use label map for tick formatting
    if (!is.null(x_spec$label_map)) {
      label_map_json <- jsonlite::toJSON(as.list(x_spec$label_map), auto_unbox = TRUE)
      opts$labels <- list(
        formatter = htmlwidgets::JS(sprintf(
          "function() { var m = %s; return m[String(this.value)] || this.value; }",
          label_map_json
        ))
      )
      # Set explicit tick positions
      if (!is.null(x_spec$breaks_ms)) {
        opts$tickPositions <- as.list(x_spec$breaks_ms)
      }
    }
  } else {
    # Numeric axis
    if (!is.null(x_spec$label_map)) {
      label_map_json <- jsonlite::toJSON(as.list(x_spec$label_map), auto_unbox = TRUE)
      opts$labels <- list(
        formatter = htmlwidgets::JS(sprintf(
          "function() { var m = %s; return m[String(this.value)] || this.value; }",
          label_map_json
        ))
      )
      if (!is.null(x_spec$breaks)) {
        opts$tickPositions <- as.list(x_spec$breaks)
      }
    }
  }

  # Axis title
  if (!is.null(x_spec$title)) {
    opts$title <- list(text = x_spec$title)
  }

  if (length(opts) == 0) return(hc)
  do.call(highcharter::hc_xAxis, c(list(hc = hc), opts))
}

#' Apply y-axis configuration to a highcharter object
#'
#' @param hc A highcharter object
#' @param y_spec The y-axis spec
#' @return Modified highcharter object
#' @noRd
apply_yaxis <- function(hc, y_spec) {
  opts <- list()

  if (!is.null(y_spec$label_map)) {
    label_map_json <- jsonlite::toJSON(as.list(y_spec$label_map), auto_unbox = TRUE)
    opts$labels <- list(
      formatter = htmlwidgets::JS(sprintf(
        "function() { var m = %s; return m[String(this.value)] || this.value; }",
        label_map_json
      ))
    )
    if (!is.null(y_spec$breaks)) {
      opts$tickPositions <- as.list(y_spec$breaks)
    }
  }

  # Axis title
  if (!is.null(y_spec$title)) {
    opts$title <- list(text = y_spec$title)
  }

  if (length(opts) == 0) return(hc)
  do.call(highcharter::hc_yAxis, c(list(hc = hc), opts))
}
