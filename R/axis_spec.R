#' Extract and normalize panel parameters across ggplot2 versions
#'
#' @param b Result of ggplot_build()
#' @return A list with x_breaks, x_labels, y_breaks, y_labels, and raw params
#' @noRd
get_panel_params <- function(b) {
  params <- b$layout$panel_params[[1]]

  x_info <- extract_axis_info(params, "x")
  y_info <- extract_axis_info(params, "y")

  list(
    raw        = params,
    x_breaks   = x_info$breaks,
    x_labels   = x_info$labels,
    x_range    = x_info$range,
    y_breaks   = y_info$breaks,
    y_labels   = y_info$labels,
    y_range    = y_info$range
  )
}

#' Extract breaks, labels, and range for a single axis
#'
#' Handles both older (ggplot2 < 3.5) and newer panel_params structures.
#'
#' @param params Panel params from layout
#' @param axis "x" or "y"
#' @return List with breaks, labels, range
#' @noRd
extract_axis_info <- function(params, axis = "x") {
  breaks <- NULL
  labels <- NULL
  range <- NULL

  # Try newer ggplot2 structure: params$x$breaks / params$x$get_labels()
  ax <- params[[axis]]

  if (!is.null(ax)) {
    # Newer ggplot2 (>= 3.3): axis object with breaks and labels
    if (is.list(ax) || is.environment(ax)) {
      # Use get_breaks() or the breaks field for data-space values.
      # Do NOT use break_positions() — it returns rescaled 0-1 values.
      breaks <- tryCatch(ax$get_breaks(), error = function(e) NULL)
      if (is.null(breaks)) {
        breaks <- safe_extract(ax, "breaks")
      }
      if (is.null(breaks)) {
        breaks <- tryCatch(ax$mapped_breaks(), error = function(e) NULL)
      }

      labels <- tryCatch(ax$get_labels(), error = function(e) NULL)
      if (is.null(labels)) {
        labels <- safe_extract(ax, "labels")
      }

      range <- safe_extract(ax, "continuous_range")
      if (is.null(range)) {
        range <- tryCatch(ax$range$range, error = function(e) NULL)
      }
    }
  }

  # Fallback: older flat structure
  if (is.null(breaks)) {
    breaks <- params[[paste0(axis, ".major_source")]]
    if (is.null(breaks)) {
      breaks <- params[[paste0(axis, ".major")]]
    }
  }

  if (is.null(labels)) {
    labels <- params[[paste0(axis, ".labels")]]
    if (is.null(labels) && !is.null(breaks)) {
      labels <- as.character(breaks)
    }
  }

  if (is.null(range)) {
    range <- params[[paste0(axis, ".range")]]
  }

  # Clean: remove NAs from breaks/labels pairs
  if (!is.null(breaks) && !is.null(labels)) {
    valid <- !is.na(breaks)
    breaks <- breaks[valid]
    labels <- labels[valid]
  }

  list(breaks = breaks, labels = labels, range = range)
}

#' Infer the type of an axis from the plot and built data
#'
#' @param p The ggplot object
#' @param b The built plot
#' @param axis "x" or "y"
#' @return One of "discrete", "numeric", "date", "datetime"
#' @noRd
infer_axis_type <- function(p, b, axis = "x") {
  # First: check panel params for discrete flag (most reliable)
  params <- b$layout$panel_params[[1]]
  ax <- params[[axis]]
  if (!is.null(ax)) {
    is_disc <- tryCatch(ax$scale_is_discrete, error = function(e) NULL)
    if (isTRUE(is_disc)) return("discrete")
    is_disc2 <- tryCatch(ax$is_discrete(), error = function(e) NULL)
    if (isTRUE(is_disc2)) return("discrete")
    if (!is.null(safe_extract(ax, "range", "levels"))) return("discrete")
  }

  # Try from explicit user scale

  scale <- NULL
  tryCatch({
    scales_list <- p$scales$scales
    for (s in scales_list) {
      if (axis %in% s$aesthetics) {
        scale <- s
        break
      }
    }
  }, error = function(e) NULL)

  if (!is.null(scale)) {
    if (is_discrete_scale(scale)) return("discrete")
    if (is_date_scale(scale)) {
      if (inherits(scale, "ScaleContinuousDatetime")) return("datetime")
      return("date")
    }
    return("numeric")
  }

  # Check panel params scale for date type
  if (!is.null(ax)) {
    sc <- tryCatch(ax$scale, error = function(e) NULL)
    if (!is.null(sc) && is_date_scale(sc)) {
      if (inherits(sc, "ScaleContinuousDatetime")) return("datetime")
      return("date")
    }
  }

  # Fallback: inspect built data
  if (length(b$data) > 0) {
    col <- if (axis == "x") "x" else "y"
    d <- b$data[[1]]
    if (col %in% names(d)) {
      vals <- d[[col]]
      if (is.factor(vals) || is.character(vals)) return("discrete")
      if (inherits(vals, "POSIXct")) return("datetime")
      if (inherits(vals, "Date")) return("date")
    }
  }

  "numeric"
}

#' Build axis specification for Highcharts
#'
#' @param p The ggplot object
#' @param b The built plot
#' @param panel_params Result of get_panel_params()
#' @param axis "x" or "y"
#' @return A list with type, breaks, labels, categories, etc.
#' @noRd
build_axis_spec <- function(p, b, panel_params, axis = "x") {
  ax_type <- infer_axis_type(p, b, axis)

  breaks_key <- paste0(axis, "_breaks")
  labels_key <- paste0(axis, "_labels")

  breaks <- panel_params[[breaks_key]]
  labels <- panel_params[[labels_key]]

  spec <- list(type = ax_type, breaks = breaks, labels = labels)

  if (ax_type == "discrete") {
    # Extract categories in order
    categories <- extract_categories(p, b, axis)
    spec$categories <- categories
  } else if (ax_type %in% c("date", "datetime")) {
    # Convert breaks to ms for Highcharts
    if (!is.null(breaks)) {
      spec$breaks_ms <- date_to_ms(breaks)
    }
    # Build label map for tick formatting
    if (!is.null(breaks) && !is.null(labels)) {
      spec$label_map <- stats::setNames(as.character(labels),
                                         as.character(date_to_ms(breaks)))
    }
  } else {
    # Numeric: build label map for custom formatting (e.g., log scale labels)
    if (!is.null(breaks) && !is.null(labels)) {
      labels_chr <- as.character(labels)
      breaks_chr <- as.character(breaks)
      # Only create label map if labels differ from raw breaks
      if (!identical(labels_chr, breaks_chr)) {
        spec$label_map <- stats::setNames(labels_chr, breaks_chr)
      }
    }
  }

  # Axis title from ggplot labels
  ax_label <- p$labels[[axis]]
  if (!is.null(ax_label) && nchar(ax_label) > 0) {
    spec$title <- ax_label
  }

  spec
}

#' Extract discrete categories from a ggplot in order
#'
#' @param p The ggplot object
#' @param b The built plot
#' @param axis "x" or "y"
#' @return Character vector of categories
#' @noRd
extract_categories <- function(p, b, axis = "x") {
  # Try from scale limits/levels
  scale <- NULL
  tryCatch({
    for (s in p$scales$scales) {
      if (axis %in% s$aesthetics) {
        scale <- s
        break
      }
    }
  }, error = function(e) NULL)

  if (!is.null(scale)) {
    limits <- tryCatch(scale$get_limits(), error = function(e) NULL)
    if (!is.null(limits)) return(as.character(limits))
  }

  # Try from panel params
  params <- b$layout$panel_params[[1]]
  ax <- params[[axis]]
  if (!is.null(ax)) {
    levels <- safe_extract(ax, "range", "levels")
    if (!is.null(levels)) return(as.character(levels))

    # Older ggplot2 format
    limits <- safe_extract(ax, "limits")
    if (!is.null(limits) && is.character(limits)) return(limits)
  }

  # Fallback from built data
  col <- if (axis == "x") "x" else "y"
  if (length(b$data) > 0 && col %in% names(b$data[[1]])) {
    vals <- b$data[[1]][[col]]
    if (is.factor(vals)) return(levels(vals))
    return(unique(as.character(vals)))
  }

  character(0)
}
