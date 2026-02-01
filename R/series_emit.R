#' Convert a layer spec into one or more Highcharts series
#'
#' Handles series splitting by group, and dispatches to the appropriate
#' emitter based on geom type.
#'
#' @param layer_spec A single element from spec$layers
#' @param x_spec The x-axis spec from the overall spec
#' @param y_spec The y-axis spec
#' @param stacking Global stacking mode (NULL, "normal", "percent")
#' @return A list of series definitions suitable for hc_add_series(),
#'   or a list with a $plotlines element for hline/vline/abline layers.
#' @noRd
emit_series_from_layer <- function(layer_spec, x_spec, y_spec, stacking = NULL) {
  data <- layer_spec$data
  group_col <- layer_spec$group_col
  hc_type <- layer_spec$hc_type
  geom <- layer_spec$geom
  mapping_labels <- layer_spec$mapping_labels

  # --- Skip annotation layers (handled separately) ---
  if (hc_type %in% c("annotation_text", "annotation_rect")) {
    return(list())
  }

  # --- Special handling for reference lines (plotLines, not series) ---
  if (hc_type %in% c("plotline_y", "plotline_x", "plotline_ab")) {
    return(emit_plotlines(layer_spec, x_spec, y_spec))
  }

  # --- Special handling for geom_smooth (line + confidence band) ---
  if (geom == "GeomSmooth") {
    return(emit_smooth_series(layer_spec, x_spec, y_spec))
  }

  # --- Special handling for boxplot ---
  if (geom == "GeomBoxplot") {
    return(emit_boxplot_series(layer_spec, x_spec, y_spec))
  }

  # --- Special handling for errorbar ---
  if (geom == "GeomErrorbar") {
    return(emit_errorbar_series(layer_spec, x_spec, y_spec))
  }

  # --- Special handling for arearange/ribbon ---
  if (hc_type == "arearange") {
    return(emit_arearange_series(layer_spec, x_spec, y_spec))
  }

  # Split data into groups
  if (!is.null(group_col) && group_col %in% names(data)) {
    groups <- split(data, data[[group_col]])
  } else {
    groups <- list(`1` = data)
  }

  series_list <- vector("list", length(groups))

  for (i in seq_along(groups)) {
    group_data <- groups[[i]]
    group_key <- names(groups)[i]

    # Determine series name
    series_name <- resolve_series_name(group_key, group_col, mapping_labels, i)

    # Determine series colour
    series_colour <- resolve_series_colour(group_data, group_col, layer_spec)

    # Determine per-point properties from continuous aesthetics
    cont_aes <- layer_spec$continuous_aes %||% character(0)
    per_point_colour <- has_per_point_colour(group_data, cont_aes)
    per_point_size <- has_per_point_size(group_data, cont_aes)

    # Build data points based on chart type and axis type
    if (hc_type == "column") {
      pos_type <- layer_spec$position_type
      points <- emit_bar_data(group_data, x_spec, y_spec, pos_type)
    } else {
      points <- emit_continuous_data(group_data, x_spec, y_spec,
                                      per_point_colour = per_point_colour,
                                      per_point_size = per_point_size)
    }

    series_def <- list(
      type = hc_type,
      name = series_name,
      data = points
    )

    if (!is.null(series_colour)) {
      series_def$color <- series_colour
    }

    # Scatter: set marker options
    if (hc_type == "scatter") {
      marker_opts <- resolve_marker_opts(group_data)
      if (!is.null(marker_opts)) {
        series_def$marker <- marker_opts
      }
    }

    # Line/area: set lineWidth and dashStyle
    if (hc_type %in% c("line", "area")) {
      lw <- resolve_line_width(group_data)
      if (!is.null(lw)) {
        series_def$lineWidth <- lw
      }
      ds <- resolve_dash_style(group_data)
      if (!is.null(ds)) {
        series_def$dashStyle <- ds
      }
    }

    # Step lines (geom_step)
    if (geom == "GeomStep") {
      series_def$step <- resolve_step_direction(layer_spec)
    }

    # Alpha transparency
    alpha <- resolve_alpha(group_data)
    if (!is.null(alpha)) {
      series_def$opacity <- alpha
    }

    # For histogram (GeomBar with numeric x), set pointRange for bin width
    if (hc_type == "column" && x_spec$type == "numeric" &&
        "xmin" %in% names(group_data) && "xmax" %in% names(group_data)) {
      bin_width <- group_data$xmax[1] - group_data$xmin[1]
      series_def$pointRange <- bin_width
    }

    series_list[[i]] <- series_def
  }

  series_list
}

#' Emit data points for line/scatter (continuous axes)
#' @noRd
emit_continuous_data <- function(data, x_spec, y_spec,
                                  per_point_colour = FALSE,
                                  per_point_size = FALSE) {
  x_vals <- data$x
  y_vals <- data$y

  # Convert dates to ms for Highcharts
  if (x_spec$type %in% c("date", "datetime")) {
    x_vals <- date_to_ms(x_vals)
  }

  # Gather per-point colour and size vectors
  colour_vals <- if (per_point_colour) data$colour %||% data$fill else NULL
  size_vals <- if (per_point_size) data$size else NULL

  # Gather per-point text for tooltip (from aes(text = ...))
  text_vals <- if ("text" %in% names(data)) data$text else NULL

  # Sort by x for line charts
  ord <- order(x_vals, na.last = TRUE)
  x_vals <- x_vals[ord]
  y_vals <- y_vals[ord]
  if (!is.null(colour_vals)) colour_vals <- colour_vals[ord]
  if (!is.null(size_vals))   size_vals   <- size_vals[ord]
  if (!is.null(text_vals))   text_vals   <- text_vals[ord]

  # Build point list
  points <- vector("list", length(x_vals))
  for (i in seq_along(x_vals)) {
    if (is.na(y_vals[i])) {
      points[[i]] <- list(x = x_vals[i], y = NULL)
    } else if (is.na(x_vals[i])) {
      next
    } else {
      pt <- list(x = x_vals[i], y = y_vals[i])
      if (!is.null(colour_vals) && !is.na(colour_vals[i])) {
        pt$color <- normalize_colour(colour_vals[i])
      }
      if (!is.null(size_vals) && !is.na(size_vals[i])) {
        pt$marker <- list(radius = size_vals[i] * 1.5)
      }
      if (!is.null(text_vals) && !is.na(text_vals[i])) {
        pt$text <- as.character(text_vals[i])
      }
      points[[i]] <- pt
    }
  }

  # Remove NULLs from skipped entries
  points <- Filter(Negate(is.null), points)
  points
}

#' Emit data for bar/column charts (discrete x axis)
#' @noRd
emit_bar_data <- function(data, x_spec, y_spec, pos_type = "identity") {
  if (x_spec$type == "discrete" && !is.null(x_spec$categories)) {
    categories <- x_spec$categories

    # Map x values to category indices
    # Built data for discrete axes uses numeric positions (1-based)
    x_vals <- data$x

    # For stacked/fill positions, ggplot2 built data has cumulative y values.
    # Use ymax - ymin to get the raw segment height for each bar.
    if (pos_type %in% c("stack", "fill") &&
        "ymin" %in% names(data) && "ymax" %in% names(data)) {
      y_vals <- data$ymax - data$ymin
    } else {
      y_vals <- data$y
    }

    # Per-point text for tooltip
    text_vals <- if ("text" %in% names(data)) data$text else NULL

    # Create a vector aligned to categories (fill missing with NULL)
    result <- vector("list", length(categories))
    for (j in seq_along(categories)) {
      result[[j]] <- NULL
    }

    for (i in seq_along(x_vals)) {
      idx <- round(x_vals[i])
      if (!is.na(idx) && idx >= 1 && idx <= length(categories)) {
        if (is.na(y_vals[i])) {
          result[[idx]] <- NULL
        } else if (!is.null(text_vals) && !is.na(text_vals[i])) {
          result[[idx]] <- list(y = y_vals[i], text = as.character(text_vals[i]))
        } else {
          result[[idx]] <- y_vals[i]
        }
      }
    }

    return(result)
  }

  # Non-discrete bar (histogram): just use x/y pairs
  emit_continuous_data(data, x_spec, y_spec)
}

# --- Specialized emitters for new geom types ---

#' Emit plotLines for geom_hline, geom_vline, geom_abline
#' Returns a list with $plotlines element instead of series
#' @noRd
emit_plotlines <- function(layer_spec, x_spec, y_spec) {
  data <- layer_spec$data
  hc_type <- layer_spec$hc_type
  plotlines <- list()

  for (i in seq_len(nrow(data))) {
    row <- data[i, ]
    pl <- list(
      width = max(1, (row$linewidth %||% 0.5) * 2),
      color = normalize_colour(row$colour %||% "black")
    )

    lt <- row$linetype
    if (!is.null(lt) && !is.na(lt)) {
      ds <- linetype_to_dashstyle(lt)
      if (!is.null(ds)) pl$dashStyle <- ds
    }

    if (hc_type == "plotline_y" && "yintercept" %in% names(data)) {
      pl$value <- row$yintercept
      pl$axis <- "y"
    } else if (hc_type == "plotline_x" && "xintercept" %in% names(data)) {
      val <- row$xintercept
      if (x_spec$type %in% c("date", "datetime")) {
        val <- date_to_ms(val)
      }
      pl$value <- val
      pl$axis <- "x"
    }
    # geom_abline: skip (complex, requires axis range for line endpoints)

    if (!is.null(pl$value)) {
      plotlines <- c(plotlines, list(pl))
    }
  }

  list(list(.plotlines = plotlines))
}

#' Emit series for geom_smooth (fitted line + optional confidence band)
#' @noRd
emit_smooth_series <- function(layer_spec, x_spec, y_spec) {
  data <- layer_spec$data
  series_list <- list()

  # Fitted line
  line_points <- emit_continuous_data(data, x_spec, y_spec)
  line_series <- list(
    type = "line",
    name = "Smooth",
    data = line_points,
    marker = list(enabled = FALSE)
  )

  colour <- resolve_series_colour(data, NULL, layer_spec)
  if (!is.null(colour)) {
    line_series$color <- colour
  }

  series_list <- c(series_list, list(line_series))

  # Confidence band (if ymin/ymax present and not all equal to y)
  if ("ymin" %in% names(data) && "ymax" %in% names(data)) {
    has_band <- !all(data$ymin == data$y) || !all(data$ymax == data$y)
    if (has_band) {
      band_points <- emit_arearange_data(data, x_spec)
      band_series <- list(
        type = "arearange",
        name = "Confidence",
        data = band_points,
        lineWidth = 0,
        fillOpacity = 0.2,
        linkedTo = ":previous",
        marker = list(enabled = FALSE)
      )
      if (!is.null(colour)) {
        band_series$color <- colour
      }
      series_list <- c(series_list, list(band_series))
    }
  }

  series_list
}

#' Emit arearange data (for confidence bands, ribbons)
#' @noRd
emit_arearange_data <- function(data, x_spec) {
  x_vals <- data$x
  if (x_spec$type %in% c("date", "datetime")) {
    x_vals <- date_to_ms(x_vals)
  }

  ord <- order(x_vals, na.last = TRUE)
  x_vals <- x_vals[ord]
  ymin_vals <- data$ymin[ord]
  ymax_vals <- data$ymax[ord]

  points <- vector("list", length(x_vals))
  for (i in seq_along(x_vals)) {
    if (is.na(x_vals[i])) next
    points[[i]] <- list(x_vals[i], ymin_vals[i], ymax_vals[i])
  }
  Filter(Negate(is.null), points)
}

#' Emit series for geom_ribbon / arearange
#' @noRd
emit_arearange_series <- function(layer_spec, x_spec, y_spec) {
  data <- layer_spec$data
  points <- emit_arearange_data(data, x_spec)

  series <- list(
    type = "arearange",
    name = "Range",
    data = points,
    lineWidth = 0,
    fillOpacity = 0.3
  )

  colour <- resolve_series_colour(data, NULL, layer_spec)
  if (!is.null(colour)) series$color <- colour

  list(series)
}

#' Emit series for geom_boxplot
#' @noRd
emit_boxplot_series <- function(layer_spec, x_spec, y_spec) {
  data <- layer_spec$data

  # Highcharts boxplot data: [low, q1, median, q3, high]
  points <- vector("list", nrow(data))
  for (i in seq_len(nrow(data))) {
    row <- data[i, ]
    points[[i]] <- list(
      low    = row$ymin,
      q1     = row$lower,
      median = row$middle,
      q3     = row$upper,
      high   = row$ymax
    )
  }

  series <- list(
    type = "boxplot",
    name = "Boxplot",
    data = points
  )

  colour <- resolve_series_colour(data, NULL, layer_spec)
  if (!is.null(colour)) series$color <- colour

  result <- list(series)

  # Add outlier points as a separate scatter series
  if ("outliers" %in% names(data)) {
    outlier_points <- list()
    for (i in seq_len(nrow(data))) {
      outs <- data$outliers[[i]]
      if (length(outs) > 0 && !all(is.na(outs))) {
        for (o in outs) {
          # x position is the category index (0-based for Highcharts)
          outlier_points <- c(outlier_points, list(list(x = data$x[i] - 1, y = o)))
        }
      }
    }
    if (length(outlier_points) > 0) {
      outlier_series <- list(
        type = "scatter",
        name = "Outliers",
        data = outlier_points,
        marker = list(radius = 3),
        linkedTo = ":previous"
      )
      result <- c(result, list(outlier_series))
    }
  }

  result
}

#' Emit series for geom_errorbar
#' @noRd
emit_errorbar_series <- function(layer_spec, x_spec, y_spec) {
  data <- layer_spec$data

  # Highcharts errorbar: [low, high]
  x_vals <- data$x
  if (x_spec$type %in% c("date", "datetime")) {
    x_vals <- date_to_ms(x_vals)
  }

  points <- vector("list", nrow(data))
  for (i in seq_len(nrow(data))) {
    if (x_spec$type == "discrete") {
      points[[i]] <- list(low = data$ymin[i], high = data$ymax[i])
    } else {
      points[[i]] <- list(x = x_vals[i], low = data$ymin[i], high = data$ymax[i])
    }
  }

  series <- list(
    type = "errorbar",
    name = "Error",
    data = points
  )

  colour <- resolve_series_colour(data, NULL, layer_spec)
  if (!is.null(colour)) series$color <- colour

  list(series)
}

# --- Resolver helpers ---

#' Resolve series name from group info and mapping labels
#' @noRd
resolve_series_name <- function(group_key, group_col, mapping_labels, index) {
  if (!is.null(mapping_labels)) {
    # Try to map the colour/fill hex back to the original label
    value_labels <- mapping_labels$value_labels
    if (!is.null(value_labels) && group_key %in% names(value_labels)) {
      return(value_labels[[group_key]])
    }
    # If grouping is by the aesthetic itself and we have limits/labels
    limits <- mapping_labels$limits
    labels <- mapping_labels$labels
    if (!is.null(limits) && !is.null(labels)) {
      idx <- match(group_key, limits)
      if (!is.na(idx) && idx <= length(labels)) {
        return(as.character(labels[idx]))
      }
    }
  }

  # If group_col is not a hex colour, use the group key as name
  if (!is.null(group_col) && !is.null(group_key)) {
    if (!grepl("^#[0-9A-Fa-f]", group_key)) {
      return(as.character(group_key))
    }
  }

  paste0("Series ", index)
}

#' Resolve series colour from built data
#' @noRd
resolve_series_colour <- function(data, group_col, layer_spec) {
  for (col in c("colour", "fill")) {
    if (col %in% names(data)) {
      colours <- unique(data[[col]])
      colours <- colours[!is.na(colours)]
      if (length(colours) == 1) {
        return(normalize_colour(colours[1]))
      }
    }
  }
  NULL
}

#' Resolve marker options (size + symbol) from built data
#' @noRd
resolve_marker_opts <- function(data) {
  opts <- list()

  if ("size" %in% names(data)) {
    sizes <- unique(data$size)
    sizes <- sizes[!is.na(sizes)]
    if (length(sizes) == 1) {
      opts$radius <- sizes[1] * 1.5
    }
  }

  if ("shape" %in% names(data)) {
    shapes <- unique(data$shape)
    shapes <- shapes[!is.na(shapes)]
    if (length(shapes) == 1) {
      sym <- shape_to_marker(shapes[1])
      if (!is.null(sym)) opts$symbol <- sym
    }
  }

  if (length(opts) == 0) return(NULL)
  opts
}

#' Resolve line width from built data
#' @noRd
resolve_line_width <- function(data) {
  if ("linewidth" %in% names(data)) {
    lws <- unique(data$linewidth)
    lws <- lws[!is.na(lws)]
    if (length(lws) == 1) {
      return(lws[1] * 2)  # approximate mm -> px
    }
  } else if ("size" %in% names(data)) {
    sizes <- unique(data$size)
    sizes <- sizes[!is.na(sizes)]
    if (length(sizes) == 1) {
      return(sizes[1] * 2)
    }
  }
  NULL
}

#' Resolve dash style from built data linetype
#' @noRd
resolve_dash_style <- function(data) {
  if ("linetype" %in% names(data)) {
    lts <- unique(data$linetype)
    lts <- lts[!is.na(lts)]
    if (length(lts) == 1) {
      ds <- linetype_to_dashstyle(lts[1])
      # Only return non-default
      if (!is.null(ds) && ds != "Solid") return(ds)
    }
  }
  NULL
}

#' Resolve alpha transparency from built data
#' @noRd
resolve_alpha <- function(data) {
  if ("alpha" %in% names(data)) {
    alphas <- unique(data$alpha)
    alphas <- alphas[!is.na(alphas)]
    if (length(alphas) == 1 && alphas[1] < 1) {
      return(alphas[1])
    }
  }
  NULL
}

#' Check if per-point colour should be emitted
#' @noRd
has_per_point_colour <- function(data, continuous_aes) {
  if ("colour" %in% continuous_aes && "colour" %in% names(data)) {
    return(length(unique(data$colour)) > 1)
  }
  if ("fill" %in% continuous_aes && "fill" %in% names(data)) {
    return(length(unique(data$fill)) > 1)
  }
  FALSE
}

#' Check if per-point size should be emitted
#' @noRd
has_per_point_size <- function(data, continuous_aes) {
  if ("size" %in% continuous_aes && "size" %in% names(data)) {
    return(length(unique(data$size)) > 1)
  }
  FALSE
}

#' Resolve step direction for geom_step
#' @noRd
resolve_step_direction <- function(layer_spec) {
  # geom_step has a direction param: "hv" (default), "vh", "mid"
  dir <- tryCatch(layer_spec$data$direction[1], error = function(e) NULL)
  if (is.null(dir)) {
    # Try from the layer's static params
    dir <- tryCatch({
      params <- layer_spec$aes_params
      params$direction
    }, error = function(e) NULL)
  }
  # Map to Highcharts step values
  switch(as.character(dir %||% "hv"),
    "hv"  = "left",
    "vh"  = "right",
    "mid" = "center",
    "left"
  )
}
