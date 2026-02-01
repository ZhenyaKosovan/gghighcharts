#' Build the normalized internal specification from a ggplot object
#'
#' This is the core pipeline step: ggplot object -> ggplot_build() -> spec.
#' The spec is a structured list that can be translated to highcharter calls.
#'
#' @param p A ggplot object (already validated)
#' @param b The result of ggplot_build(p)
#' @param title Optional title override
#' @param subtitle Optional subtitle override
#' @param warn_unsupported Logical; emit warnings for dropped features
#' @return A list with meta, x, y, layers, and flipped flag
#' @noRd
build_spec <- function(p, b, title = NULL, subtitle = NULL,
                       warn_unsupported = TRUE) {
  # Validation returns info about coord_flip
  validation <- validate_plot(p, b, warn_unsupported)
  is_flipped <- validation$flipped

  # Panel params
  panel_params <- get_panel_params(b)

  # When coord_flip is used, ggplot2 swaps panel_params (x becomes values,
  # y becomes categories) but does NOT swap built data. We need to read
  # axis specs from the swapped panel_params to match the unswapped data,
  # then use chart.inverted = TRUE in Highcharts to flip visually.
  x_axis_name <- if (is_flipped) "y" else "x"
  y_axis_name <- if (is_flipped) "x" else "y"

  x_spec <- build_axis_spec(p, b, panel_params, axis = x_axis_name)
  y_spec <- build_axis_spec(p, b, panel_params, axis = y_axis_name)

  # Build layer specs
  layers <- vector("list", length(p$layers))
  for (i in seq_along(p$layers)) {
    layer <- p$layers[[i]]
    built_data <- b$data[[i]]

    geom_cls <- geom_class_name(layer)
    hc_type <- geom_to_hc_type(geom_cls)

    # Extract relevant built columns
    layer_data <- built_data

    # Collect aesthetic params from built data
    aes_params <- list()
    if ("colour" %in% names(layer_data)) aes_params$colour <- layer_data$colour
    if ("fill" %in% names(layer_data)) aes_params$fill <- layer_data$fill
    if ("size" %in% names(layer_data)) aes_params$size <- layer_data$size
    if ("alpha" %in% names(layer_data)) aes_params$alpha <- layer_data$alpha
    if ("linewidth" %in% names(layer_data)) aes_params$linewidth <- layer_data$linewidth
    if ("shape" %in% names(layer_data)) aes_params$shape <- layer_data$shape

    # Detect which aesthetics have continuous scales
    continuous_aes <- detect_continuous_aes(b)

    # Determine grouping column (skip continuous colour/fill)
    group_col <- determine_group_col(layer_data, geom_cls, continuous_aes)

    # Extract the original mapping labels for series names
    mapping_labels <- extract_mapping_labels(p, layer, b, i)

    # Detect position type for bar/column geoms
    pos_type <- detect_position_type(layer)

    layers[[i]] <- list(
      geom           = geom_cls,
      hc_type        = hc_type,
      data           = layer_data,
      aes_params     = aes_params,
      group_col      = group_col,
      mapping_labels = mapping_labels,
      position_type  = pos_type,
      continuous_aes = continuous_aes,
      layer_index    = i
    )
  }

  # Determine global stacking mode from bar/column layers
  stacking <- detect_stacking(layers)

  # Extract axis limits from coord_cartesian
  axis_limits <- extract_coord_limits(p, is_flipped)

  # Extract theme options
  theme_opts <- extract_theme_opts(b)

  # Extract annotations (GeomText, GeomRect layers)
  annotations <- extract_annotations(p, b)

  # Detect if any layer has text data (from aes(text = ...))
  has_text_aes <- any(vapply(seq_along(p$layers), function(i) {
    "text" %in% names(b$data[[i]])
  }, logical(1)))

  # Meta
  meta <- list(
    title    = title %||% p$labels$title,
    subtitle = subtitle %||% p$labels$subtitle
  )

  list(
    meta         = meta,
    x            = x_spec,
    y            = y_spec,
    layers       = layers,
    flipped      = is_flipped,
    stacking     = stacking,
    axis_limits  = axis_limits,
    theme_opts   = theme_opts,
    annotations  = annotations,
    has_text_aes = has_text_aes
  )
}

#' Determine the grouping column for a built data layer
#'
#' @param data Built layer data frame
#' @return Character name of the grouping column, or NULL
#' @noRd
determine_group_col <- function(data, geom_cls = NULL, continuous_aes = character(0)) {
  # Check if colour or fill vary — prefer these for series grouping
  # because `group` in ggplot2 built data can be per-bar (e.g., dodged bars
  # where each bar gets its own group integer).
  # Skip continuous aesthetics — they should produce per-point colours,
  # not one series per unique colour value.
  for (col in c("fill", "colour")) {
    if (col %in% continuous_aes) next
    if (col %in% names(data) && length(unique(data[[col]])) > 1) {
      return(col)
    }
  }

  # For bar/column geoms, don't split by group if fill/colour are constant.
  # ggplot2 assigns unique group per bar for discrete x, which would
  # incorrectly create one series per bar.
  if (!is.null(geom_cls) && geom_cls %in% c("GeomCol", "GeomBar")) {
    return(NULL)
  }

  # Fallback: use "group" if it has more than one unique value.
  # However, skip if group variation is solely caused by a non-grouping
  # aesthetic like "text" (used for tooltip labels, not series splitting).
  # ggplot2 assigns unique groups when any discrete aesthetic is mapped,
  # including text, which would incorrectly create one series per point.
  if ("group" %in% names(data)) {
    n_groups <- length(unique(data$group))
    if (n_groups > 1) {
      # Check if group variation is driven only by text aesthetic:
      # if text has as many unique values as groups and colour/fill are
      # constant, this is spurious grouping.
      if ("text" %in% names(data) &&
          length(unique(data$text)) == n_groups &&
          (!("colour" %in% names(data)) || length(unique(data$colour)) <= 1) &&
          (!("fill" %in% names(data)) || length(unique(data$fill)) <= 1)) {
        return(NULL)
      }
      return("group")
    }
  }

  NULL
}

#' Extract aesthetic label mapping for series names
#'
#' Tries to find the original labels for colour/fill/group that were used
#' in the ggplot mapping, so series can be named meaningfully.
#'
#' @param p The ggplot object
#' @param layer The layer object
#' @param b The built plot
#' @param layer_index The layer index
#' @return A named list with group_value -> label mappings, or NULL
#' @noRd
extract_mapping_labels <- function(p, layer, b, layer_index) {
  # Look for colour or fill scales that have labels
  # Use b$plot$scales which includes auto-added scales
  all_scales <- tryCatch(b$plot$scales$scales, error = function(e) {
    tryCatch(p$scales$scales, error = function(e2) list())
  })

  for (aes_name in c("colour", "fill")) {
    scale <- NULL
    tryCatch({
      for (s in all_scales) {
        if (aes_name %in% s$aesthetics) {
          scale <- s
          break
        }
      }
    }, error = function(e) NULL)

    if (!is.null(scale) && is_discrete_scale(scale)) {
      # Get the palette: map from data values to display labels
      limits <- tryCatch(scale$get_limits(), error = function(e) NULL)
      labels <- tryCatch(scale$get_labels(), error = function(e) NULL)

      if (!is.null(limits) && !is.null(labels)) {
        palette <- tryCatch(
          scale$map(limits),
          error = function(e) NULL
        )

        if (!is.null(palette)) {
          return(list(
            aesthetic = aes_name,
            value_labels = stats::setNames(as.character(limits),
                                            as.character(palette)),
            limits = limits,
            labels = labels
          ))
        }
      }
    }
  }

  NULL
}

#' Detect which aesthetics (colour, fill, size) use continuous scales
#' @noRd
detect_continuous_aes <- function(b) {
  result <- character(0)
  all_scales <- tryCatch(b$plot$scales$scales, error = function(e) list())

  for (s in all_scales) {
    if (inherits(s, "ScaleContinuous") || inherits(s, "ScaleBinned")) {
      for (aes_name in s$aesthetics) {
        if (aes_name %in% c("colour", "fill", "size")) {
          result <- c(result, aes_name)
        }
      }
    }
  }

  unique(result)
}

#' Detect the position type from a ggplot2 layer
#' @noRd
detect_position_type <- function(layer) {
  pos <- layer$position
  if (inherits(pos, "PositionFill"))  return("fill")
  if (inherits(pos, "PositionStack")) return("stack")
  if (inherits(pos, "PositionDodge")) return("dodge")
  "identity"
}

#' Determine global stacking mode from bar/column layers
#' @noRd
detect_stacking <- function(layers) {
  for (layer in layers) {
    if (layer$hc_type == "column") {
      if (layer$position_type == "fill")  return("percent")
      if (layer$position_type == "stack") return("normal")
    }
  }
  NULL
}

#' Extract axis limits from coord_cartesian
#' @noRd
extract_coord_limits <- function(p, is_flipped = FALSE) {
  limits <- list(x = NULL, y = NULL)
  coord_limits <- tryCatch(p$coordinates$limits, error = function(e) NULL)
  if (is.null(coord_limits)) return(limits)

  x_lim <- coord_limits$x
  y_lim <- coord_limits$y

  # coord_flip swaps the limits in ggplot2
  if (is_flipped) {
    tmp <- x_lim
    x_lim <- y_lim
    y_lim <- tmp
  }

  limits$x <- x_lim
  limits$y <- y_lim
  limits
}

#' Null-coalescing operator
#' @noRd
`%||%` <- function(x, y) if (is.null(x)) y else x
