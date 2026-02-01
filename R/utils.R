# Convert NA values in a vector to NULL (for JSON serialization)
# Highcharts expects `null` for missing data points.
na_to_null <- function(x) {
  lapply(x, function(val) if (is.na(val)) NULL else val)
}

# Convert a Date, POSIXct, or numeric (days since epoch) to milliseconds
# since Unix epoch (UTC). When ggplot2 builds date data, it stores values
# as numeric days since 1970-01-01.
date_to_ms <- function(x) {
  if (inherits(x, "POSIXct")) {
    as.numeric(x) * 1000
  } else if (inherits(x, "Date")) {
    as.numeric(as.POSIXct(x, tz = "UTC")) * 1000
  } else {
    # Numeric: assume days since epoch (ggplot2 built data for Date axes)
    as.numeric(x) * 86400 * 1000
  }
}

# Determine the ggplot2 geom class name from a layer
# Returns a character string like "GeomLine", "GeomPoint", etc.
geom_class_name <- function(layer) {
  cls <- class(layer$geom)
  geom_cls <- cls[startsWith(cls, "Geom")]
  if (length(geom_cls) == 0) return(cls[1])
  geom_cls[1]
}

# Map ggplot2 geom class names to Highcharts series types
geom_to_hc_type <- function(geom_name) {
  switch(geom_name,
    GeomLine     = "line",
    GeomPath     = "line",
    GeomPoint    = "scatter",
    GeomCol      = "column",
    GeomBar      = "column",
    GeomArea     = "area",
    GeomRibbon   = "arearange",
    GeomStep     = "line",
    GeomSmooth   = "line",
    GeomHline    = "plotline_y",
    GeomVline    = "plotline_x",
    GeomAbline   = "plotline_ab",
    GeomErrorbar = "errorbar",
    GeomBoxplot  = "boxplot",
    GeomText     = "annotation_text",
    GeomLabel    = "annotation_text",
    GeomRect     = "annotation_rect",
    stop("Unsupported geom: ", geom_name, call. = FALSE)
  )
}

# Map ggplot2 linetype values to Highcharts dashStyle names.
# ggplot2 uses integers 0-6 or names; Highcharts uses string dash styles.
linetype_to_dashstyle <- function(lt) {
  if (is.null(lt) || is.na(lt)) return(NULL)
  lt <- as.character(lt)
  switch(lt,
    "0" = , "blank"    = "ShortDot",
    "1" = , "solid"    = "Solid",
    "2" = , "dashed"   = "Dash",
    "3" = , "dotted"   = "Dot",
    "4" = , "dotdash"  = "DashDot",
    "5" = , "longdash" = "LongDash",
    "6" = , "twodash"  = "LongDashDot",
    "Solid"
  )
}

# Map ggplot2 point shapes (0-25) to Highcharts marker symbols.
shape_to_marker <- function(shape) {
  if (is.null(shape) || is.na(shape)) return(NULL)
  shape <- as.integer(shape)
  switch(as.character(shape),
    "0"  = "square",
    "1"  = "circle",
    "2"  = "triangle",
    "3"  = "cross",      # plus sign
    "4"  = "cross",      # x mark
    "5"  = "diamond",
    "6"  = "triangle-down",
    "7"  = "square",
    "8"  = "cross",
    "9"  = "diamond",
    "10" = "circle",
    "11" = "triangle",
    "12" = "square",
    "13" = "circle",
    "14" = "square",
    "15" = "square",
    "16" = "circle",
    "17" = "triangle",
    "18" = "diamond",
    "19" = "circle",
    "20" = "circle",
    "21" = "circle",
    "22" = "square",
    "23" = "diamond",
    "24" = "triangle",
    "25" = "triangle-down",
    "circle"
  )
}

# Resolve animation parameter to a value suitable for Highcharts
# plotOptions.series.animation.
# TRUE -> NULL (use default, no override needed)
# FALSE -> FALSE (disable)
# numeric -> list(duration = value) (custom duration in ms)
resolve_animation <- function(animation) {
  if (is.null(animation) || isTRUE(animation)) return(NULL)
  if (isFALSE(animation)) return(FALSE)
  if (is.numeric(animation) && length(animation) == 1) {
    return(list(duration = as.integer(animation)))
  }
  NULL
}

# Safely extract a value from a nested list, returning a default if missing
safe_extract <- function(x, ..., default = NULL) {
  tryCatch(
    {
      keys <- list(...)
      val <- x
      for (key in keys) {
        val <- val[[key]]
        if (is.null(val)) return(default)
      }
      val
    },
    error = function(e) default
  )
}

# Convert ggplot2 colour hex to a format suitable for Highcharts
# ggplot2 may produce 8-character hex (#RRGGBBAA). Highcharts needs 6 or 8.
normalize_colour <- function(col) {
  if (is.null(col) || is.na(col)) return(NULL)
  col <- as.character(col)
  if (!startsWith(col, "#")) {
    tryCatch(
      {
        rgb_val <- grDevices::col2rgb(col, alpha = TRUE)
        col <- grDevices::rgb(rgb_val[1], rgb_val[2], rgb_val[3],
                              rgb_val[4], maxColorValue = 255)
      },
      error = function(e) return(col)
    )
  }
  # Strip alpha if fully opaque (#RRGGBBFF -> #RRGGBB)
  if (nchar(col) == 9 && toupper(substr(col, 8, 9)) == "FF") {
    col <- substr(col, 1, 7)
  }
  col
}

# Check if a scale is discrete
is_discrete_scale <- function(scale) {
  inherits(scale, "ScaleDiscrete")
}

# Check if a scale is a date/datetime scale
is_date_scale <- function(scale) {
  inherits(scale, c("ScaleContinuousDate", "ScaleContinuousDatetime"))
}

# Emit a warning about unsupported features if warn_unsupported is TRUE
warn_unsupported_feature <- function(feature, warn = TRUE) {
  if (warn) {
    rlang::warn(
      paste0("gghighcharts: unsupported feature ignored: ", feature),
      class = "gghighcharts_unsupported"
    )
  }
}
