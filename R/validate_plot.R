#' Validate that a ggplot object is within the supported scope
#'
#' Checks for supported geoms, single panel (no facets), and cartesian
#' coordinates. Raises informative errors for unsupported features.
#'
#' @param p A ggplot object
#' @param b The result of `ggplot_build(p)` (optional; built internally if NULL)
#' @param warn_unsupported Logical; emit warnings for silently dropped features
#' @return Invisible TRUE if valid; otherwise throws an error
#' @noRd
validate_plot <- function(p, b = NULL, warn_unsupported = TRUE) {
  if (!inherits(p, "ggplot")) {
    rlang::abort("Input must be a ggplot object.", class = "gghighcharts_error")
  }

  # --- Check for facets ---
  if (!inherits(p$facet, "FacetNull")) {
    rlang::abort(
      c(
        "Faceted plots are not supported by gghighcharts.",
        i = "Only single-panel plots can be converted.",
        i = "Remove `facet_wrap()` or `facet_grid()` and try again."
      ),
      class = "gghighcharts_unsupported_facet"
    )
  }

  # --- Check panels from built data ---
  if (!is.null(b)) {
    n_panels <- length(b$layout$panel_params)
    if (n_panels != 1) {
      rlang::abort(
        c(
          paste0("Plot has ", n_panels, " panels; only single-panel plots are supported."),
          i = "Remove faceting to use gghighcharts."
        ),
        class = "gghighcharts_unsupported_facet"
      )
    }
  }

  # --- Check coordinate system ---
  coord_class <- class(p$coordinates)[1]
  supported_coords <- c("CoordCartesian", "CoordFlip")
  if (!coord_class %in% supported_coords) {
    rlang::abort(
      c(
        paste0("Unsupported coordinate system: ", coord_class),
        i = "Only cartesian coordinates (and coord_flip) are supported.",
        i = "coord_polar(), coord_sf(), etc. cannot be translated."
      ),
      class = "gghighcharts_unsupported_coord"
    )
  }

  is_flipped <- coord_class == "CoordFlip"

  # --- Check geom layers ---
  supported_geoms <- c("GeomLine", "GeomPath", "GeomPoint", "GeomCol", "GeomBar",
                       "GeomArea", "GeomStep", "GeomSmooth", "GeomRibbon",
                       "GeomHline", "GeomVline", "GeomAbline",
                       "GeomErrorbar", "GeomBoxplot",
                       "GeomText", "GeomLabel", "GeomRect")

  if (length(p$layers) == 0) {
    rlang::abort(
      "Plot has no layers.",
      class = "gghighcharts_error"
    )
  }

  for (i in seq_along(p$layers)) {
    layer <- p$layers[[i]]
    geom_cls <- geom_class_name(layer)

    if (!geom_cls %in% supported_geoms) {
      rlang::abort(
        c(
          paste0("Unsupported geom in layer ", i, ": ", geom_cls),
          i = paste0("Supported geoms: ", paste(supported_geoms, collapse = ", ")),
          i = "Remove unsupported layers or use a different plot type."
        ),
        class = "gghighcharts_unsupported_geom"
      )
    }

    # Warn about unsupported aesthetics
    if (warn_unsupported) {
      aes_names <- names(layer$mapping)
      unsupported_aes <- setdiff(
        aes_names,
        c("x", "y", "colour", "color", "fill", "group", "size", "shape",
          "linetype", "alpha", "ymin", "ymax", "xmin", "xmax",
          "yintercept", "xintercept", "intercept", "slope",
          "lower", "middle", "upper", "weight", "text", "label")
      )
      for (a in unsupported_aes) {
        warn_unsupported_feature(paste0("aesthetic '", a, "' in layer ", i))
      }
    }
  }

  invisible(list(valid = TRUE, flipped = is_flipped))
}
