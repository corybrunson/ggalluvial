#' Alluvia across strata
#'
#' `geom_alluvium` receives a dataset of the horizontal (`x`) and vertical (`y`,
#' `ymin`, `ymax`) positions of the **lodes** of an alluvial plot, the
#' intersections of the alluvia with the strata. It plots both the lodes
#' themselves, using [geom_lode()], and the flows between them, using
#' [geom_flow()].
#'
#' The helper function `data_to_alluvium()` takes internal **ggplot2** data
#' (mapped aesthetics) and curve parameters for a single alluvium as input and
#' returns a data frame of `x`, `y`, and `shape` used by [grid::xsplineGrob()]
#' to render the alluvium.
#' @template geom-aesthetics
#' @template geom-curves
#' @template defunct-geom-params
#'   

#' @import ggplot2
#' @family alluvial geom layers
#' @seealso [ggplot2::layer()] for additional arguments and
#'   [stat_alluvium()] and
#'   [stat_flow()] for the corresponding stats.
#' @inheritParams geom_lode
#' @param knot.pos The horizontal distance of x-spline knots from each stratum
#'   (`width/2` from its axis), either (if `knot.prop = TRUE`, the default) as a
#'   proportion of the length of the x-spline, i.e. of the gap between adjacent
#'   strata, or (if `knot.prop = FALSE`) on the scale of the `x` direction.
#' @param knot.prop Logical; whether to interpret `knot.pos` as a proportion of
#'   the length of each flow (the default), rather than on the `x` scale.
#' @param curve_type Character; the type of curve used to produce flows.
#'   Defaults to `"xspline"` and can be alternatively set to one of `"linear"`,
#'   `"cubic"`, `"quintic"`, `"sine"`, `"arctangent"`, and `"sigmoid"`.
#'   `"xspline"` produces approximation splines using 4 points per curve; the
#'   alternatives produce interpolation splines between points along the graphs
#'   of functions of the associated type. See the **Curves** section.
#' @param curve_range For alternative `curve_type`s based on asymptotic
#'   functions, the value along the asymptote at which to truncate the function
#'   to obtain the shape that will be scaled to fit between strata. See the
#'   **Curves** section.
#' @param segments The number of segments to be used in drawing each alternative
#'   curve (each curved boundary of each flow). If less than 3, will be silently
#'   changed to 3.
#' @example inst/examples/ex-geom-alluvium.r
#' @export
geom_alluvium <- function(mapping = NULL,
                          data = NULL,
                          stat = "alluvium",
                          position = "identity",
                          width = 1/3,
                          knot.pos = 1/4, knot.prop = TRUE,
                          curve_type = NULL, curve_range = NULL,
                          segments = NULL,
                          na.rm = FALSE,
                          show.legend = NA,
                          inherit.aes = TRUE,
                          ...) {
  layer(
    geom = GeomAlluvium,
    mapping = mapping,
    data = data,
    stat = stat,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      width = width,
      knot.pos = knot.pos,
      knot.prop = knot.prop,
      curve_type = curve_type,
      curve_range = curve_range,
      segments = segments,
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname ggalluvial-ggproto
#' @usage NULL
#' @export
GeomAlluvium <- ggproto(
  "GeomAlluvium", Geom,
  
  required_aes = c("x", "y", "ymin", "ymax"),
  
  default_aes = aes(linewidth = .5, linetype = 1,
                    colour = "transparent", fill = "gray", alpha = .5),
  
  setup_data = function(data, params) {
    
    if (! is.null(params$aes.flow)) {
      warning("Parameter `aes.flow` cannot be used in `geom_alluvium`, ",
              "and will be ignored; ",
              "use `geom_lode` and `geom_flow` instead.")
      params$aes.flow <- NULL
    }
    
    # check whether color or differentiation aesthetics vary within alluvia
    aesthetics <- intersect(.color_diff_aesthetics, names(data))
    if (nrow(unique(data[, c("alluvium", aesthetics), drop = FALSE])) !=
        length(unique(data$alluvium))) {
      warning("Some differentiation aesthetics vary within alluvia, ",
              "and will be diffused by their first value.\n",
              "Consider using `geom_flow()` instead.")
    }
    
    knot.pos <- params$knot.pos
    if (is.null(knot.pos)) knot.pos <- 1/4
    
    # positioning parameters
    transform(data,
              knot.pos = knot.pos)
  },
  
  draw_group = function(self, data, panel_scales, coord,
                        width = 1/3,
                        knot.pos = 1/4, knot.prop = TRUE,
                        curve_type = NULL, curve_range = NULL,
                        segments = NULL) {
    
    # parameter defaults
    if (is.null(curve_type)) curve_type <- ggalluvial_opt("curve_type")
    if (is.null(curve_range)) curve_range <- ggalluvial_opt("curve_range")
    if (is.null(segments)) segments <- ggalluvial_opt("segments")
    
    # add width to data
    data <- transform(data, width = width)
    
    first_row <- data[1, setdiff(names(data),
                                 c("x", "xmin", "xmax",
                                   "width", "knot.pos",
                                   "y", "ymin", "ymax")),
                      drop = FALSE]
    rownames(first_row) <- NULL
    
    # default to 48 segments per curve, ensure the minimum number of segments
    if (is.null(segments)) segments <- 48 else if (segments < 3) {
      #warning("Must use at least 3 segments; substituting `segments = 3`.")
      segments <- 3
    }
    
    curve_data <- data_to_alluvium(
      data,
      knot.prop = knot.prop,
      curve_type = curve_type,
      curve_range = curve_range,
      segments = segments
    )
    
    data <- data.frame(first_row, curve_data)
    
    # transform (after calculating spline paths)
    coords <- coord$transform(data, panel_scales)
    
    # graphics object
    grid::xsplineGrob(
      x = coords$x, y = coords$y, shape = coords$shape,
      open = FALSE,
      gp = grid::gpar(
        col = coords$colour, fill = coords$fill,
        alpha = coords$alpha,
        lty = coords$linetype,
        lwd = (coords$linewidth %||% coords$size) * .pt
      )
    )
  },
  
  draw_key = draw_key_polygon,
  
  non_missing_aes = "size",
  rename_size = TRUE
)

#' @rdname geom_alluvium
#' @export
data_to_alluvium <- function(
  data,
  knot.prop = TRUE,
  curve_type = "spline",
  curve_range = NULL,
  segments = NULL
) {
  if (nrow(data) == 1L) {
    # spline coordinates (one axis)
    
    with(data, data.frame(
      x = x + width / 2 * c(-1, 1, 1, -1),
      y = ymin + (ymax - ymin) * c(0, 0, 1, 1),
      shape = rep(0, 4L)
    ))
  } else if (curve_type %in% c("spline", "xspline")) {
    # spline coordinates (more than one axis)
    
    # calculate control point coordinates for x-splines:
    # left side, right side, foreward knot, rearward knot, left side, right side
    w_fore <- rep(data$width, c(3, rep(4, nrow(data) - 2L), 3))
    k_fore <- rep(data$knot.pos, c(3, rep(4, nrow(data) - 2L), 3))
    if (knot.prop) {
      # distances between strata
      b_fore <- rep(data$x, c(1, rep(2, nrow(data) - 2L), 1)) +
        c(1, -1) * rep(data$width / 2, c(1, rep(2, nrow(data) - 2L), 1))
      d_fore <- diff(b_fore)[seq(length(b_fore) - 1L) %% 2L]
      # scale `k_fore` to these distances
      k_fore <- k_fore * c(0, rep(d_fore, rep(4, nrow(data) - 1L)), 0)
    }
    # axis position +/- corresponding width +/- relative knot position
    x_fore <- rep(data$x, c(3, rep(4, nrow(data) - 2L), 3)) +
      w_fore / 2 * c(-1, rep(c(1, 1, -1, -1), nrow(data) - 1L), 1) +
      k_fore * c(0, rep(c(0, 1, -1, 0), nrow(data) - 1L), 0)
    # vertical positions are those of lodes
    ymin_fore <- rep(data$ymin, c(3, rep(4, nrow(data) - 2L), 3))
    ymax_fore <- rep(data$ymax, c(3, rep(4, nrow(data) - 2L), 3))
    shape_fore <- c(0, rep(c(0, 1, 1, 0), nrow(data) - 1L), 0)
    data.frame(
      x = c(x_fore, rev(x_fore)),
      y = c(ymin_fore, rev(ymax_fore)),
      shape = rep(shape_fore, 2L)
    )
  } else {
    # unit curve coordinates (more than one axis)
    
    # specs for a single flow curve
    curve_fun <- make_curve_fun(curve_type, curve_range)
    i_once <- seq(0L, 1L, length.out = segments + 1L)
    f_once <- curve_fun(i_once)
    # coordinates for a full curve
    b_fore <- as.vector(rbind(data$x - data$w / 2, data$x + data$w / 2))
    x_fore <- c(
      b_fore[1],
      t(b_fore[seq(nrow(data) - 1L) * 2L] +
          outer(diff(b_fore)[seq(nrow(data) - 1L) * 2L], i_once, "*")),
      b_fore[nrow(data) * 2L]
    )
    ymin_fore <- c(
      data$ymin[1L],
      t(data$ymin[-nrow(data)] + outer(diff(data$ymin), f_once, "*")),
      data$ymin[nrow(data)]
    )
    ymax_fore <- c(
      data$ymax[1L],
      t(data$ymax[-nrow(data)] + outer(diff(data$ymax), f_once, "*")),
      data$ymax[nrow(data)]
    )
    data.frame(
      x = c(x_fore, rev(x_fore)),
      y = c(ymin_fore, rev(ymax_fore)),
      shape = 0
    )
  }
}
