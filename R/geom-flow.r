#' Flows between lodes or strata
#'
#' `geom_flow` receives a dataset of the horizontal (`x`) and vertical (`y`,
#' `ymin`, `ymax`) positions of the **lodes** of an alluvial plot, the
#' intersections of the alluvia with the strata. It reconfigures these into
#' alluvial segments connecting pairs of corresponding lodes in adjacent strata
#' and plots filled x-splines between each such pair, using a provided knot
#' position parameter `knot.pos`, and filled rectangles at either end, using a
#' provided `width`.
#'
#' The helper function `positions_toflow()` takes the corner and knot positions
#' and curve parameters for a single flow as input and returns a data frame of
#' `x`, `y`, and `shape` used by [grid::xsplineGrob()] to render the flow.
#' @template geom-aesthetics
#' @template geom-curves
#' @template defunct-geom-params
#'   

#' @import ggplot2
#' @family alluvial geom layers
#' @seealso [ggplot2::layer()] for additional arguments and
#'   [stat_alluvium()] and
#'   [stat_flow()] for the corresponding stats.
#' @inheritParams geom_alluvium
#' @param aes.flow Character; how inter-lode flows assume aesthetics from lodes.
#'   Options are "forward" and "backward".
#' @param x0,x1,ymin0,ymax0,ymin1,ymax1,kp0,kp1 Numeric corner and knot position
#'   data for the ribbon of a single flow.
#' @example inst/examples/ex-geom-flow.r
#' @export
geom_flow <- function(mapping = NULL,
                      data = NULL,
                      stat = "flow",
                      position = "identity",
                      width = 1/3,
                      knot.pos = 1/4, knot.prop = TRUE,
                      curve_type = NULL, curve_range = NULL,
                      segments = NULL,
                      aes.flow = "forward",
                      na.rm = FALSE,
                      show.legend = NA,
                      inherit.aes = TRUE,
                      ...) {

  aes.flow <- match.arg(aes.flow, c("forward", "backward"))

  layer(
    geom = GeomFlow,
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
      aes.flow = aes.flow,
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname ggalluvial-ggproto
#' @usage NULL
#' @export
GeomFlow <- ggproto(
  "GeomFlow", Geom,

  required_aes = c("x", "y", "ymin", "ymax"),

  default_aes = aes(linewidth = .5, linetype = 1,
                    colour = "transparent", fill = "gray", alpha = .5),

  setup_data = function(data, params) {

    width <- params$width
    if (is.null(width)) {
      width <- 1/3
    }

    knot.pos <- params$knot.pos
    if (is.null(knot.pos)) knot.pos <- 1/4

    # positioning parameters
    transform(data,
              xmin = x - width / 2,
              xmax = x + width / 2,
              knot.pos = knot.pos)
  },

  draw_panel = function(self, data, panel_params, coord,
                        width = 1/3, aes.flow = "forward",
                        knot.pos = 1/4, knot.prop = TRUE,
                        curve_type = NULL, curve_range = NULL,
                        segments = NULL) {
    
    # parameter defaults
    if (is.null(curve_type)) curve_type <- ggalluvial_opt("curve_type")
    if (is.null(curve_range)) curve_range <- ggalluvial_opt("curve_range")
    if (is.null(segments)) segments <- ggalluvial_opt("segments")
    
    # exclude one-sided flows
    data <- data[complete.cases(data), ]

    # adjoin data with itself by alluvia along adjacent axes
    flow_pos <- intersect(names(data), c("x", "xmin", "xmax",
                                         "width", "knot.pos",
                                         "y", "ymin", "ymax"))
    flow_aes <- intersect(names(data), c("linewidth", "size", "linetype",
                                         "colour", "fill", "alpha"))
    flow_fore <- if (aes.flow != "backward") flow_aes else NULL
    flow_back <- if (aes.flow != "forward") flow_aes else NULL
    data <- self_adjoin(
      data = data, key = "x", by = "alluvium",
      link = flow_pos,
      keep.x = flow_fore, keep.y = flow_back,
      suffix = c(".0", ".1")
    )

    # aesthetics (in prescribed order)
    aesthetics <- intersect(.color_diff_aesthetics, names(data))
    # arrange data by aesthetics for consistent (reverse) z-ordering
    data <- data[do.call(order, lapply(
      data[, c("step", aesthetics)],
      function(x) factor(x, levels = unique(x))
    )), ]

    # construct x-spline grobs
    grobs <- lapply(split(data, seq_len(nrow(data))), function(row) {

      # path of spline or unit curve
      f_path <- positions_to_flow(
        row$xmax.0, row$xmin.1,
        row$ymin.0, row$ymax.0, row$ymin.1, row$ymax.1,
        row$knot.pos.0, row$knot.pos.1,
        knot.prop = knot.prop,
        curve_type = curve_type, curve_range = curve_range,
        segments = segments
      )
      # aesthetics
      aes <- as.data.frame(row[flow_aes], stringsAsFactors = FALSE)
      # join aesthetics to path
      f_data <- cbind(f_path, aes[rep(1, nrow(f_path)), ])

      # transform (after calculating spline paths)
      f_coords <- coord$transform(f_data, panel_params)

      # single spline grob
      grid::xsplineGrob(
        x = f_coords$x, y = f_coords$y, shape = f_coords$shape,
        open = FALSE,
        gp = grid::gpar(
          col = f_coords$colour, fill = f_coords$fill,
          alpha = f_coords$alpha,
          lty = f_coords$linetype,
          lwd = (f_coords$linewidth %||% f_coords$size) * .pt
        )
      )
    })

    # combine spline grobs
    grob <- do.call(grid::grobTree, grobs)
    grob$name <- grid::grobName(grob, "xspline")
    grob
  },

  draw_key = draw_key_polygon,
  
  non_missing_aes = "size",
  rename_size = TRUE
)

#' @rdname geom_flow
#' @export
positions_to_flow <- function(
  x0, x1, ymin0, ymax0, ymin1, ymax1, kp0, kp1,
  knot.prop, curve_type, curve_range, segments
) {
  if (curve_type %in% c("spline", "xspline")) {
    # x-spline path
    k_fore <- c(0, kp0, -kp1, 0)
    if (knot.prop) k_fore <- k_fore * (x1 - x0)
    x_fore <- rep(c(x0, x1), each = 2) + k_fore
    data.frame(
      x = c(x_fore, rev(x_fore)),
      y = c(ymin0, ymin0, ymin1, ymin1, ymax1, ymax1, ymax0, ymax0),
      shape = rep(c(0, 1, 1, 0), times = 2)
    )
  } else {
    # default to 48 segments per curve, ensure the minimum number of segments
    if (is.null(segments)) segments <- 48 else if (segments < 3) {
      #warning("Must use at least 3 segments; substituting `segments = 3`.")
      segments <- 3
    }
    # unit curve path
    curve_fun <- make_curve_fun(curve_type, curve_range)
    i_fore <- seq(0, 1, length.out = segments + 1)
    f_fore <- curve_fun(i_fore)
    x_fore <- x0 + (x1 - x0) * i_fore
    data.frame(
      x = c(x_fore, rev(x_fore)),
      y = c(ymin0 + (ymin1 - ymin0) * f_fore,
            ymax1 + (ymax0 - ymax1) * f_fore),
      shape = 0
    )
  }
}
