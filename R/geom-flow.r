#' Flows between lodes or strata
#'
#' `geom_flow` receives a dataset of the horizontal (`x`) and
#' vertical (`y`, `ymin`, `ymax`) positions of the **lodes**
#' of an alluvial plot, the intersections of the alluvia with the strata.
#' It reconfigures these into alluvial segments connecting pairs of
#' corresponding lodes in adjacent strata and plots filled x-splines between
#' each such pair, using a provided knot position parameter `knot.pos`, and
#' filled rectangles at either end, using a provided `width`.
#' @template geom-aesthetics
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
#' @example inst/examples/ex-geom-flow.r
#' @export
geom_flow <- function(mapping = NULL,
                      data = NULL,
                      stat = "flow",
                      position = "identity",
                      width = 1/3,
                      knot.pos = 1/4, knot.fix = FALSE,
                      curve = "xspline", reach = NULL, segments = NULL,
                      space = "rgb", interpolate = "linear",
                      aes.flow = "forward",
                      na.rm = FALSE,
                      show.legend = NA,
                      inherit.aes = TRUE,
                      ...) {
  
  aes.flow <- match.arg(aes.flow, c("forward", "backward", "interpolate"))
  
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
      knot.fix = knot.fix,
      curve = curve,
      reach = reach,
      segments = segments,
      space = space,
      interpolate = interpolate,
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
  
  default_aes = aes(size = .5, linetype = 1,
                    colour = 0, fill = "gray", alpha = .5),
  
  setup_data = function(data, params) {
    
    width <- params$width
    if (is.null(width)) {
      width <- 1/3
    }
    
    knot.pos <- params$knot.pos
    if (is.null(knot.pos)) knot.pos <- 1/6
    
    # positioning parameters
    transform(data,
              xmin = x - width / 2,
              xmax = x + width / 2,
              knot.pos = knot.pos)
  },
  
  draw_panel = function(self, data, panel_params, coord,
                        width = 1/3, aes.flow = "forward",
                        knot.pos = 1/4, knot.fix = FALSE,
                        curve = "xspline", reach = NULL, segments = NULL,
                        space = "rgb", interpolate = "linear") {
    save(data, panel_params, coord,
         width, aes.flow, knot.pos, knot.fix, curve, reach, segments,
         space, interpolate,
         file = "draw-panel.rda")
    load("draw-panel.rda")
    
    # exclude one-sided flows
    data <- data[complete.cases(data), ]
    
    # ensure compatibility among parameters
    if (aes.flow == "interpolate" && curve == "xspline") {
      stop("Cannot interpolate flow aesthetics along an x-spline curve; ",
           "use an alternative curve instead.")
    }
    if (curve != "xspline") {
      # default to 48 segments per curve, ensure the minimum number of segments
      if (is.null(segments)) segments <- 48 else if (segments < 3) {
        #warning("Must use at least 3 segments; substituting `segments = 3`.")
        segments <- 3
      }
    }
    
    # adjoin data with itself by alluvia along adjacent axes
    flow_pos <- intersect(names(data), c("x", "xmin", "xmax", "width",
                                         "y", "ymin", "ymax", "knot.pos"))
    flow_aes <- intersect(names(data), c("size", "linetype",
                                         "colour", "fill", "alpha"))
    flow_fore <- if (aes.flow == "forward") flow_aes else NULL
    flow_back <- if (aes.flow == "backward") flow_aes else NULL
    flow_link <- c(flow_pos, if (aes.flow == "interpolate") flow_aes)
    data <- self_adjoin(
      data = data, key = "x", by = "alluvium",
      link = flow_link,
      keep.x = flow_fore, keep.y = flow_back,
      suffix = c(".0", ".1")
    )
    
    # aesthetics (in prescribed order)
    aesthetics <- intersect(
      if (aes.flow == "interpolate") paste0(flow_aes, ".0") else flow_aes,
      names(data)
    )
    # arrange data by aesthetics for consistent (reverse) z-ordering
    data <- data[do.call(order, lapply(
      data[, c("step", aesthetics)],
      function(x) factor(x, levels = unique(x))
    )), ]
    
    # construct x-spline grobs
    grobs <- plyr::alply(data, 1, function(row) {
      
      # path of spline or unit curve
      f_path <- row_to_curve(row$xmax.0, row$xmin.1,
                             row$ymin.0, row$ymax.0, row$ymin.1, row$ymax.1,
                             row$knot.pos.0, row$knot.pos.1,
                             knot.fix = knot.fix,
                             curve = curve, reach = reach, segments = segments)
      
      if (aes.flow == "interpolate") {
        # -+- interpolate values in suitable ways -+-
        aes_fore <- data.frame(
          fill = seq_col(row$fill.0, row$fill.1, length.out = segments+1,
                         space = space, interpolate = interpolate),
          size = seq(row$size.0, row$size.1, length.out = segments+1),
          linetype = rep(
            c(row$linetype.0, row$linetype.1),
            c(ceiling((segments+1) / 2), floor((segments+1) / 2))
          ),
          colour = seq_col(row$colour.0, row$colour.1, length.out = segments+1,
                           space = space, interpolate = interpolate),
          alpha = seq(row$alpha.0, row$alpha.1, length.out = segments+1),
          stringsAsFactors = FALSE
        )
        aes <- rbind(aes_fore, aes_fore[seq(nrow(aes_fore), 1), , drop = FALSE])
        # join aesthetics to path
        f_data <- cbind(f_path, aes)
      } else {
        # uniform aesthetics
        aes <- as.data.frame(row[flow_aes], stringsAsFactors = FALSE)
        # join aesthetics to path
        f_data <- cbind(f_path, aes[rep(1, nrow(f_path)), ])
      }
      
      # transform (after calculating spline paths)
      f_coords <- coord$transform(f_data, panel_params)
      
      # -+- construct separate polygons and segments for interpolation -+-
      
      # single spline grob
      grid::xsplineGrob(
        x = f_coords$x, y = f_coords$y, shape = f_coords$shape,
        open = FALSE,
        gp = grid::gpar(
          col = f_coords$colour, fill = f_coords$fill, alpha = f_coords$alpha,
          lty = f_coords$linetype, lwd = f_coords$size * .pt
        )
      )
    })
    
    # combine spline grobs
    grob <- do.call(grid::grobTree, grobs)
    grob$name <- grid::grobName(grob, "xspline")
    grob
  },
  
  draw_key = draw_key_polygon
)

# send to spline or unit curve depending on parameters
row_to_curve <- function(
  x0, x1, ymin0, ymax0, ymin1, ymax1, kp0, kp1,
  curve, reach, segments, knot.fix
) {
  if (curve %in% c("spline", "xspline")) {
    # x-spline path
    row_to_xspline(x0, x1, ymin0, ymax0, ymin1, ymax1,
                   kp0, kp1, knot.fix)
  } else {
    # unit curve path
    row_to_unit_curve(x0, x1, ymin0, ymax0, ymin1, ymax1,
                      curve, reach, segments)
  }
}

row_to_xspline <- function(
  x0, x1, ymin0, ymax0, ymin1, ymax1,
  kp0, kp1, knot.fix
) {
  k_fore <- c(0, kp0, -kp1, 0)
  if (! knot.fix) k_fore <- k_fore * (x1 - x0)
  x_fore <- rep(c(x0, x1), each = 2) + k_fore
  data.frame(
    x = c(x_fore, rev(x_fore)),
    y = c(ymin0, ymin0, ymin1, ymin1, ymax1, ymax1, ymax0, ymax0),
    shape = rep(c(0, 1, 1, 0), times = 2)
  )
}

row_to_unit_curve <- function(
  x0, x1, ymin0, ymax0, ymin1, ymax1,
  curve, reach, segments
) {
  curve_fun <- make_curve_fun(curve, reach)
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

seq_col <- function(from, to, length.out, ...) {
  if (from == 0 && to == 0) return (rep(0, length.out))
  crf <- colorRamp(c(from, to), ...)
  arr <- crf(seq(0, 1, length.out = length.out))
  apply(arr, 1, function(x) rgb(x[1], x[2], x[3], maxColorValue = 256))
}
