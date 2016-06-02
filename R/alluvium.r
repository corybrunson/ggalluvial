#' Alluvial flows
#' 
#' \code{stat_alluvium} calculates the depth of each group at each axis. 
#' \code{geom_alluvium} plots an x-spline for each group through the axes at 
#' these depths.
#' 
#' @name alluvium
#' @seealso \code{\link{stratum}} for intra-axis boxes and
#'   \code{\link{ggalluvial}} for a shortcut method.
#' @usage NULL
#' @export
#' @inheritParams layer
#' @param axis_width The width of each variable axis, as a proportion of the 
#'   separation between axes.
#' @param ribbon_bend The horizontal distance between a variable axis 
#'   (\code{axis_width/2} from its center) and the control point of the 
#'   x-spline, also as a proportion of the separation between the axes.
#' @example inst/examples/alluvium.r
StatAlluvium <- ggproto(
    "StatAlluvium", Stat,
    setup_data = function(data, params) {
        if (is.null(data$weight)) data$weight <- rep(1, nrow(data))
        aggregate(
            formula = as.formula(paste("weight ~",
                                       paste(setdiff(names(data), "weight"),
                                             collapse = "+"))),
            data = data,
            FUN = sum
        )
    },
    compute_panel = function(data, scales, params,
                             axis_width = 1/3) {
        axis_ind <- get_axes(names(data))
        # x and y coordinates of center of flow at each axis
        compute_alluvium <- function(i) {
            # order axis indices
            axis_seq <- axis_ind[zigzag(n = length(axis_ind), i = i)]
            # order ribbons according to axes, in above order
            ribbon_seq <- do.call(order, data[axis_seq])
            # ribbon floors and ceilings along axis
            ymin_seq <- c(0, cumsum(data$weight[ribbon_seq]))
            ymax_seq <- c(cumsum(data$weight[ribbon_seq]), sum(data$weight))
            # ribbon breaks
            cbind(i,
                  ymin_seq[order(ribbon_seq)],
                  ymax_seq[order(ribbon_seq)])
        }
        alluvia <- do.call(rbind, lapply(1:length(axis_ind), compute_alluvium))
        colnames(alluvia) <- c("x", "ymin", "ymax")
        data <- data.frame(data, alluvia)
        # widths and x bounds
        data$xmin <- data$x - axis_width / 2
        data$xmax <- data$x + axis_width / 2
        data$width <- axis_width
        # y centers
        data$y <- (data$ymin + data$ymax) / 2
        data
    }
)

#' @rdname alluvium
#' @usage NULL
#' @export
stat_alluvium <- function(mapping = NULL, data = NULL, geom = "alluvium",
                          position = "identity", na.rm = FALSE,
                          show.legend = NA, inherit.aes = TRUE, ...) {
    layer(
        stat = StatAlluvium, data = data, mapping = mapping, geom = geom,
        position = position, show.legend = show.legend,
        inherit.aes = inherit.aes,
        params = list(na.rm = na.rm, ...)
    )
}

#' @rdname alluvium
#' @usage NULL
#' @export
GeomAlluvium <- ggproto(
    "GeomAlluvium", Geom,
    default_aes = aes(size = .5, linetype = 1,
                      colour = 0, fill = "gray", alpha = .5),
    setup_data = function(data, params) data,
    draw_group = function(data, panel_scales, coord,
                          ribbon_bend = 1/6) {
        first_row <- data[1, setdiff(names(data), c("x", "xmin", "xmax",
                                                    "y", "ymin", "ymax",
                                                    "width")),
                          drop = FALSE]
        rownames(first_row) <- NULL
        if (nrow(data) == 1) {
            # spline coordinates (one axis)
            spline_data <- data.frame(
                x = data$x + data$width / 2 * c(-1, 1, 1, -1),
                y = data$ymin + first_row$weight * c(0, 0, 1, 1),
                shape = rep(0, 4)
            )
        } else {
            # spline coordinates (more than one axis)
            x_oneway <- rep(data$x, c(3, rep(4, nrow(data) - 2), 3)) +
                rep(data$width, c(3, rep(4, nrow(data) - 2), 3)) / 2 *
                c(-1, rep(c(1, 1, -1, -1), nrow(data) - 1), 1) +
                ribbon_bend * c(0, rep(c(0, 1, -1, 0), nrow(data) - 1), 0)
            y_oneway <- rep(data$ymin, c(3, rep(4, nrow(data) - 2), 3))
            shape_oneway <- c(0, rep(c(0, 1, 1, 0), nrow(data) - 1), 0)
            spline_data <- data.frame(
                x = c(x_oneway, rev(x_oneway)),
                y = c(y_oneway, rev(y_oneway) + first_row$weight),
                shape = rep(shape_oneway, 2)
            )
        }
        data <- data.frame(first_row, spline_data)
        # transform (after calculating spline paths)
        coords <- coord$transform(data, panel_scales)
        # graphics object
        grid::xsplineGrob(
            x = coords$x, y = coords$y, shape = coords$shape,
            open = FALSE,
            gp = grid::gpar(
                col = coords$colour, fill = coords$fill, alpha = coords$alpha,
                lty = coords$linetype, lwd = coords$size * .pt
            )
        )
    },
    draw_key = draw_key_polygon
)

#' @rdname alluvium
#' @usage NULL
#' @export
geom_alluvium <- function(mapping = NULL, data = NULL, stat = "alluvium",
                          na.rm = FALSE, show.legend = NA, inherit.aes = TRUE,
                          ...) {
    layer(
        geom = GeomAlluvium, mapping = mapping, data = data, stat = stat,
        position = "identity", show.legend = show.legend,
        inherit.aes = inherit.aes,
        params = list(na.rm = na.rm, ...)
    )
}
