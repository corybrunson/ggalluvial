#' Alluvial flows
#' 
#' \code{stat_alluvium} calculates the depth of each group at each axis. 
#' \code{geom_alluvium} plots an x-spline for each group through the axes at 
#' these depths.
#' 
#' @seealso \code{\link{geom_stratum}} for intra-axis boxes.
#' @export
#' @inheritParams layer
#' @param axis_width The width of each variable axis, as a proportion of the
#'   separation between axes.
#' @param ribbon_bend The horizontal distance between a variable axis
#'   (\code{axis_width/2} from its center) and the control point of the
#'   x-spline, also as a proportion of the separation between the axes.

StatAlluvium <- ggproto(
    "StatAlluvium", Stat,
    # frequency
    required_aes = c("freq"),
    # add axis-specific y0 and y1 params
    setup_data = function(self, data, params) {
        # aggregate freq over axes and panels
        data <- aggregate(
            formula = as.formula(paste("freq ~",
                                       paste(setdiff(names(data), "freq"),
                                             collapse = "+"))),
            data = data, FUN = sum
        )
        # identify axes (in numerical order)
        axis_ind <- grep("^axis[0-9\\.]*$", names(data))
        axis_ind <- axis_ind[order(as.numeric(gsub("^axis", "",
                                                   names(data)[axis_ind])))]
        # vertical floors at each axis, by panel
        # (might want to parametrize the option to "hide" rather than "collapse"
        # but not sure if anyone would ever use the former option)
        if (FALSE) {
            # hide version
            alluvium_data <- as.data.frame(
                do.call(rbind, lapply(1:length(axis_ind), function(i) {
                    # order axis indices
                    axis_seq <- axis_ind[zigzag(n = length(axis_ind), i = i)]
                    # order ribbons according to axes, in above order
                    ribbon_seq <- do.call(order, data[axis_seq])
                    # ribbon increments along axis
                    incrs <- c(0, cumsum(data$freq[ribbon_seq]))
                    # ribbon breaks in original order (omits last)
                    cbind(i, incrs[order(ribbon_seq)])
                }))
            )
            # axis column name
            names(alluvium_data) <- c("pos", "y0")
            # combine with original data
            res_data <- data.frame(data, alluvium_data)
        } else {
            # collapse version
            res_data <- do.call(rbind, lapply(unique(data$PANEL), function(p) {
                p_data <- subset(data, PANEL == p)
                rownames(p_data) <- NULL
                p_all <- do.call(rbind, lapply(1:length(axis_ind), function(i) {
                    # order axis indices
                    axis_seq <- axis_ind[zigzag(n = length(axis_ind), i = i)]
                    # order ribbons according to axes, in above order
                    ribbon_seq <- do.call(order, p_data[axis_seq])
                    # ribbon increments along axis
                    incrs <- c(0, cumsum(p_data$freq[ribbon_seq]))
                    # ribbon breaks in original order (omits last)
                    cbind(i, incrs[order(ribbon_seq)])
                }))
                # axis column name
                colnames(p_all) <- c("pos", "y0")
                # combine with original data
                data.frame(p_data, p_all)
            }))
        }
        res_data
    },
    # calculate coordinates governing ribbon segments
    compute_group = function(data, scales, params,
                             # axes to be placed at positive integers
                             axis_width = 1/3, ribbon_bend = 1/6) {
        first_row <- data[1, setdiff(names(data), c("pos", "y0")),
                          drop = FALSE]
        rownames(first_row) <- NULL
        # spline coordinates (one axis)
        if (nrow(data) == 1) {
            spline_data <- data.frame(
                x = data$pos + axis_width / 2 * c(-1, 1, 1, -1),
                y = data$y0 + first_row$freq * c(0, 0, 1, 1),
                shape = rep(0, 4)
            )
            return(data.frame(first_row, spline_data))
        }
        # spline coordinates (more than one axis)
        x_oneway <- rep(data$pos, c(3, rep(4, nrow(data) - 2), 3)) +
            axis_width / 2 * c(-1, rep(c(1, 1, -1, -1), nrow(data) - 1), 1) +
            ribbon_bend * c(0, rep(c(0, 1, -1, 0), nrow(data) - 1), 0)
        y_oneway <- rep(data$y0, c(3, rep(4, nrow(data) - 2), 3))
        shape_oneway <- c(0, rep(c(0, 1, 1, 0), nrow(data) - 1), 0)
        spline_data <- data.frame(
            x = c(x_oneway, rev(x_oneway)),
            y = c(y_oneway, rev(y_oneway) + first_row$freq),
            shape = rep(shape_oneway, 2)
        )
        data.frame(first_row, spline_data)
    }
)

stat_alluvium <- function(mapping = NULL, data = NULL, geom = "alluvium",
                         position = "identity", na.rm = FALSE, show.legend = NA,
                         inherit.aes = TRUE, ...) {
    layer(
        stat = StatAlluvium, data = data, mapping = mapping, geom = geom,
        position = position, show.legend = show.legend,
        inherit.aes = inherit.aes,
        params = list(na.rm = na.rm, ...)
    )
}

GeomAlluvium <- ggproto(
    "GeomAlluvium", Geom,
    required_aes = c("freq"),
    default_aes = aes(size = .5, linetype = 1,
                      colour = 0, fill = "gray", alpha = .5),
    draw_group = function(data, panel_scales, coord) {
        coords <- coord$transform(data, panel_scales)
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
