#' Variable axes and strata
#' 
#' \code{stat_stratum} calculates the centers of the levels at each axis.
#' \code{geom_stratum} stacks a box for each level of a variable at its axis.
#' 
#' @seealso \code{\link{geom_alluvium}} for inter-axis flows.
#' @export
#' @inheritParams layer
#' @param axis_width The width of each variable axis, as a proportion of the
#'   separation between axes.

StatStratum <- ggproto(
    "StatStratum", Stat,
    required_aes = c("freq"),
    setup_data = function(data, params) {
        # aggregate freq over axes
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
        # stack axis-aggregated data with cumulative frequencies
        # (might want to parametrize the option to "hide" rather than "collapse"
        # but not sure if anyone would ever use the former option)
        if (FALSE) {
            # hide version
            stratum_data <- do.call(rbind, lapply(1:length(axis_ind), function(i) {
                agg <- aggregate(x = data$freq, by = data[axis_ind[i]], FUN = sum)
                names(agg) <- c("label", "freq")
                cbind(pos = i, agg, cumfreq = cumsum(agg$freq))
            }))
            # add panels and groups
            res_data <- do.call(rbind, lapply(unique(data$PANEL), function(p) {
                cbind(stratum_data, PANEL = p, group = 1:nrow(stratum_data))
            }))
        } else {
            # collapse version
            res_data <- do.call(rbind, lapply(unique(data$PANEL), function(p) {
                p_data <- subset(data, PANEL == p)
                do.call(rbind, lapply(1:length(axis_ind), function(i) {
                    agg <- aggregate(x = p_data$freq, by = p_data[axis_ind[i]],
                                     FUN = sum)
                    names(agg) <- c("label", "freq")
                    cbind(pos = i, agg, cumfreq = cumsum(agg$freq), PANEL = p)
                }))
            }))
            # add group
            res_data$group <- 1:nrow(res_data)
        }
        res_data
    },
    compute_group = function(data, scales) {
        rownames(data) <- NULL
        rect_data <- data.frame(x = data$pos,
                                y = (data$cumfreq - data$freq / 2))
        data.frame(data, rect_data)
        #box_data <- data.frame(
        #    x = (data$pos + axis_width / 2 * c(-1, 1))[c(1, 1, 2, 2)],
        #    y = (data$cumfreq - data$freq * c(1, 0))[c(1, 2, 2, 1)]
        #)
        #data.frame(data, box_data)
    }
)

stat_stratum <- function(mapping = NULL, data = NULL, geom = "stratum",
                          na.rm = FALSE, show.legend = NA,
                          inherit.aes = TRUE, ...) {
    layer(
        stat = StatStratum, data = data, mapping = mapping, geom = geom,
        position = "identity", show.legend = show.legend,
        inherit.aes = inherit.aes,
        params = list(na.rm = na.rm, ...)
    )
}

GeomStratum <- ggproto(
    "GeomStratum", GeomRect,
    required_aes = c("freq"),
    default_aes = aes(size = .5, linetype = 1,
                      colour = "black", fill = "white", alpha = 1),
    setup_data = function(data, params,
                          axis_width = 1/3) {
        transform(data,
                  xmin = x - axis_width / 2, xmax = x + axis_width / 2,
                  ymin = y - freq / 2, ymax = y + freq / 2)
    },
    draw_group = function(data, panel_scales, coord) {
        # reproducing Wickham's GeomBar hack
        ggproto_parent(GeomRect, self)$draw_panel(data, panel_scales, coord)
        #coords <- coord$transform(data, panel_scales)
        #grid::polygonGrob(
        #    x = coords$x, y = coords$y,
        #    gp = grid::gpar(
        #        col = coords$colour, fill = coords$fill, alpha = coords$alpha,
        #        lty = coords$linetype, lwd = coords$size * .pt
        #    )
        #)
    },
    draw_key = draw_key_polygon
)

geom_stratum <- function(mapping = NULL, data = NULL, stat = "stratum",
                          na.rm = FALSE, show.legend = NA, inherit.aes = TRUE,
                          ...) {
    layer(
        geom = GeomStratum, mapping = mapping, data = data, stat = stat,
        position = "identity", show.legend = show.legend,
        inherit.aes = inherit.aes, params = list(na.rm = na.rm, ...)
    )
}
