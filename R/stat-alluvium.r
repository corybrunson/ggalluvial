#' Alluvial positions
#' 
#' Given a dataset with alluvial structure, \code{stat_alluvium} calculates the 
#' centroids (\code{x} and \code{y}) of the \strong{lodes}, the intersections of
#' the alluvia with the strata, together with their weights (heights; 
#' \code{ymin} and \code{ymax}). It leverages the \code{group} aesthetic for 
#' plotting purposes (for now).
#' 
#' @section Aesthetics: \code{stat_alluvium} understands the following
#'   aesthetics (required aesthetics are in bold):
#' \itemize{
#'   \item \code{x}
#'   \item \code{stratum}
#'   \item \code{alluvium}
#'   \item \code{axis[0-9]*} (\code{axis1}, \code{axis2}, etc.)
#'   \item \code{weight}
#'   \item \code{group}
#' }
#' Currently, \code{group} is ignored.
#' Use \code{x}, \code{stratum}, and \code{alluvium} for data in lode form and 
#' \code{axis[0-9]*} for data in alluvium form (see \code{\link{is_alluvial}});
#' arguments to parameters inconsistent with the data format will be ignored.
#' 
#' @name stat-alluvium
#' @import ggplot2
#' @seealso \code{\link[ggplot2]{layer}} for additional arguments,
#'   \code{\link{geom_alluvium}} for the corresponding geom,
#'   \code{\link{stat_stratum}} and \code{\link{geom_stratum}} for
#'   intra-axis boxes, and
#'   \code{\link{ggalluvial}} for a shortcut method.
#' @inheritParams stat-stratum
#' @param aggregate.wts Logical; whether to aggregate weights across otherwise
#'   equivalent rows before computing lode and flow positions. Defaults to TRUE.
#' @param lode.guidance The function to prioritize the axis variables for 
#'   ordering the lodes within each stratum. Defaults to "zigzag", other options
#'   include "rightleft", "leftright", "rightward", and "leftward" (see 
#'   \code{\link{lode-guidance-functions}}).
#' @param aes.bind Whether to prioritize aesthetics before axes (other than the
#'   index axis) when ordering the lodes within each stratum. Defaults to FALSE.
#' @param lode.ordering A list (of length the number of axes) of integer vectors
#'   (each of length the number of rows of \code{data}) or NULL entries 
#'   (indicating no imposed ordering), or else a numeric matrix of corresponding
#'   dimensions, giving the preferred ordering of alluvia at each axis. This 
#'   will be used to order the lodes within each stratum by sorting the lodes 
#'   first by stratum and then by the provided vectors.
#' @example inst/examples/ex-stat-alluvium.r
#' @usage NULL
#' @export
stat_alluvium <- function(mapping = NULL,
                          data = NULL,
                          geom = "alluvium",
                          decreasing = NA,
                          na.rm = FALSE,
                          show.legend = NA,
                          inherit.aes = TRUE,
                          ...) {
  layer(
    stat = StatAlluvium,
    data = data,
    mapping = mapping,
    geom = geom,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      decreasing = decreasing,
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname stat-alluvium
#' @usage NULL
#' @export
StatAlluvium <- ggproto(
  "StatAlluvium", Stat,
  
  setup_params = function(data, params) {
    
    if (!is.null(params$lode.ordering)) {
      if (is.list(params$lode.ordering)) {
        # replace any null entries with uniform NA vectors
        wh_null <- which(sapply(params$lode.ordering, is.null))
        len <- unique(sapply(params$lode.ordering[wh_null], length))
        if (length(len) > 1) stop("Lode orderings have different lengths.")
        for (w in wh_null) params$lode.ordering[[w]] <- rep(NA, len)
        # convert list to array (requires equal-length numeric entries)
        params$lode.ordering <- do.call(cbind, params$lode.ordering)
      }
    }
    
    params
  },
  
  setup_data = function(data, params) {
    
    # assign 'stratum' to 'alluvium' if 'alluvium' not provided, and vice-versa
    if (is.null(data$alluvium) & !is.null(data$stratum)) {
      data <- transform(data, alluvium = stratum)
    } else if (is.null(data$stratum) & !is.null(data$alluvium)) {
      data <- transform(data, stratum = alluvium)
    }
    
    # assign uniform weight if not provided
    if (is.null(data$weight)) {
      data$weight <- rep(1, nrow(data))
    }
    type <- get_alluvial_type(data)
    if (type == "none") {
      stop("Data is not in a recognized alluvial form ",
           "(see `?is_alluvial` for details).")
    }
    
    if (params$na.rm) {
      data <- na.omit(object = data)
    } else {
      data <- na_keep(data = data, type = type)
    }
    
    # ensure that data is in lode form
    if (type == "alluvia") {
      axis_ind <- get_axes(names(data))
      data <- to_lodes(data = data,
                       key = "x", value = "stratum", id = "alluvium",
                       axes = axis_ind)
      # positioning requires numeric 'x'
      data$x <- as.numeric(as.factor(data$x))
    }
    
    data
  },
  
  compute_panel = function(self, data, scales,
                           decreasing = NA,
                           aggregate.wts = TRUE,
                           lode.guidance = "zigzag",
                           aes.bind = FALSE,
                           lode.ordering = NULL) {
    
    # aggregate up to weight
    if (aggregate.wts) {
      data <- aggregate(x = data$weight,
                        by = data[, setdiff(names(data), "weight")],
                        FUN = sum)
      names(data)[ncol(data)] <- "weight"
    }
    
    # sort data by 'x' then 'alluvium' (to match 'alluv')
    data <- data[do.call(order, data[, c("x", "alluvium")]), ]
    
    if (is.null(lode.ordering)) {
      lode_fn <- get(paste0("lode_", lode.guidance))
    } else {
      # check that array has correct dimensions
      stopifnot(dim(lode.ordering) ==
                  c(dplyr::n_distinct(data$alluvium),
                    dplyr::n_distinct(data$x)))
    }
    
    # aesthetic fields
    aes_col <- setdiff(names(data),
                       c("x", "stratum", "alluvium",
                         "weight", "PANEL", "group"))
    # put axis fields into alluvial form, according to 'decreasing' parameter
    if (is.na(decreasing)) {
      alluv <- alluviate(data, "x", "stratum", "alluvium")
    } else if (decreasing) {
      alluv <- alluviate(data, "x", "weight", "alluvium")
      alluv <- transform(alluv, weight = -weight)
    } else {
      alluv <- alluviate(data, "x", "weight", "alluvium")
    }
    # sort by 'alluvium' (to match 'data')
    alluv <- alluv[order(alluv$alluvium), ]
    # axis and aesthetic indices
    axis_ind <- which(!(names(alluv) %in% names(data)))
    
    # vertical positions of flows at each axis
    position_lodes <- function(i) {
      # defined rows
      wh_def <- which(!is.na(alluv[[axis_ind[i]]]))
      # depends on whether the user has provided a lode.ordering
      if (is.null(lode.ordering)) {
        # order axis indices
        axis_seq <- axis_ind[lode_fn(n = length(axis_ind), i = i)]
        # order lodes according to axes and aesthetics
        lode_seq <- do.call(
          order,
          if (aes.bind) {
            cbind(alluv[wh_def, axis_seq[1], drop = FALSE],
                  subset(data, x == names(alluv)[axis_ind[i]])[aes_col],
                  alluv[wh_def, axis_seq[-1], drop = FALSE])
          } else {
            alluv[wh_def, axis_seq, drop = FALSE]
          }
        )
      } else {
        lode_seq <- order(alluv[[axis_ind[i]]][wh_def],
                          lode.ordering[wh_def, i])
      }
      # lode floors and ceilings along axis
      subdata <- subset(data, x == names(alluv)[axis_ind[i]])
      cumweight <- cumsum(subdata$weight[lode_seq])
      ymin_seq <- c(0, cumweight)
      ymax_seq <- c(cumweight, sum(subdata$weight[lode_seq]))
      # lode breaks
      data.frame(x = I(names(alluv)[axis_ind[i]]),
                 ymin = ymin_seq[order(lode_seq)],
                 ymax = ymax_seq[order(lode_seq)])
    }
    lode_positions <- do.call(rbind, lapply(1:length(axis_ind), position_lodes))
    data <- cbind(data, lode_positions[, -1])
    stopifnot(all(data$weight == data$ymax - data$ymin))
    
    # add vertical centroids and 'group' to encode alluvia
    data <- transform(data,
                      y = (ymin + ymax) / 2,
                      group = as.numeric(alluvium))
    
    data
  }
)

# build alluvial dataset for reference during lode-ordering
alluviate <- function(data, key, value, id) {
  to_alluvia(
    data[, c(key, value, id)],
    key = key, value = value, id = id
  )
}
