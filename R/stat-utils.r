# Identify elements in a character vector that fit the pattern of axis aesthetic
# names, and return their indices in the numerical order of the axis numbers
# (with \code{axis} first, if present). Only non-negative integers are allowed.
get_axes <- function(x) {
  stopifnot(dplyr::n_distinct(x) == length(x))
  axis_ind <- grep("^axis[0-9]*$", x)
  axis_ind[order(as.numeric(gsub("^axis", "", x[axis_ind])), na.last = FALSE)]
}

get_alluvial_type <- function(data) {
  # ensure that data is alluvial
  if (!is.null(data$x) | !is.null(data$stratum) | !is.null(data$alluvium)) {
    if (is.null(data$x) | is.null(data$stratum) | is.null(data$alluvium)) {
      stop("Parameters 'x', 'stratum', and 'alluvium' are required ",
           "for data in lode form.")
    }
    return(is_alluvial_lodes(data,
                             key = "x", value = "stratum", id = "alluvium",
                             weight = "weight",
                             logical = FALSE))
  } else {
    axis_ind <- get_axes(names(data))
    return(is_alluvial_alluvia(data,
                               axes = axis_ind,
                               weight = "weight",
                               logical = FALSE))
  }
}

# incorporate any missing values into factor levels
na_keep <- function(data, type) {
  if (type == "none") {
    stop("Data is not in a recognized alluvial form ",
         "(see `help(is_alluvial)` for details).")
  } else if (type == "lodes") {
    if (is.factor(data$stratum)) {
      data$stratum <- addNA(data$stratum, ifany = TRUE)
    } else {
      data$stratum[is.na(data$stratum)] <- ""
    }
  } else {
    axis_ind <- get_axes(names(data))
    for (i in axis_ind) {
      if (any(is.na(data[[i]]))) {
        if (is.factor(data[[i]])) {
          data[[i]] <- addNA(data[[i]], ifany = TRUE)
        } else {
          data[[i]][is.na(data[[i]])] <- ""
        }
      }
    }
  }
  data
}

# arrange data by aesthetics for consistent (reverse) z-ordering
z_order_colors <- function(data) {
  stopifnot("group" %in% names(data))
  # arrange by fill, then color
  z_aes <- intersect(c("fill", "colour", "group"), names(data))
  if (length(z_aes) > 0) {
    data <- data[do.call(order, data[, z_aes, drop = FALSE]), , drop = FALSE]
    data$group <- cumsum(!duplicated(data$group))
  }
  data
}
