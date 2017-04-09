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
    stop("Non-alluvial data fed to 'na_keep'.")
  } else if (type == "lodes") {
    if (is.factor(data$stratum)) {
      data$stratum <- addNA(data$stratum, ifany = TRUE)
    } else {
      data$stratum[is.na(data$stratum)] <- "NA"
    }
  } else {
    axis_ind <- get_axes(names(data))
    for (i in axis_ind) {
      if (any(is.na(data[[i]]))) {
        if (is.factor(data[[i]])) {
          data[[i]] <- addNA(data[[i]], ifany = TRUE)
        } else {
          data[[i]][is.na(data[[i]])] <- "NA"
        }
      }
    }
  }
  data
}

# automatically summarize over numeric, character, and factor fields
auto_aggregate <- function(data, by) {
  agg <- aggregate(x = rep(1, nrow(data)),
                   by = data[, by],
                   FUN = unique)
  agg[[3]] <- NULL
  agg_vars <- setdiff(names(data), by)
  for (var in agg_vars) {
    agg2 <- aggregate(x = data[[var]],
                     by = data[, by],
                     FUN = if (var %in% c("size", "linetype",
                                          "fill", "color", "alpha",
                                          "PANEL", "group")) {
                       only
                     } else {
                       agg_fn(data[[var]])
                     })
    names(agg2) <- c(by, var)
    agg <- merge(agg, agg2, by = by, all = TRUE)
  }
  agg[do.call(order, agg[, by]), ]
}

# single unique value, or else NA
only <- function(x) {
  uniq <- unique(x)
  if (length(uniq) == 1) {
    uniq
  } else {
    NA
  }
}

# select aggregation function based on variable type
agg_fn <- function(x) {
  if (is.character(x) | is.factor(x)) {
    function(y) {
      uniq <- unique(y)
      if (length(uniq) == 1) as.character(uniq) else NA
    }
  } else if (is.numeric(x)) {
    sum
  } else {
    function(y) NA
  }
}
