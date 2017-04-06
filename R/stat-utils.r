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
