# Identify elements in a character vector that fit the pattern of axis aesthetic
# names, and return their indices in the numerical order of the axis numbers
# (with `axis` first, if present). Only non-negative integers are allowed.
get_axes <- function(x) {
  if (anyDuplicated(x)) {
    dupes <- unique(x[duplicated(x)])
    stop("Duplicated variables: ", paste(dupes, collapse = ", "))
  }
  axis_ind <- grep("^axis[0-9]*$", x)
  axis_ind[order(as.numeric(gsub("^axis", "", x[axis_ind])), na.last = FALSE)]
}

get_alluvial_type <- function(data) {
  # ensure that data is alluvial
  if (!is.null(data$x) | !is.null(data$stratum) | !is.null(data$alluvium)) {
    if (is.null(data$x) | is.null(data$stratum) | is.null(data$alluvium)) {
      stop("Parameters `x`, `stratum`, and `alluvium` are required ",
           "for data in lode form.")
    }
    if (is_lodes_form(data,
                      key = "x", value = "stratum", id = "alluvium",
                      weight = "y",
                      silent = TRUE)) return("lodes")
  } else {
    axis_ind <- get_axes(names(data))
    if (is_alluvia_form(data,
                        axes = axis_ind,
                        weight = "y",
                        silent = TRUE)) return("alluvia")
  }
  return("none")
}

# incorporate any missing values into factor levels
na_keep <- function(data, type) {
  if (type == "none") {
    stop("Data is not in a recognized alluvial form ",
         "(see `help('alluvial-data')` for details).")
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

# replace a vector `x` of any type with
# a numeric vector of *contiguous* integers that sort in the same order as `x`
contiguate <- function(x) {
  x <- xtfrm(x)
  match(x, sort(unique(x)))
}

# build alluvial dataset for reference during lode-ordering
alluviate <- function(data, key, value, id) {
  to_alluvia_form(
    data[, c(key, value, id)],
    key = key, value = value, id = id
  )
}

# define 'deposit' variable to rank strata vertically
deposit_data <- function(data, decreasing, reverse, absolute) {
  if (is.na(decreasing)) {
    deposits <- unique(data[, c("x", "yneg", "stratum")])
    deposits <- transform(deposits, deposit = order(order(
      x, -yneg,
      xtfrm(stratum) * (-1) ^ (yneg * absolute + reverse)
    )))
  } else {
    deposits <- stats::aggregate(
      x = data$y,
      by = data[, c("x", "yneg", "stratum"), drop = FALSE],
      FUN = sum
    )
    names(deposits)[ncol(deposits)] <- "y"
    deposits <- transform(deposits, deposit = order(order(
      x, yneg,
      xtfrm(y) * (-1) ^ (yneg * absolute + decreasing),
      xtfrm(stratum) * (-1) ^ (yneg * absolute + reverse)
    )))
    deposits$y <- NULL
  }
  merge(data, deposits, all.x = TRUE, all.y = FALSE)
}

# define 'deposit' variable to rank strata outward from zero (pos then neg)
# -+- originally used to simplify 'ycum' calculation; deprecated -+-
deposit_data_abs <- function(data, decreasing, reverse, absolute) {
  if (is.na(decreasing)) {
    deposits <- unique(data[, c("x", "yneg", "stratum")])
    deposits <- transform(deposits, deposit = order(order(
      x, yneg,
      xtfrm(stratum) * (-1) ^ (reverse + yneg * ! absolute)
    )))
  } else {
    deposits <- stats::aggregate(
      x = data$y,
      by = data[, c("x", "yneg", "stratum"), drop = FALSE],
      FUN = sum
    )
    names(deposits)[ncol(deposits)] <- "y"
    deposits <- transform(deposits, deposit = order(order(
      x, yneg,
      xtfrm(y) * (-1) ^ (decreasing + yneg * ! absolute),
      xtfrm(stratum) * (-1) ^ (reverse + yneg * ! absolute)
    )))
    deposits$y <- NULL
  }
  merge(data, deposits, all.x = TRUE, all.y = FALSE)
}

# calculate cumulative 'y' values, accounting for sign
cumulate <- function(x) {
  if (length(x) == 0) return(x)
  s <- unique(sign(x))
  stopifnot(length(s) == 1 && s %in% c(-1, 1))
  if (s == 1) {
    cumsum(x) - x / 2
  } else {
    rev(cumsum(rev(x))) - rev(x) / 2
  }
}

# arrange data by aesthetics for consistent (reverse) z-ordering
z_order_aes <- function(data, aesthetics) {
  
  # `aesthetics` and 'group' are fixed within contiguous alluvial segments
  aes_data <- data[! duplicated(data[, c("alluvium", "group")]),
                   c("alluvium", aesthetics, "group")]
  if (length(aes_data) == 2) return(data)
  aes_data <- aes_data[do.call(order, aes_data[, c(aesthetics, "alluvium")]), ]
  
  # ensure order of "group" respects aesthetics
  data$group <- match(data$group, unique(aes_data$group))
  data[with(data, order(x, group)), , drop = FALSE]
}
