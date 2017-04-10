#' Quick alluvial diagrams and formula interface
#' 
#' Produces an alluvial diagram with flows, boxes, and labels, optionally based
#' on a formula in terms of the data elements
#' 
#' @name ggalluvial
#' @import ggplot2
#' @seealso \code{\link{stat_stratum}}, \code{\link{geom_stratum}},
#'   \code{\link{stat_alluvium}}, and \code{\link{geom_alluvium}}
#' @export
#' @param ... Arguments passed to \code{ggplot} and inherited by 
#'   \code{geom_alluvium} and \code{geom_stratum}.
#' @param formula A formula to specify the axes and alluvial divisions
#' @param data A data frame or frequency table
#' @param weight A weight variable, from \code{data} or of compatible length 
#'   with the elements of \code{formula}
ggalluvial <- function(...) {
  input_list <- list(...)
  if (!is.null(input_list[["formula"]]) | any(sapply(input_list, is.call))) {
    ggalluvial.formula(...)
  } else {
    ggalluvial.default(...)
  }
}

#' @rdname ggalluvial
#' @export
ggalluvial.default <- function(...) {
  input_list <- list(...)
  aes_input <- input_list[[which(sapply(input_list, class) == "uneval")]]
  axis_input <- aes_input[grep("^axis[0-9]*$", names(aes_input))]
  axis_breaks <- as.numeric(gsub("^axis", "", names(axis_input)))
  axis_labels <- unname(as.character(axis_input))
  ggplot(...) +
    geom_alluvium() +
    geom_stratum() +
    geom_text(stat = "stratum") +
    scale_x_continuous(breaks = axis_breaks, labels = axis_labels)
}

#' @rdname ggalluvial
#' @export
ggalluvial.formula <- function(formula, data = NULL, weight, ...) {
  formula <- stats::as.formula(formula)
  if (!is.null(data)) {
    if (!is.data.frame(data)) {
      data <- as.data.frame(data)
      if (!missing(weight)) {
        warning("Using weights from table; 'weight' argument ignored")
      }
      weight <- "Freq"
    }
  }
  
  rhs <- labels(stats::terms(formula))
  lhs <- setdiff(all.vars(formula), rhs)
  
  if (length(lhs) > 1) {
    stop("Multilpe variables on LHS of '%s'")
  }
  
  luv_data <- stats::model.frame(formula = formula, data = data)
  if (!missing(weight)) {
    if (is.character(weight)) {
      luv_data[[weight]] <- data[[weight]]
    } else {
      luv_data$weight <- weight
    }
  }
  
  formula_aes <- aes()
  if (!missing(weight)) formula_aes[["weight"]] <-
    if (is.character(weight)) as.name(weight) else as.name("weight")
  
  # choose categorical or time series format based on number of RHS variables
  dep_incl <- (length(formula) == 3)
  if (length(rhs) > 1) {
    
    formula_axes <- rhs
    for (i in 1:length(formula_axes)) {
      formula_aes[[paste0("axis", i)]] <- as.name(formula_axes[i])
    }
    if (dep_incl) formula_aes[["fill"]] <- as.name(lhs)
    
    ggalluvial.default(luv_data, formula_aes, ...)
    
  } else {
    
    formula_aes$x <- as.name(rhs)
    grp <- as.name(lhs)
    formula_aes$group <- grp
    formula_aes$fill <- grp
    formula_aes$colour <- grp
    
    ggplot(data = luv_data, mapping = formula_aes, ...) +
      geom_alluvium()
    
  }
}
