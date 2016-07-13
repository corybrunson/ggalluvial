#' Quick alluvial diagrams and formula interface
#' 
#' Produces an alluvial diagram with flows, boxes, and labels, optionally based
#' on a formula in terms of the data elements
#' 
#' @name ggalluvial
#' @import ggplot2
#' @seealso \code{\link{alluvium}} and \code{\link{stratum}}
#' @export
#' @param ... arguments passed to \code{ggplot} and inherited by 
#'   \code{geom_alluvium} and \code{geom_stratum}.
#' @param formula a formula to specify the axes and alluvial divisions
#' @param data a data frame or frequency table
#' @param weight a weight variable, from \code{data} or of compatible length 
#'   with the elements of \code{formula}
ggalluvial <- function(...) {
    input_list <- list(...)
    if (!is.null(input_list[["formula"]]) |
        ((is.null(names(input_list)) | input_list[[1]] == "") &
         is.call(input_list[[1]]))) {
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
    axis_input <- aes_input[grep("^axis[0-9\\.]$", names(aes_input))]
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
    
    dep_incl <- (length(formula) == 3)
    if (dep_incl & length(all.vars(formula[[2]])) > 1) {
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
    formula_axes <- all.vars(formula[[2 + dep_incl]])
    for (i in 1:length(formula_axes)) {
        formula_aes[[paste0("axis", i)]] <- as.name(formula_axes[i])
    }
    if (dep_incl) formula_aes[["fill"]] <- as.name(all.vars(formula[[2]]))
    
    ggalluvial.default(luv_data, formula_aes, ...)
}
