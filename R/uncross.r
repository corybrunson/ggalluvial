#' Uncross and recolor alluvial diagrams
#'
#' Alluvial diagrams are only as legible as their stratum order. The parameters
#' `sort_strata` and `color_strata`, recognized by [stat_stratum()],
#' [stat_alluvium()], and [stat_flow()], delegate the choice of stratum order
#' and of a shared stratum coloring to an *uncrossing engine* --- notably the
#' **wompwomp** package, which sorts the \eqn{k}-partite graph underlying an
#' alluvial diagram so as to (nearly) minimize the number of crossing flows.
#'
#' @section Choosing an engine:
#'
#'   Each of `sort_strata`, `color_strata`, and the `engine` argument of
#'   `sort_axes()` names an engine:
#'
#'   - `NULL` (the default): do nothing; strata are ordered and colored exactly
#'     as they were before.
#'   - `NA`: also do nothing, but ignore the package option, so that one layer
#'     opts out of a session-wide default.
#'   - An engine name, e.g. `"wompwomp"`, which uses that engine's default
#'     algorithm. [uncross_engines()] lists the registered engines.
#'   - A list whose first, unnamed element is an engine name and whose named
#'     elements are passed to the engine, e.g.
#'     `list("wompwomp", method = "neighbornet", alpha = 3)`. `method` selects
#'     the engine's algorithm; everything else is a tuning argument.
#'   - A function implementing the engine contract directly (see
#'     [register_uncross_engine()]), optionally wrapped in a list the same way.
#'
#'   Engines are looked up in a registry rather than hard-coded, so supporting
#'   a new package or algorithm takes one call to [register_uncross_engine()]
#'   and no change to the `stat_*()` layers.
#'
#' @section Engines:
#'
#'   **wompwomp** provides all three operations. Its `method` values are those
#'   of [wompwomp::sort_to_uncross()] for `sort_strata` (`"neighbornet"`, the
#'   default, `"tsp"`, `"greedy_wolf"`, `"barycenter"`, ...), its
#'   `column_method` values for `sort_axes()` (`"tsp"`, the default,
#'   `"neighbornet"`, ...), and those of [wompwomp::get_lode_clusters()] for
#'   `color_strata` (`"advanced"`, the default, `"left"`, `"right"`, ...). Its
#'   remaining tuning arguments travel alongside `method`: `alpha`, `beta`,
#'   `fixed_column`, `resolution`, `verbose`, and the `options` list built by
#'   [wompwomp::sort_to_uncross_options()] or
#'   [wompwomp::get_lode_clusters_options()], which is where the rest of
#'   wompwomp's knobs live.
#'
#'   Four engines need no other package and take no `method`. Each orders the
#'   strata within every axis independently, so unlike **wompwomp** they cannot
#'   reduce crossings; they are useful for imposing a predictable order, and
#'   for comparison.
#'
#'   - `"alphabetical"` and `"reverse_alphabetical"`: by the stratum values.
#'   - `"increasing"` and `"decreasing"`: by the total size of each stratum,
#'     breaking ties by value. Note that the `decreasing` *parameter* of the
#'     `stat_*()` layers already orders strata by size and takes precedence
#'     over any engine; these are the size orderings expressed as an engine, so
#'     that they compose with `reverse` the way every other engine does.
#'
#' @section Sorting strata:
#'
#'   `sort_strata` replaces the default ordering of the strata at each axis
#'   (the ordering induced by the values of the `stratum` aesthetic) with an
#'   ordering computed from the alluvial structure of the whole panel. It is
#'   applied before `decreasing`, `reverse`, and `absolute`, so those
#'   parameters retain their usual meanings: `reverse` still flips the computed
#'   order, and `decreasing` still promotes stratum size above it.
#'
#'   `sort_strata` must be given the same value in every layer of a plot, just
#'   as `decreasing`, `reverse`, and `absolute` must be: each layer computes
#'   its own positions, so sorting the strata of `geom_stratum()` alone would
#'   leave the flows of `geom_flow()` at the heights the default order gave
#'   them, detached from the boxes they belong to. Setting the package option
#'   is usually easier than repeating the parameter. Results are cached within
#'   a session, so the computation is still performed only once for a given
#'   panel, engine, and set of arguments.
#'
#'   Some algorithms are stochastic --- wompwomp's `"tsp"` is, since the solver
#'   it calls starts from a random tour, while its default `"neighbornet"` is
#'   not. The cache is what keeps a stochastic algorithm from ordering the
#'   strata differently in each layer of the same plot, but a *new session*
#'   will still produce a different (equally uncrossed) order. Use a
#'   deterministic algorithm, or `set.seed()` before the plot is first drawn,
#'   when a plot must reproduce exactly. [uncross_reset()] empties the cache.
#'
#'   Uncrossing needs to know which strata are linked, so it requires the
#'   `alluvium` aesthetic. `stat_stratum()` invents one for lodes-form data
#'   that provides none; rather than uncross links that are an artifact of row
#'   order, it warns and leaves the strata alone.
#'
#' @section Coloring strata:
#'
#'   `color_strata` assigns each stratum an integer cluster identifier, shared
#'   across axes, so that strata containing largely the same cases receive the
#'   same identifier. It is exposed as the computed variable `cluster` and is
#'   used via [`ggplot2::after_stat()`][ggplot2::aes_eval]:
#'   `aes(fill = after_stat(cluster))`. It is a factor, so it maps to a
#'   discrete fill or color scale.
#'
#'   Unlike `sort_strata`, `color_strata` moves nothing, so it need only be set
#'   in the layers that read `after_stat(cluster)`.
#'
#'   On `geom_alluvium()` a differentiation aesthetic must be constant along an
#'   alluvium; because `cluster` is a property of the stratum, not of the
#'   alluvium, use it with `geom_stratum()`, `geom_lode()`, or `geom_flow()`.
#'
#' @section Sorting axes:
#'
#'   The order of the axes themselves cannot be set from within a statistical
#'   transformation: position scales are trained and mapped before stats are
#'   computed, so permuting `x` inside a `stat_*()` would move the strata out
#'   from under their axis labels. `sort_axes()` is therefore an ordinary
#'   function that returns the recommended axis order, to be passed to
#'   [ggplot2::scale_x_discrete()] or to [to_lodes_form()].
#'
#'   The **wompwomp** engine's default here is `column_method = "tsp"`, which
#'   is stochastic in the same way as `sort_strata`'s `"tsp"`: repeated calls
#'   may return a tour and its reverse, which are equally uncrossed but order
#'   the axes oppositely. Seed it, or pass
#'   `list("wompwomp", method = "neighbornet")`, when the axis order must be
#'   reproducible.
#'
#'   Candidate axis orders are compared by the crossings they induce, which
#'   depends on how the strata are ordered, so the engine sorts the strata
#'   while it searches. That inner ordering is `"neighbornet"` by default and
#'   can be set with `strata_method`, as in
#'   `list("wompwomp", method = "tsp", strata_method = "greedy_wblf")`. Only
#'   the axis order is returned.
#'
#'   Applying the result with `scale_x_discrete(limits = )` changes the `x`
#'   positions that the `stat_*()` layers see, and it does so before they run,
#'   so `sort_strata` sorts the strata against the new axis order rather than
#'   the original one. This is what you want --- crossings occur between
#'   adjacent axes, so the best stratum order depends on which axes are
#'   adjacent --- but it does mean the strata of a plot with a reordered axis
#'   scale will not match those of the same plot without it. Sorting the axes
#'   and then the strata is also two passes over a problem that
#'   [wompwomp::sort_to_uncross()] can optimize jointly; call it directly on
#'   your data when you want the joint result.
#'
#' @section Package options:
#'
#'   - `ggalluvial.sort_strata`: default for the `sort_strata` parameter;
#'     defaults to `NULL`.
#'   - `ggalluvial.color_strata`: default for the `color_strata` parameter;
#'     defaults to `NULL`.
#'   - `ggalluvial.sort_axes`: default for the `engine` argument of
#'     `sort_axes()`; defaults to `NULL`.
#'
#'   See [base::options()] for how to use options.
#'
#' @name uncross
#' @family alluvial stat layers
#' @seealso [stat_stratum()], [stat_alluvium()], and [stat_flow()], which take
#'   the `sort_strata` and `color_strata` parameters.
#' @param data A data frame in lodes or alluvia form; see [alluvial-data].
#' @param axes For data in alluvia form, the axis variables, as a character or
#'   numeric vector. Leave `NULL` for data in lodes form.
#' @param key,value,id For data in lodes form, the axis, stratum, and alluvium
#'   variables, as in [to_alluvia_form()].
#' @param weight Optional; the variable holding the size of each row. Rows are
#'   weighted equally if `NULL`.
#' @param engine An engine, as described above. Defaults to the option
#'   `ggalluvial.sort_axes`.
#' @return `sort_axes()` returns a character vector of axis variable names, in
#'   the recommended order.
#' @example inst/examples/ex-uncross.r
NULL

# ------------------------------------------------------------------------
# engine registry
# ------------------------------------------------------------------------

.uncross_engines <- new.env(parent = emptyenv())

#' Register an uncrossing engine
#'
#' Makes an algorithm for ordering or coloring strata available to the
#' `sort_strata` and `color_strata` parameters of [stat_stratum()],
#' [stat_alluvium()], and [stat_flow()], and to [sort_axes()], under a name
#' that those parameters will recognize.
#'
#' Each of `sort_strata`, `sort_axes`, and `color_strata` is a function of four
#' arguments:
#'
#' \describe{
#'   \item{`wide`}{a data frame in alluvia form: one row per alluvium, one
#'     character column per axis (named by `axes`), and one numeric column
#'     (named by `weight`) holding the size of the alluvium.}
#'   \item{`axes`}{a character vector of the axis column names of `wide`, in
#'     plot order.}
#'   \item{`weight`}{the name of the weight column of `wide`.}
#'   \item{`method`}{the algorithm to use, or `NULL` for the engine's own
#'     default.}
#' }
#'
#' plus any additional arguments the caller passed alongside `method`. They
#' must return, respectively:
#'
#' \describe{
#'   \item{`sort_strata`}{a list, named by `axes`, of character vectors giving
#'     the strata at that axis in ascending order.}
#'   \item{`sort_axes`}{a character vector: a permutation of `axes`.}
#'   \item{`color_strata`}{a list, named by `axes`, of named vectors mapping
#'     each stratum at that axis to a cluster identifier.}
#' }
#'
#' @param name Character; the name under which to register the engine.
#' @param sort_strata,sort_axes,color_strata Functions implementing the
#'   contract above, or `NULL` if the engine does not provide that operation.
#' @return Invisibly, the registered engine, as a list.
#' @seealso [uncross] for how engines are chosen.
#' @export
register_uncross_engine <- function(name,
                                    sort_strata = NULL,
                                    sort_axes = NULL,
                                    color_strata = NULL) {
  stopifnot(is.character(name), length(name) == 1L, nzchar(name))
  for (f in list(sort_strata, sort_axes, color_strata)) {
    if (! is.null(f) && ! is.function(f)) {
      stop("Engine operations must be functions or `NULL`.")
    }
  }
  engine <- list(name = name,
                 sort_strata = sort_strata,
                 sort_axes = sort_axes,
                 color_strata = color_strata)
  assign(name, engine, envir = .uncross_engines)
  invisible(engine)
}

#' @rdname register_uncross_engine
#' @return `uncross_engines()` returns the names of the registered engines.
#' @export
uncross_engines <- function() sort(ls(.uncross_engines))

get_uncross_engine <- function(name) {
  if (! exists(name, envir = .uncross_engines, inherits = FALSE)) {
    stop("No uncrossing engine named '", name, "'. Registered engines: ",
         paste(uncross_engines(), collapse = ", "), ".")
  }
  get(name, envir = .uncross_engines, inherits = FALSE)
}

# Resolve an engine specification to a function, an algorithm, and extra
# arguments. `what` is the operation being requested. Returns `NULL` if the
# operation is switched off.
uncross_spec <- function(spec, what) {
  if (is.null(spec)) return(NULL)
  # `NA` switches the operation off for one layer, overriding the option
  if (! is.list(spec) && ! is.function(spec) &&
      length(spec) == 1L && is.na(spec)) {
    return(NULL)
  }

  args <- list()
  if (is.list(spec) && ! is.function(spec)) {
    if (length(spec) == 0L) return(NULL)
    nms <- names(spec) %||% rep("", length(spec))
    wh <- which(! nzchar(nms))
    if (length(wh) != 1L) {
      stop("A list passed to `", what, "` must have exactly one unnamed ",
           "element, naming the engine, as in ",
           "`list(\"wompwomp\", method = \"neighbornet\")`.")
    }
    args <- spec[-wh]
    spec <- spec[[wh]]
    # these are the engine contract's own parameters; matching one by name
    # would displace the positional arguments and bind them silently wrong
    clash <- intersect(names(args), c("wide", "axes", "weight"))
    if (length(clash) > 0) {
      stop("`", what, "` cannot take ",
           paste0("`", clash, "`", collapse = ", "), " as an argument.")
    }
  }

  method <- args$method
  args$method <- NULL
  if (! is.null(method) && (! is.character(method) || length(method) != 1L)) {
    stop("`method` must be a single string.")
  }

  if (is.function(spec)) {
    return(list(fun = spec, method = method, args = args,
                key = paste(deparse(spec), collapse = "\n")))
  }
  if (! is.character(spec) || length(spec) != 1L) {
    stop("`", what, "` must name an engine (one of ",
         paste(uncross_engines(), collapse = ", "),
         "), or be a function, a list, `NA`, or `NULL`.")
  }

  engine <- get_uncross_engine(spec)
  fun <- engine[[what]]
  if (is.null(fun)) {
    stop("Uncrossing engine '", spec, "' does not implement `", what, "`.")
  }
  list(fun = fun, method = method, args = args,
       key = paste0(spec, "\r", method %||% ""))
}

# The part of a resolved specification that determines its result, for use as
# a cache key. The function itself is excluded in favor of `key`, which
# identifies it without hashing its enclosing environment.
uncross_spec_key <- function(spec) list(spec$key, spec$method, spec$args)

# Run a resolved specification against the reshaped data.
uncross_call <- function(spec, parts) {
  do.call(spec$fun, c(
    list(parts$wide, parts$axes, parts$weight, spec$method),
    spec$args
  ))
}

# ------------------------------------------------------------------------
# reshaping lodes-form layer data to the engine contract
# ------------------------------------------------------------------------

# Sentinel written into cells of the alluvia-form matrix at which an alluvium
# has no stratum. Engines see it as an ordinary stratum; its results are
# discarded.
.uncross_gap <- "\u00a0(none)\u00a0"

# Reshape the lodes-form data of one panel into the alluvia form that engines
# consume. Returns `NULL` when there is nothing to uncross.
uncross_wide <- function(data) {
  if (is.null(data$x) || is.null(data$stratum) || is.null(data$alluvium)) {
    return(NULL)
  }
  xs <- sort(unique(data$x))
  if (length(xs) < 2L) return(NULL)

  ids <- unique(data$alluvium)
  axes <- paste0("axis", seq_along(xs))

  strata <- as.character(data$stratum)
  gap <- .uncross_gap
  while (gap %in% strata) gap <- paste0(gap, "\u00a0")

  mat <- matrix(gap, nrow = length(ids), ncol = length(xs),
                dimnames = list(NULL, axes))
  mat[cbind(match(data$alluvium, ids), match(data$x, xs))] <- strata

  # one weight per alluvium: the mean of its (unsigned) lode heights, which
  # reduces to the alluvium's height whenever it is constant across axes
  y <- if (is.null(data$y)) rep(1, nrow(data)) else abs(data$y)
  wts <- vapply(split(y, factor(match(data$alluvium, ids),
                                levels = seq_along(ids))),
                function(z) if (length(z) == 0L) 0 else mean(z),
                numeric(1))

  weight <- ".weight"
  while (weight %in% axes) weight <- paste0(weight, ".")

  wide <- as.data.frame(mat, stringsAsFactors = FALSE)
  wide[[weight]] <- unname(wts)
  wide <- wide[wide[[weight]] > 0, , drop = FALSE]
  if (nrow(wide) == 0L) return(NULL)

  list(wide = wide, axes = axes, weight = weight, gap = gap, x = xs)
}

# ------------------------------------------------------------------------
# memoization
# ------------------------------------------------------------------------

.uncross_cache <- new.env(parent = emptyenv())

# Engines may be stochastic and are often expensive; every layer of a plot
# recomputes the same panel. Memoize on the reshaped data and the resolved
# specification so that layers agree with each other and pay the cost once.
uncross_memo <- function(key, expr) {
  hash <- rlang::hash(key)
  if (exists(hash, envir = .uncross_cache, inherits = FALSE)) {
    return(get(hash, envir = .uncross_cache, inherits = FALSE))
  }
  res <- expr()
  if (length(ls(.uncross_cache)) >= 32L) rm(list = ls(.uncross_cache),
                                            envir = .uncross_cache)
  assign(hash, res, envir = .uncross_cache)
  res
}

# ------------------------------------------------------------------------
# the two operations used by the `stat_*()` layers
# ------------------------------------------------------------------------

# Ranks by which to order the strata at each axis, as an integer vector named
# by 'x' and 'stratum'. Returns `NULL` if `sort_strata` is `NULL` or if there
# is nothing to sort.
uncross_ranks <- function(data, sort_strata) {
  spec <- uncross_spec(sort_strata, "sort_strata")
  if (is.null(spec)) return(NULL)
  parts <- uncross_wide(data)
  if (is.null(parts)) return(NULL)

  key <- list("sort_strata", parts$wide, parts$axes, uncross_spec_key(spec))
  ord <- uncross_memo(key, function() {
    res <- uncross_call(spec, parts)
    if (! is.list(res) || ! all(parts$axes %in% names(res))) {
      stop("`sort_strata` must return a list of stratum orders ",
           "named by the axes.")
    }
    res
  })

  ranks <- integer(0)
  for (i in seq_along(parts$axes)) {
    lv <- setdiff(as.character(ord[[parts$axes[[i]]]]), parts$gap)
    if (length(lv) == 0L) next
    r <- stats::setNames(seq_along(lv), paste0(parts$x[[i]], "\r", lv))
    ranks <- c(ranks, r)
  }
  if (length(ranks) == 0L) NULL else ranks
}

# Cluster identifiers for the strata, as a vector named by 'x' and 'stratum'.
uncross_clusters <- function(data, color_strata) {
  spec <- uncross_spec(color_strata, "color_strata")
  if (is.null(spec)) return(NULL)
  parts <- uncross_wide(data)
  if (is.null(parts)) return(NULL)

  key <- list("color_strata", parts$wide, parts$axes, uncross_spec_key(spec))
  map <- uncross_memo(key, function() {
    res <- uncross_call(spec, parts)
    if (! is.list(res) || ! all(parts$axes %in% names(res))) {
      stop("`color_strata` must return a list of stratum clusters ",
           "named by the axes.")
    }
    res
  })

  clusters <- character(0)
  for (i in seq_along(parts$axes)) {
    m <- unlist(map[[parts$axes[[i]]]])
    m <- m[setdiff(names(m), parts$gap)]
    if (length(m) == 0L) next
    clusters <- c(clusters, stats::setNames(as.character(m),
                                            paste0(parts$x[[i]], "\r",
                                                   names(m))))
  }
  if (length(clusters) == 0L) NULL else clusters
}

# Attach the computed variable 'cluster' to `data`, as a factor whose levels
# are in the numerical order of the cluster identifiers when they are numbers.
uncross_attach_clusters <- function(data, clusters) {
  if (is.null(clusters)) return(data)
  val <- unname(clusters[paste0(data$x, "\r", as.character(data$stratum))])
  lv <- unique(val[! is.na(val)])
  num <- suppressWarnings(as.numeric(lv))
  lv <- if (anyNA(num)) sort(lv) else lv[order(num)]
  data$cluster <- factor(val, levels = lv)
  data
}

# ------------------------------------------------------------------------
# axis order
# ------------------------------------------------------------------------

#' @rdname uncross
#' @export
sort_axes <- function(data,
                      axes = NULL,
                      key = "x", value = "stratum", id = "alluvium",
                      weight = NULL,
                      engine = NULL) {
  if (is.null(engine)) engine <- ggalluvial_opt("sort_axes")
  spec <- uncross_spec(engine, "sort_axes")
  if (is.null(spec)) {
    stop("No engine given; set `engine` or the option `ggalluvial.sort_axes`.")
  }
  if (! is.data.frame(data)) data <- as.data.frame(data)

  if (is.null(axes)) {
    # lodes form
    for (v in c(key, value, id)) {
      if (! v %in% names(data)) stop("`data` has no variable '", v, "'.")
    }
    lodes <- data.frame(
      x = data[[key]],
      stratum = data[[value]],
      alluvium = data[[id]],
      stringsAsFactors = FALSE
    )
    lodes$y <- if (is.null(weight)) 1 else data[[weight]]
    axis_names <- levels(as.factor(data[[key]]))
    parts <- uncross_wide(lodes)
    if (is.null(parts)) stop("Fewer than two axes to sort.")
    # `uncross_wide()` names the axes 'axis1', ..., in the order of `key`
    axis_names <- axis_names[match(as.character(parts$x), axis_names)]
  } else {
    # alluvia form
    if (is.numeric(axes)) axes <- names(data)[axes]
    if (! all(axes %in% names(data))) {
      stop("Some `axes` are not variables of `data`.")
    }
    axis_names <- axes
    n <- nrow(data)
    lodes <- data.frame(
      x = rep(seq_along(axes), each = n),
      stratum = unlist(lapply(axes, function(a) as.character(data[[a]]))),
      alluvium = rep(seq_len(n), times = length(axes)),
      stringsAsFactors = FALSE
    )
    lodes$y <- if (is.null(weight)) {
      1
    } else {
      rep(data[[weight]], times = length(axes))
    }
    parts <- uncross_wide(lodes)
    if (is.null(parts)) stop("Fewer than two axes to sort.")
  }

  ckey <- list("sort_axes", parts$wide, parts$axes, uncross_spec_key(spec))
  ord <- as.character(uncross_memo(ckey, function() uncross_call(spec, parts)))
  if (! setequal(ord, parts$axes)) {
    stop("`sort_axes` must return a permutation of the axes.")
  }
  axis_names[match(ord, parts$axes)]
}

#' @rdname uncross
#' @return `uncross_reset()` clears the cache of engine results and returns
#'   invisibly.
#' @export
uncross_reset <- function() {
  rm(list = ls(.uncross_cache), envir = .uncross_cache)
  invisible(NULL)
}

# ------------------------------------------------------------------------
# built-in ordering engines
# ------------------------------------------------------------------------

# Each orders the strata within every axis independently, so none of them can
# reduce crossings; they impose a predictable order instead.
order_strata_engine <- function(how) {
  force(how)
  function(wide, axes, weight, method = NULL, ...) {
    if (! is.null(method)) {
      stop("The '", how, "' engine has a single algorithm ",
           "and takes no `method`.")
    }
    if (length(list(...)) > 0L) {
      stop("The '", how, "' engine takes no arguments.")
    }
    stats::setNames(lapply(axes, function(a) {
      lv <- sort(unique(wide[[a]]))
      if (how == "alphabetical") return(lv)
      if (how == "reverse_alphabetical") return(rev(lv))
      size <- vapply(lv, function(v) sum(wide[[weight]][wide[[a]] == v]),
                     numeric(1))
      # ties broken by value, so that the order is fully determined
      lv[order(if (how == "increasing") size else -size, seq_along(lv))]
    }), axes)
  }
}

# ------------------------------------------------------------------------
# the wompwomp engine
# ------------------------------------------------------------------------

require_wompwomp <- function() {
  if (! requireNamespace("wompwomp", quietly = TRUE)) {
    stop("The 'wompwomp' uncrossing engine requires the {wompwomp} package. ",
         "Install it with `install.packages(\"wompwomp\")`.")
  }
}

# Check the arguments an adapter is about to forward to wompwomp: `fixed` names
# the arguments the adapter sets itself, and the rest must be formals of `fun`.
# wompwomp splits its tuning knobs between a function and its `*_options()`
# constructor, so say which is which rather than let R report an unused
# argument.
wompwomp_args <- function(args, what, fixed, fun, opts_fun, opts_label) {
  taken <- intersect(names(args), fixed)
  if (length(taken) > 0) {
    stop("`", what, "` sets ", paste0("`", taken, "`", collapse = ", "),
         " itself; it cannot be given alongside `method`.")
  }
  unknown <- setdiff(names(args), setdiff(names(formals(fun)), "..."))
  if (length(unknown) > 0) {
    msg <- paste0("`", what, "` does not recognize ",
                  paste0("`", unknown, "`", collapse = ", "), ".")
    in_opts <- intersect(unknown, names(formals(opts_fun)))
    if (length(in_opts) > 0) {
      msg <- paste0(msg, " Pass ",
                    paste0("`", in_opts, "`", collapse = ", "),
                    " through `options = ", opts_label, "(...)`.")
    }
    stop(msg)
  }
  args
}

wompwomp_sort_strata <- function(wide, axes, weight, method = NULL, ...) {
  require_wompwomp()
  args <- wompwomp_args(
    list(...), "sort_strata",
    c("data", "cols", "wt", "method", "column_method"),
    wompwomp::sort_to_uncross, wompwomp::sort_to_uncross_options,
    "wompwomp::sort_to_uncross_options"
  )
  out <- do.call(wompwomp::sort_to_uncross, c(
    list(data = wide, cols = axes, wt = weight,
         method = method %||% "neighbornet",
         # the axis order is `sort_axes()`'s business, not this one's
         column_method = "none"),
    args
  ))
  stats::setNames(lapply(axes, function(a) levels(out[[a]])), axes)
}

wompwomp_sort_axes <- function(wide, axes, weight, method = NULL,
                               strata_method = NULL, ...) {
  require_wompwomp()
  args <- wompwomp_args(
    list(...), "sort_axes",
    c("data", "cols", "wt", "method", "column_method"),
    wompwomp::sort_to_uncross, wompwomp::sort_to_uncross_options,
    "wompwomp::sort_to_uncross_options"
  )
  out <- do.call(wompwomp::sort_to_uncross, c(
    list(data = wide, cols = axes, wt = weight,
         # Candidate axis orders are scored by the crossings they induce, so
         # the strata must be sorted while they are compared: scored against
         # strata left in their incoming order, the count is a poor proxy for
         # the crossings the finished plot will have, and picks worse orders.
         # Only the column order is read off; the sorted strata are recomputed
         # by `sort_strata` against whichever order the plot ends up using.
         method = strata_method %||% "neighbornet",
         column_method = method %||% "tsp"),
    args
  ))
  intersect(names(out), axes)
}

wompwomp_color_strata <- function(wide, axes, weight, method = NULL, ...) {
  require_wompwomp()
  args <- wompwomp_args(
    list(...), "color_strata", c("data", "cols", "wt", "method"),
    wompwomp::get_lode_clusters, wompwomp::get_lode_clusters_options,
    "wompwomp::get_lode_clusters_options"
  )
  map <- do.call(wompwomp::get_lode_clusters, c(
    list(data = wide, cols = axes, wt = weight,
         method = method %||% "advanced"),
    args
  ))
  stats::setNames(lapply(axes, function(a) unlist(map[[a]])), axes)
}

.onLoad <- function(libname, pkgname) {
  register_uncross_engine(
    "wompwomp",
    sort_strata = wompwomp_sort_strata,
    sort_axes = wompwomp_sort_axes,
    color_strata = wompwomp_color_strata
  )
  for (how in c("alphabetical", "reverse_alphabetical",
                "increasing", "decreasing")) {
    register_uncross_engine(how, sort_strata = order_strata_engine(how))
  }
}
