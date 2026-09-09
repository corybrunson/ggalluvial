# A deterministic stand-in for a real uncrossing engine, so that the plumbing
# can be tested without {wompwomp} installed. It records its calls, so that the
# tests can also check that results are shared between layers.
stub_log <- new.env(parent = emptyenv())
stub_log$n <- 0L

stub_reset <- function() {
  stub_log$n <- 0L
  uncross_reset()
}

# strata in reverse alphabetical order at every axis, optionally rotated (to
# give the tuning-argument tests an observably different order) or thinned (to
# exercise strata that the engine declines to rank)
stub_sort_strata <- function(wide, axes, weight, method = NULL,
                             rotate = 0L, drop = NULL) {
  stub_log$n <- stub_log$n + 1L
  stats::setNames(lapply(axes, function(a) {
    lv <- sort(unique(wide[[a]]), decreasing = TRUE)
    if (! is.null(method) && method == "identity") lv <- rev(lv)
    if (rotate > 0L) lv <- c(utils::tail(lv, rotate), utils::head(lv, -rotate))
    if (! is.null(drop)) lv <- setdiff(lv, drop)
    lv
  }), axes)
}

# axes in reverse order
stub_sort_axes <- function(wide, axes, weight, method = NULL) {
  stub_log$n <- stub_log$n + 1L
  rev(axes)
}

# one cluster per distinct stratum value, shared across axes
stub_color_strata <- function(wide, axes, weight, method = NULL) {
  stub_log$n <- stub_log$n + 1L
  vals <- sort(unique(unlist(lapply(axes, function(a) wide[[a]]))))
  stats::setNames(lapply(axes, function(a) {
    lv <- sort(unique(wide[[a]]))
    stats::setNames(match(lv, vals), lv)
  }), axes)
}

register_uncross_engine("stub",
                        sort_strata = stub_sort_strata,
                        sort_axes = stub_sort_axes,
                        color_strata = stub_color_strata)
register_uncross_engine("halfstub", sort_strata = stub_sort_strata)

# a small three-axis data set in lodes form
toy <- data.frame(
  x = rep(1:3, each = 4),
  stratum = rep(c("a", "b", "c", "d"), times = 3),
  alluvium = rep(1:4, times = 3),
  y = rep(c(4, 3, 2, 1), times = 3),
  stringsAsFactors = FALSE
)

# strata at each axis, from bottom to top, of a built layer
built_strata <- function(p, i = 1L) {
  d <- ggplot_build(p)$data[[i]]
  d <- d[order(d$x, d$ymin), , drop = FALSE]
  lapply(split(as.character(d$stratum), d$x), unique)
}

# -- the specification grammar -----------------------------------------------

test_that("`NULL` and `NA` switch the operations off", {
  expect_null(uncross_spec(NULL, "sort_strata"))
  expect_null(uncross_spec(NA, "sort_strata"))
  expect_null(uncross_spec(list(), "sort_strata"))
  expect_null(uncross_ranks(toy, NULL))
  expect_null(uncross_clusters(toy, NULL))
})

test_that("a bare string names an engine, not an algorithm", {
  spec <- uncross_spec("stub", "sort_strata")
  expect_identical(spec$fun, stub_sort_strata)
  expect_null(spec$method)
  expect_identical(spec$args, list())
  # a method name is not an engine name
  expect_error(uncross_spec("neighbornet", "sort_strata"),
               "No uncrossing engine named 'neighbornet'")
  # and the error names the engines that do exist
  expect_error(uncross_spec("nosuch", "sort_strata"), "stub")
})

test_that("a list supplies the algorithm and its tuning arguments", {
  spec <- uncross_spec(list("stub", method = "identity", rotate = 1L),
                       "sort_strata")
  expect_identical(spec$fun, stub_sort_strata)
  expect_identical(spec$method, "identity")
  expect_identical(spec$args, list(rotate = 1L))
  # `method` alone, and arguments alone, both work
  expect_identical(uncross_spec(list("stub", method = "x"),
                                "sort_strata")$method, "x")
  expect_identical(uncross_spec(list("stub", rotate = 1L),
                                "sort_strata")$args, list(rotate = 1L))
})

test_that("functions are used as engines directly", {
  spec <- uncross_spec(stub_sort_strata, "sort_strata")
  expect_identical(spec$fun, stub_sort_strata)
  expect_null(spec$method)
  spec <- uncross_spec(list(stub_sort_strata, method = "identity"),
                       "sort_strata")
  expect_identical(spec$fun, stub_sort_strata)
  expect_identical(spec$method, "identity")
})

test_that("invalid specifications are rejected", {
  expect_error(uncross_spec(list(1, 2), "sort_strata"), "exactly one unnamed")
  expect_error(uncross_spec(list(rotate = 1L), "sort_strata"),
               "exactly one unnamed")
  expect_error(uncross_spec(c("a", "b"), "sort_strata"), "must name an engine")
  expect_error(uncross_spec(1L, "sort_strata"), "must name an engine")
  expect_error(uncross_spec(list("stub", method = c("a", "b")), "sort_strata"),
               "`method` must be a single string")
  expect_error(uncross_spec("halfstub", "color_strata"), "does not implement")
})

test_that("arguments may not shadow the engine contract", {
  for (nm in c("wide", "axes", "weight")) {
    spec <- stats::setNames(list("stub", 1), c("", nm))
    expect_error(uncross_spec(spec, "sort_strata"),
                 "cannot take .* as an argument")
  }
})

test_that("engines can be registered and listed", {
  expect_true(all(c("stub", "halfstub", "wompwomp", "alphabetical",
                    "reverse_alphabetical", "increasing", "decreasing") %in%
                    uncross_engines()))
  expect_error(register_uncross_engine("bad", sort_strata = "nope"),
               "must be functions")
})

# -- built-in ordering engines -----------------------------------------------

test_that("the built-in engines order the strata as advertised", {
  gg <- ggplot(toy, aes(x = x, stratum = stratum, alluvium = alluvium, y = y))
  order_by <- function(engine) {
    uncross_reset()
    built_strata(gg + geom_stratum(sort_strata = engine))[["1"]]
  }
  # `reverse = TRUE` (the default) mirrors the engine's ascending order
  expect_identical(order_by("alphabetical"), c("d", "c", "b", "a"))
  expect_identical(order_by("reverse_alphabetical"), c("a", "b", "c", "d"))
  # sizes are a = 4, b = 3, c = 2, d = 1
  expect_identical(order_by("increasing"), c("a", "b", "c", "d"))
  expect_identical(order_by("decreasing"), c("d", "c", "b", "a"))
})

test_that("the built-in engines take no algorithm and no arguments", {
  gg <- ggplot(toy, aes(x = x, stratum = stratum, alluvium = alluvium, y = y))
  uncross_reset()
  expect_warning(ggplot_build(gg + geom_stratum(
    sort_strata = list("increasing", method = "x")
  )), "takes no `method`")
  uncross_reset()
  expect_warning(ggplot_build(gg + geom_stratum(
    sort_strata = list("increasing", nosuch = 1)
  )), "takes no arguments")
  # they order strata, and nothing else
  expect_error(uncross_spec("increasing", "color_strata"),
               "does not implement")
  expect_error(uncross_spec("alphabetical", "sort_axes"),
               "does not implement")
})

# -- reshaping ---------------------------------------------------------------

test_that("`uncross_wide()` reshapes lodes data to alluvia form", {
  parts <- uncross_wide(toy)
  expect_identical(parts$axes, c("axis1", "axis2", "axis3"))
  expect_identical(nrow(parts$wide), 4L)
  expect_identical(parts$wide$axis1, c("a", "b", "c", "d"))
  # the weight of an alluvium is the mean of its lode heights
  expect_equal(parts$wide[[parts$weight]], c(4, 3, 2, 1))
  expect_identical(parts$x, 1:3)
})

test_that("`uncross_wide()` gives up when there is nothing to uncross", {
  expect_null(uncross_wide(toy[toy$x == 1, , drop = FALSE]))
  expect_null(uncross_wide(toy[, c("x", "y")]))
})

test_that("`uncross_wide()` marks axes at which an alluvium is absent", {
  gappy <- toy[! (toy$x == 2 & toy$stratum == "b"), , drop = FALSE]
  parts <- uncross_wide(gappy)
  expect_identical(parts$wide$axis2, c("a", parts$gap, "c", "d"))
  # the sentinel is never assigned a rank
  ranks <- uncross_ranks(gappy, "stub")
  expect_false(any(grepl(parts$gap, names(ranks), fixed = TRUE)))
  expect_length(ranks, 11L)
})

# -- deposits ----------------------------------------------------------------

test_that("`deposit_data()` respects supplied ranks", {
  data <- transform(toy, yneg = FALSE)
  ranks <- stats::setNames(c(4L, 3L, 2L, 1L),
                           paste0(1, "\r", c("a", "b", "c", "d")))
  dep <- deposit_data(data[data$x == 1, ], NA, FALSE, TRUE, ranks)
  expect_identical(dep$deposit[order(dep$stratum)], c(4L, 3L, 2L, 1L))
  # without ranks, the strata keep their intrinsic order
  dep0 <- deposit_data(data[data$x == 1, ], NA, FALSE, TRUE, NULL)
  expect_identical(dep0$deposit[order(dep0$stratum)], 1:4)
})

test_that("strata missing from the ranks are deposited last", {
  data <- transform(toy[toy$x == 1, ], yneg = FALSE)
  ranks <- stats::setNames(c(2L, 1L), paste0(1, "\r", c("c", "d")))
  dep <- deposit_data(data, NA, FALSE, TRUE, ranks)
  dep <- dep[order(dep$deposit), ]
  expect_identical(as.character(dep$stratum), c("d", "c", "a", "b"))
})

test_that("`decreasing` still takes precedence over supplied ranks", {
  data <- transform(toy[toy$x == 1, ], yneg = FALSE)
  ranks <- stats::setNames(c(4L, 3L, 2L, 1L),
                           paste0(1, "\r", c("a", "b", "c", "d")))
  # the ranks alone would deposit the strata in the order d, c, b, a
  plain <- deposit_data(data, NA, FALSE, TRUE, ranks)
  expect_identical(as.character(plain$stratum[order(plain$deposit)]),
                   c("d", "c", "b", "a"))
  # `decreasing` overrules them and deposits the largest stratum first
  dep <- deposit_data(data, TRUE, FALSE, TRUE, ranks)
  dep <- dep[order(dep$deposit), ]
  expect_identical(as.character(dep$stratum), c("a", "b", "c", "d"))
})

# -- the `stat_*()` parameters -----------------------------------------------

test_that("`sort_strata` reorders the strata in every `stat_*()`", {
  gg <- ggplot(toy, aes(x = x, stratum = stratum, alluvium = alluvium, y = y))
  for (layer in list(geom_stratum(sort_strata = "stub"),
                     geom_flow(sort_strata = "stub"),
                     geom_alluvium(sort_strata = "stub"))) {
    stub_reset()
    expect_identical(built_strata(gg + layer)[["1"]], c("a", "b", "c", "d"))
  }
  # and without sorting the strata keep their intrinsic (reversed) order
  stub_reset()
  expect_identical(built_strata(gg + geom_stratum())[["1"]],
                   c("d", "c", "b", "a"))
})

test_that("`sort_strata` results are shared between layers", {
  stub_reset()
  p <- ggplot(toy, aes(x = x, stratum = stratum, alluvium = alluvium, y = y)) +
    geom_flow(sort_strata = "stub") +
    geom_stratum(sort_strata = "stub")
  b <- ggplot_build(p)
  expect_identical(stub_log$n, 1L)
  expect_identical(built_strata(p, 1L), built_strata(p, 2L))
})

test_that("the algorithm and the arguments both reach the engine", {
  stub_reset()
  gg <- ggplot(toy, aes(x = x, stratum = stratum, alluvium = alluvium, y = y))
  plain <- built_strata(gg + geom_stratum(sort_strata = "stub"))
  spun <- built_strata(gg + geom_stratum(sort_strata = list("stub",
                                                           rotate = 1L)))
  flipped <- built_strata(gg + geom_stratum(sort_strata =
                                              list("stub",
                                                   method = "identity")))
  # three distinct specifications, three engine calls, three orders
  expect_identical(stub_log$n, 3L)
  expect_identical(plain[["1"]], c("a", "b", "c", "d"))
  expect_identical(spun[["1"]], c("b", "c", "d", "a"))
  expect_identical(flipped[["1"]], c("d", "c", "b", "a"))
})

test_that("strata the engine leaves unranked are deposited last", {
  stub_reset()
  gg <- ggplot(toy, aes(x = x, stratum = stratum, alluvium = alluvium, y = y))
  ord <- built_strata(gg + geom_stratum(sort_strata = list("stub",
                                                          drop = "d")))
  expect_identical(ord[["1"]], c("d", "a", "b", "c"))
})

test_that("`color_strata` computes the `cluster` variable", {
  stub_reset()
  gg <- ggplot(toy, aes(x = x, stratum = stratum, alluvium = alluvium, y = y))
  d <- ggplot_build(gg + geom_stratum(color_strata = "stub"))$data[[1]]
  expect_true("cluster" %in% names(d))
  expect_s3_class(d$cluster, "factor")
  expect_identical(nlevels(d$cluster), 4L)
  # each stratum value takes the same cluster wherever it appears
  expect_identical(nrow(unique(d[, c("stratum", "cluster")])), 4L)
  # and there is no such variable when the parameter is unset
  d0 <- ggplot_build(gg + geom_stratum())$data[[1]]
  expect_false("cluster" %in% names(d0))
})

test_that("`cluster` survives the aggregations in each `stat_*()`", {
  gg <- ggplot(toy, aes(x = x, stratum = stratum, alluvium = alluvium, y = y))
  for (layer in list(geom_stratum(color_strata = "stub"),
                     geom_flow(color_strata = "stub"),
                     geom_lode(stat = "alluvium", color_strata = "stub"))) {
    stub_reset()
    d <- ggplot_build(gg + layer)$data[[1]]
    expect_true("cluster" %in% names(d))
    expect_false(anyNA(d$cluster))
  }
})

test_that("`cluster` is available to `after_stat()`", {
  stub_reset()
  p <- ggplot(toy, aes(x = x, stratum = stratum, alluvium = alluvium, y = y)) +
    geom_stratum(aes(fill = after_stat(cluster)), color_strata = "stub")
  d <- ggplot_build(p)$data[[1]]
  expect_identical(length(unique(d$fill)), 4L)
})

test_that("either spelling of `color_strata` is accepted", {
  stub_reset()
  gg <- ggplot(toy, aes(x = x, stratum = stratum, alluvium = alluvium, y = y))
  us <- ggplot_build(gg + geom_stratum(color_strata = "stub"))$data[[1]]
  uk <- ggplot_build(gg + geom_stratum(colour_strata = "stub"))$data[[1]]
  st <- ggplot_build(gg + stat_stratum(colour_strata = "stub"))$data[[1]]
  expect_identical(us$cluster, uk$cluster)
  expect_identical(us$cluster, st$cluster)
})

test_that("`NA` opts a layer out of the package option", {
  gg <- ggplot(toy, aes(x = x, stratum = stratum, alluvium = alluvium, y = y))
  stub_reset()
  withr::local_options(list(ggalluvial.sort_strata = "stub",
                            ggalluvial.color_strata = "stub"))
  expect_identical(built_strata(gg + geom_stratum(sort_strata = NA))[["1"]],
                   c("d", "c", "b", "a"))
  d <- ggplot_build(gg + geom_stratum(color_strata = NA))$data[[1]]
  expect_false("cluster" %in% names(d))
})

test_that("uncrossing is refused without the `alluvium` aesthetic", {
  # `stat_stratum()` invents an alluvium from row order when none is given;
  # uncrossing those links would make the plot depend on the order of the data
  bare <- toy[, c("x", "stratum", "y")]
  gg <- ggplot(bare, aes(x = x, stratum = stratum, y = y))
  stub_reset()
  expect_warning(ord <- built_strata(gg + geom_stratum(sort_strata = "stub")),
                 "require the `alluvium` aesthetic")
  expect_identical(ord[["1"]], c("d", "c", "b", "a"))
  expect_identical(stub_log$n, 0L)

  stub_reset()
  expect_warning(b <- ggplot_build(gg + geom_stratum(color_strata = "stub")),
                 "require the `alluvium` aesthetic")
  expect_false("cluster" %in% names(b$data[[1]]))

  # the package option is refused the same way, and shuffling the rows of the
  # data cannot change the result
  stub_reset()
  withr::local_options(list(ggalluvial.sort_strata = "stub"))
  expect_warning(a <- built_strata(gg + geom_stratum()),
                 "require the `alluvium` aesthetic")
  shuffled <- bare[c(5:12, 1:4), , drop = FALSE]
  expect_warning(
    b <- built_strata(ggplot(shuffled, aes(x = x, stratum = stratum, y = y)) +
                        geom_stratum()),
    "require the `alluvium` aesthetic"
  )
  expect_identical(a, b)
  expect_identical(stub_log$n, 0L)
})

test_that("uncrossing is skipped when there is only one axis", {
  one <- toy[toy$x == 1, , drop = FALSE]
  stub_reset()
  p <- ggplot(one, aes(x = x, stratum = stratum, alluvium = alluvium, y = y)) +
    geom_stratum(sort_strata = "stub", color_strata = "stub")
  expect_silent(b <- ggplot_build(p))
  expect_identical(stub_log$n, 0L)
  expect_false("cluster" %in% names(b$data[[1]]))
})

test_that("engines that violate the contract are reported", {
  register_uncross_engine("wrong",
                          sort_strata = function(...) list(nope = 1),
                          color_strata = function(...) "nope")
  gg <- ggplot(toy, aes(x = x, stratum = stratum, alluvium = alluvium, y = y))
  # ggplot2 downgrades an error raised inside a stat to a warning
  stub_reset()
  expect_warning(ggplot_build(gg + geom_stratum(sort_strata = "wrong")),
                 "list of stratum orders")
  stub_reset()
  expect_warning(ggplot_build(gg + geom_stratum(color_strata = "wrong")),
                 "list of stratum clusters")
})

# -- package options ---------------------------------------------------------

test_that("`ggalluvial.sort_strata` sets the default", {
  gg <- ggplot(toy, aes(x = x, stratum = stratum, alluvium = alluvium, y = y))
  stub_reset()
  withr::local_options(list(ggalluvial.sort_strata = "stub"))
  expect_identical(built_strata(gg + geom_stratum())[["1"]],
                   c("a", "b", "c", "d"))
})

test_that("`ggalluvial.color_strata` sets the default", {
  gg <- ggplot(toy, aes(x = x, stratum = stratum, alluvium = alluvium, y = y))
  stub_reset()
  withr::local_options(list(ggalluvial.color_strata = "stub"))
  d <- ggplot_build(gg + geom_stratum())$data[[1]]
  expect_true("cluster" %in% names(d))
})

test_that("the options are defaults, not overrides", {
  gg <- ggplot(toy, aes(x = x, stratum = stratum, alluvium = alluvium, y = y))
  stub_reset()
  withr::local_options(list(ggalluvial.sort_strata = "stub"))
  ord <- built_strata(gg + geom_stratum(sort_strata = list("stub",
                                                          rotate = 1L)))
  expect_identical(ord[["1"]], c("b", "c", "d", "a"))
})

# -- `sort_axes()` -----------------------------------------------------------

test_that("`sort_axes()` orders the axes of lodes-form data", {
  stub_reset()
  d <- data.frame(
    axis = factor(rep(c("first", "second", "third"), each = 4),
                  levels = c("first", "second", "third")),
    stratum = rep(c("a", "b", "c", "d"), times = 3),
    id = rep(1:4, times = 3),
    n = rep(c(4, 3, 2, 1), times = 3)
  )
  expect_identical(
    sort_axes(d, key = "axis", value = "stratum", id = "id", weight = "n",
              engine = "stub"),
    c("third", "second", "first")
  )
})

test_that("`sort_axes()` orders the axes of alluvia-form data", {
  stub_reset()
  w <- data.frame(
    first = c("a", "b", "c", "d"),
    second = c("b", "a", "d", "c"),
    third = c("c", "d", "a", "b"),
    n = c(4, 3, 2, 1),
    stringsAsFactors = FALSE
  )
  expect_identical(
    sort_axes(w, axes = c("first", "second", "third"), weight = "n",
              engine = "stub"),
    c("third", "second", "first")
  )
  # axes may also be given by position
  expect_identical(sort_axes(w, axes = 1:3, weight = "n", engine = "stub"),
                   c("third", "second", "first"))
})

test_that("`sort_axes()` validates its inputs and its engine", {
  w <- data.frame(first = c("a", "b"), second = c("b", "a"),
                  stringsAsFactors = FALSE)
  expect_error(sort_axes(w, axes = c("first", "second")), "No engine given")
  expect_error(sort_axes(w, axes = c("first", "nope"), engine = "stub"),
               "not variables")
  expect_error(sort_axes(w, key = "nope", engine = "stub"), "no variable")
  register_uncross_engine("notaperm", sort_axes = function(...) c("axis1"))
  expect_error(sort_axes(w, axes = c("first", "second"), engine = "notaperm"),
               "permutation")
})

test_that("`ggalluvial.sort_axes` sets the default engine", {
  stub_reset()
  w <- data.frame(first = c("a", "b"), second = c("b", "a"),
                  stringsAsFactors = FALSE)
  withr::local_options(list(ggalluvial.sort_axes = "stub"))
  expect_identical(sort_axes(w, axes = c("first", "second")),
                   c("second", "first"))
})

# -- the wompwomp engine -----------------------------------------------------

test_that("wompwomp reduces the number of crossings", {
  skip_if_not_installed("wompwomp")
  uncross_reset()
  data(vaccinations, envir = environment())
  gg <- ggplot(vaccinations,
               aes(x = survey, stratum = response, alluvium = subject,
                   y = freq))

  # the objective of the stratum order that a plot actually deposits
  objective <- function(p) {
    ord <- built_strata(p)
    w <- to_alluvia_form(vaccinations, key = survey, value = response,
                         id = subject, distill = "first")
    axes <- levels(vaccinations$survey)
    for (i in seq_along(axes)) {
      w[[axes[i]]] <- factor(as.character(w[[axes[i]]]),
                             levels = ord[[as.character(i)]])
    }
    wompwomp::compute_crossing_objective(
      as.data.frame(w[c(axes, "freq")]), cols = axes, wt = "freq"
    )$output_objective
  }

  before <- objective(gg + geom_stratum())
  # wompwomp's default algorithm, "neighbornet", is deterministic
  after <- objective(gg + geom_stratum(sort_strata = "wompwomp"))
  expect_lt(after, before)
  # the per-axis engines cannot reduce crossings, but must still be valid
  expect_gte(objective(gg + geom_stratum(sort_strata = "increasing")), after)
})

test_that("a stochastic algorithm orders the strata alike in every layer", {
  skip_if_not_installed("wompwomp")
  uncross_reset()
  data(vaccinations, envir = environment())
  tsp <- list("wompwomp", method = "tsp")
  p <- ggplot(vaccinations,
              aes(x = survey, stratum = response, alluvium = subject,
                  y = freq)) +
    geom_flow(sort_strata = tsp) +
    geom_stratum(sort_strata = tsp) +
    geom_alluvium(sort_strata = tsp)
  expect_identical(built_strata(p, 1L), built_strata(p, 2L))
  expect_identical(built_strata(p, 2L), built_strata(p, 3L))
  # every stratum is still deposited exactly once at each axis
  ord <- built_strata(p, 2L)
  for (o in ord) expect_setequal(o, levels(vaccinations$response))
  # and the order is not the default one
  plain <- built_strata(ggplot(vaccinations,
                               aes(x = survey, stratum = response,
                                   alluvium = subject, y = freq)) +
                          geom_stratum())
  expect_false(identical(ord, plain))
})

test_that("wompwomp assigns strata to shared clusters", {
  skip_if_not_installed("wompwomp")
  uncross_reset()
  data(vaccinations, envir = environment())
  p <- ggplot(vaccinations,
              aes(x = survey, stratum = response, alluvium = subject,
                  y = freq)) +
    geom_stratum(color_strata = "wompwomp")
  d <- ggplot_build(p)$data[[1]]
  expect_s3_class(d$cluster, "factor")
  expect_false(anyNA(d$cluster))
  # fewer clusters than strata means that some strata were matched across axes
  expect_lt(nlevels(d$cluster), nrow(d))
})

test_that("wompwomp orders the axes", {
  skip_if_not_installed("wompwomp")
  uncross_reset()
  data(vaccinations, envir = environment())
  ord <- sort_axes(vaccinations, key = "survey", value = "response",
                   id = "subject", weight = "freq", engine = "wompwomp")
  expect_setequal(ord, levels(vaccinations$survey))
  # the algorithm may be named too
  uncross_reset()
  ord2 <- sort_axes(vaccinations, key = "survey", value = "response",
                    id = "subject", weight = "freq",
                    engine = list("wompwomp", method = "neighbornet"))
  expect_setequal(ord2, levels(vaccinations$survey))
})

test_that("wompwomp sorts the strata while it searches for an axis order", {
  skip_if_not_installed("wompwomp")
  # Candidate axis orders are scored by the crossings they induce, so the
  # strata must be sorted while they are compared. Scored against the incoming
  # stratum order, the search reliably picks a worse axis order.
  set.seed(11)
  n <- 400
  d <- data.frame(id = 1:n, A = sample(paste0("a", 1:4), n, TRUE),
                  stringsAsFactors = FALSE)
  d$B <- ifelse(runif(n) < .85, sub("a", "b", d$A),
                sample(paste0("b", 1:4), n, TRUE))
  d$C <- sample(paste0("c", 1:4), n, TRUE)
  d$D <- ifelse(runif(n) < .85, sub("c", "d", d$C),
                sample(paste0("d", 1:4), n, TRUE))
  d$freq <- 1
  axes <- c("A", "B", "C", "D")
  lodes <- do.call(rbind, lapply(axes, function(a) {
    data.frame(x = a, stratum = d[[a]], id = d$id, freq = 1,
               stringsAsFactors = FALSE)
  }))
  lodes$x <- factor(lodes$x, levels = axes)

  # the crossings of an axis order, once its strata are sorted for it
  scored <- function(an) {
    o <- wompwomp::sort_to_uncross(d[c(an, "freq")], cols = an, wt = "freq",
                                   method = "neighbornet",
                                   column_method = "none")
    ww <- d
    for (a in an) ww[[a]] <- factor(ww[[a]], levels = levels(o[[a]]))
    wompwomp::compute_crossing_objective(ww[c(an, "freq")], cols = an,
                                         wt = "freq")$output_objective
  }
  best <- scored(axes)
  for (s in 1:3) {
    set.seed(s)
    uncross_reset()
    an <- sort_axes(lodes, key = "x", value = "stratum", id = "id",
                    weight = "freq", engine = "wompwomp")
    expect_setequal(an, axes)
    expect_equal(scored(an), best)
  }
  # `strata_method` reaches the inner sort
  set.seed(0)
  uncross_reset()
  expect_setequal(
    sort_axes(lodes, key = "x", value = "stratum", id = "id", weight = "freq",
              engine = list("wompwomp", method = "tsp",
                            strata_method = "tsp")),
    axes
  )
})

test_that("wompwomp takes tuning arguments alongside the algorithm", {
  skip_if_not_installed("wompwomp")
  data(vaccinations, envir = environment())
  gg <- ggplot(vaccinations,
               aes(x = survey, stratum = response, alluvium = subject,
                   y = freq))
  ord <- function(spec) {
    uncross_reset()
    built_strata(gg + geom_stratum(sort_strata = spec))
  }
  # `alpha` and `beta` reach `sort_to_uncross()` and change the result
  expect_false(identical(
    ord("wompwomp"),
    ord(list("wompwomp", method = "neighbornet", alpha = 3, beta = 1))
  ))
  # as does an `options` list
  uncross_reset()
  expect_silent(ggplot_build(
    gg + geom_stratum(sort_strata = list(
      "wompwomp",
      options = wompwomp::sort_to_uncross_options(weighted_metric = FALSE)
    ))
  ))
  # and `resolution` reaches `get_lode_clusters()`
  uncross_reset()
  coarse <- ggplot_build(gg + geom_stratum(color_strata = "wompwomp"))$data[[1]]
  uncross_reset()
  fine <- ggplot_build(gg + geom_stratum(
    color_strata = list("wompwomp", resolution = 3)
  ))$data[[1]]
  expect_gt(nlevels(fine$cluster), nlevels(coarse$cluster))
})

test_that("wompwomp reports arguments it cannot forward", {
  skip_if_not_installed("wompwomp")
  gg <- ggplot(toy, aes(x = x, stratum = stratum, alluvium = alluvium, y = y))
  build <- function(spec) {
    uncross_reset()
    ggplot_build(gg + geom_stratum(sort_strata = spec))
  }
  # an argument the adapter sets itself
  expect_warning(build(list("wompwomp", column_method = "tsp")),
                 "sets `column_method` itself")
  # a knob that lives in `sort_to_uncross_options()` rather than in the
  # function, which is the easy mistake to make
  expect_warning(build(list("wompwomp", weighted_metric = FALSE)),
                 "through `options = wompwomp::sort_to_uncross_options")
  # and one that is simply not an argument at all
  expect_warning(build(list("wompwomp", nosuch = 1)),
                 "does not recognize `nosuch`")
  uncross_reset()
  expect_warning(
    ggplot_build(gg + geom_stratum(color_strata = list("wompwomp",
                                                      cutoff = 0.3))),
    "through `options = wompwomp::get_lode_clusters_options"
  )
})

test_that("wompwomp reports an invalid algorithm", {
  skip_if_not_installed("wompwomp")
  uncross_reset()
  expect_warning(
    ggplot_build(
      ggplot(toy, aes(x = x, stratum = stratum, alluvium = alluvium, y = y)) +
        geom_stratum(sort_strata = list("wompwomp", method = "nosuchmethod"))
    ),
    "should be one of"
  )
})
