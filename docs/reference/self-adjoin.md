# Adjoin a dataset to itself

This function binds a dataset to itself along adjacent pairs of a `key`
variable. It is invoked by [`geom_flow()`](geom_flow.md) to convert data
in lodes form to something similar to alluvia form.

## Usage

``` r
self_adjoin(
  data,
  key,
  by = NULL,
  link = NULL,
  keep.x = NULL,
  keep.y = NULL,
  suffix = c(".x", ".y")
)
```

## Arguments

- data:

  A data frame in lodes form (repeated measures data; see
  [`alluvial-data`](alluvial-data.md)).

- key:

  Column of `data` indicating sequential collection; handled as in
  [`tidyr::spread()`](https://tidyr.tidyverse.org/reference/spread.html).

- by:

  Character vector of variables to self-adjoin by; passed to
  [`dplyr::mutate-joins`](https://dplyr.tidyverse.org/reference/mutate-joins.html)
  functions.

- link:

  Character vector of variables to adjoin. Will be replaced by pairs of
  variables suffixed by `suffix`.

- keep.x, keep.y:

  Character vector of variables to associate with the first
  (respectively, second) copy of `data` after adjoining. These variables
  can overlap with each other but cannot overlap with `by` or `link`.

- suffix:

  Suffixes to add to the adjoined `link` variables; passed to
  [`dplyr::mutate-joins`](https://dplyr.tidyverse.org/reference/mutate-joins.html)
  functions.

## Details

`self_adjoin` invokes
[`dplyr::mutate-joins`](https://dplyr.tidyverse.org/reference/mutate-joins.html)
functions in order to convert a dataset with measures along a discrete
`key` variable into a dataset consisting of column bindings of these
measures (by any `by` variables) along adjacent values of `key`.

## See also

Other alluvial data manipulation: [`alluvial-data`](alluvial-data.md)

## Examples

``` r
# self-adjoin `majors` data
data(majors)
major_changes <- self_adjoin(majors, key = semester,
                             by = "student", link = c("semester", "curriculum"))
major_changes$change <- major_changes$curriculum.x == major_changes$curriculum.y
head(major_changes)
#>    step student semester.x curriculum.x semester.y curriculum.y change
#> 1 CURR1       1      CURR1     Painting      CURR3     Painting   TRUE
#> 2 CURR1       2      CURR1     Painting      CURR3     Painting   TRUE
#> 3 CURR1       6      CURR1     Sculpure      CURR3     Sculpure   TRUE
#> 4 CURR1       8      CURR1     Painting      CURR3     Painting   TRUE
#> 5 CURR1       9      CURR1     Sculpure      CURR3  Art History  FALSE
#> 6 CURR1      10      CURR1     Painting      CURR3     Painting   TRUE

# self-adjoin `vaccinations` data
data(vaccinations)
vaccination_steps <- self_adjoin(vaccinations, key = survey, by = "subject",
                                 link = c("survey", "response"),
                                 keep.x = c("freq"))
head(vaccination_steps)
#>        step subject  survey.x response.x freq  survey.y response.y
#> 1 ms153_NSA       1 ms153_NSA     Always   48 ms432_NSA     Always
#> 2 ms153_NSA       2 ms153_NSA     Always    9 ms432_NSA     Always
#> 3 ms153_NSA       3 ms153_NSA     Always   66 ms432_NSA    Missing
#> 4 ms153_NSA       4 ms153_NSA     Always    1 ms432_NSA    Missing
#> 5 ms153_NSA       5 ms153_NSA     Always   11 ms432_NSA    Missing
#> 6 ms153_NSA       6 ms153_NSA     Always    1 ms432_NSA      Never
vaccination_steps <- self_adjoin(vaccinations, key = survey, by = "subject",
                                 link = c("survey", "response"),
                                 keep.x = c("freq"),
                                 keep.y = c("start_date", "end_date"))
head(vaccination_steps)
#>        step subject  survey.x response.x freq  survey.y response.y start_date
#> 1 ms153_NSA       1 ms153_NSA     Always   48 ms432_NSA     Always 2015-06-04
#> 2 ms153_NSA       2 ms153_NSA     Always    9 ms432_NSA     Always 2015-06-04
#> 3 ms153_NSA       3 ms153_NSA     Always   66 ms432_NSA    Missing 2015-06-04
#> 4 ms153_NSA       4 ms153_NSA     Always    1 ms432_NSA    Missing 2015-06-04
#> 5 ms153_NSA       5 ms153_NSA     Always   11 ms432_NSA    Missing 2015-06-04
#> 6 ms153_NSA       6 ms153_NSA     Always    1 ms432_NSA      Never 2015-06-04
#>     end_date
#> 1 2015-10-05
#> 2 2015-10-05
#> 3 2015-10-05
#> 4 2015-10-05
#> 5 2015-10-05
#> 6 2015-10-05
```
