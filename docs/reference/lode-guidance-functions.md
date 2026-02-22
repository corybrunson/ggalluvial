# Lode guidance functions

These functions control the order of lodes within strata in an alluvial
diagram. They are invoked by [`stat_alluvium()`](stat_alluvium.md) and
can be passed to the `lode.guidance` parameter.

## Usage

``` r
lode_zigzag(n, i)

lode_zagzig(n, i)

lode_forward(n, i)

lode_rightward(n, i)

lode_backward(n, i)

lode_leftward(n, i)

lode_frontback(n, i)

lode_rightleft(n, i)

lode_backfront(n, i)

lode_leftright(n, i)
```

## Arguments

- n:

  Numeric, a positive integer

- i:

  Numeric, a positive integer at most `n`

## Details

Each function orders the numbers 1 through `n`, starting at index `i`.
The choice of function made in [`stat_alluvium()`](stat_alluvium.md)
determines the order in which the other axes contribute to the sorting
of lodes within each index axis. After starting at `i`, the functions
order the remaining axes as follows:

- `zigzag`: Zigzag outward from `i`, starting in the outward direction

- `zigzag`: Zigzag outward from `i`, starting in the inward direction

- `forward`: Increasing order (alias `rightward`)

- `backward`: Decreasing order (alias `leftward`)

- `frontback`: Proceed forward from `i` to `n`, then backward to 1
  (alias `rightleft`)

- `backfront`: Proceed backward from `i` to 1, then forward to `n`
  (alias `leftright`)

An extended discussion of how strata and lodes are arranged in alluvial
plots, including the effects of different lode guidance functions, can
be found in the vignette "The Order of the Rectangles" via
[`vignette("order-rectangles", package = "ggalluvial")`](../articles/order-rectangles.md).
