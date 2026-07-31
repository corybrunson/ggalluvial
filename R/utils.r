# color and differentiation aesthetics
.color_diff_aesthetics <- c(
  "fill", "bg",
  "alpha",
  "fg", "col", "colour", "color",
  "lty", "linetype",
  "cex", "lwd", "linewidth", "size",
  "pch", "shape"
)

# text aesthetics
.text_aesthetics <- c(
  "label",
  "vjust", "hjust", "angle",
  "family", "fontface", "lineheight"
)

# all differentiation aesthetics (color + text)
.diff_aesthetics <- c(.color_diff_aesthetics, .text_aesthetics)

# Detect differentiation aesthetics in data, including ggnewscale-renamed variants
# ggnewscale renames aesthetics like fill -> fill_ggnewscale_1
detect_diff_aes <- function(data) {
  pattern <- paste0("^(", paste(.diff_aesthetics, collapse = "|"), ")(_|$)")
  names(data)[grepl(pattern, names(data))]
}

# distilling functions
most <- function(x) {
  x[which(factor(x) == names(which.max(table(factor(x)))))[1]]
}
