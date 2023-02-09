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

# distilling functions
most <- function(x) {
  x[which(factor(x) == names(which.max(table(factor(x)))))[1]]
}
