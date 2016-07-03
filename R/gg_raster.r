#' Plot a raster using gg_plot
#'
#' Creates a facetted plot of a raster or set of rasters using ggplot.
#'
#' @param x A Raster* object.
#' @param labels Character vector. Facet labels.
#' @param nrow Integer. Number of rows for facets of rasters.
#' @param highlight Highlight a region of the raster by alter alpha level of some parts.
#' @param invert Invert the highlighted region.
#'
#' @importFrom ggplot2 aes_string coord_equal facet_wrap geom_raster ggplot theme_bw scale_alpha_discrete
#' @importFrom methods setGeneric setMethod
#' @importFrom raster getValues ncell xyFromCell
#' @importFrom stats setNames
#' @importFrom utils stack
#'
#' @export

setGeneric("gg_raster",
           function(x, labels = names(x), nrow = 1, highlight = 1,
                    invert = FALSE) {
             standardGeneric("gg_raster")})

gg_raster_ <-
  function(x, labels, highlight, invert) {
    data <- stack(as.data.frame(getValues(x)))
    data <- setNames(data, c("value", "variable"))
    data <- cbind(data, xyFromCell(x, seq_len(ncell(x))))
    if (length(highlight) == 1) highlight <- c(0, highlight)
    if (length(highlight) == nrow(data)) data$hl <- highlight
    else {
      data$hl <- NA
      data[!is.na(data$value), "hl"] <-
        with(data[!is.na(data$value),],
             (value > quantile(value, highlight[1]) &
              value < quantile(value, highlight[2])) == !invert)
    }
    levels(data[, "variable"]) <- sort(labels)
    data[, "variable"] <- factor(data[, "variable"], levels = labels)

    ggplot(data, aes_string("x", "y")) + theme_bw() + coord_equal() +
    geom_raster(aes_string(fill = "value", alpha = "hl"), na.rm = TRUE) +
    scale_alpha_discrete(guide = "none", range = c(.15, 1))
  }

gg_raster__ <- function(x, labels, nrow, highlight, invert) {
  gg_raster_(x, labels, highlight, invert) + facet_wrap(~variable, nrow = nrow)}

#' @describeIn gg_raster plot a raster using gg_plot
#' @export
setMethod("gg_raster", "RasterLayer", gg_raster_)

#' @describeIn gg_raster plot a raster using gg_plot
#' @export
setMethod("gg_raster", "RasterStack", gg_raster__)

#' @describeIn gg_raster plot a raster using gg_plot
#' @export
setMethod("gg_raster", "RasterBrick", gg_raster__)


