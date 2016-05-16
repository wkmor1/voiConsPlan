#' Plot a raster using gg_plot
#'
#' Creates a facetted plot of a raster or set of rasters using ggplot.
#'
#' @param x A Raster* object.
#' @param labels Character vector. Facet labels.
#' @param nrow Integer. Number of rows for facets of rasters.
#'
#' @importFrom ggplot2 aes_string facet_wrap geom_raster ggplot theme_bw
#' @importFrom methods setGeneric setMethod
#' @importFrom raster getValues ncell xyFromCell
#' @importFrom utils stack
#'
#' @export

setGeneric("gg_raster",
           function(x, labels = names(x), nrow = 1) {
             standardGeneric("gg_raster")})

gg_raster_ <-
  function(x, labels) {
    data <- stack(as.data.frame(getValues(x)))
    data <- setNames(data, c("value", "variable"))
    data <- cbind(data, xyFromCell(x, seq_len(ncell(x))))

    levels(data[, "variable"]) <- sort(labels)
    data[, "variable"] <- factor(data[, "variable"], levels = labels)

    ggplot(data, aes_string("x", "y")) + theme_bw() +
    geom_raster(aes_string(fill = "value"), na.rm = TRUE)
  }

gg_raster__ <- function(x, labels, nrow) {
  gg_raster_(x, labels) + facet_wrap(~variable, nrow = nrow)}

#' @describeIn gg_raster plot a raster using gg_plot
#' @export
setMethod("gg_raster", "RasterLayer", gg_raster_)

#' @describeIn gg_raster plot a raster using gg_plot
#' @export
setMethod("gg_raster", "RasterStack", gg_raster__)

#' @describeIn gg_raster plot a raster using gg_plot
#' @export
setMethod("gg_raster", "RasterBrick", gg_raster__)


