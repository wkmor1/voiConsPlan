#' @importFrom dplyr %>%
#' @importFrom raster crop getData merge subset xmax xmin
#' @export
get_bioclim <-
  function(layer, extent, var = "bio") {
    lapply(
      c(xmin, xmax),
      function(x) {
        getData(
          "worldclim",
          var = var,
          res = 0.5,
          lon = do.call(x, list(extent)),
          lat = ymin(extent)
        ) %>%
          crop(extent, snap = "out")
      }
    ) %>%
    do.call(merge, .) %>%
    subset(layer)
  }
