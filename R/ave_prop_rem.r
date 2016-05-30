#' @importFrom raster cellStats
#' @export
ave_prop_rem <-
  function(x, y, p) {
    ans <- cellStats(x * (y > p), sum) / cellStats(x, sum)
    sum(ans, na.rm = TRUE) / nlayers(x)
  }
