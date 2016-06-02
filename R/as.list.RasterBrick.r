#' @importFrom raster nlayers raster
#' @export
as.list.RasterBrick <-
  function(x, ...) lapply(seq_len(nlayers(x)), function(y) raster(x, y))
