#' @importFrom raster raster
#' @export
extract_layers <- function(x, brick_list) lapply(brick_list, raster, layer = x)
