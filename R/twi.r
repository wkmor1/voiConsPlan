#' @importFrom topmodel sinkfill topidx
#' @importFrom raster as.matrix setValues xres
#' @export
twi <-
  function(x, sinkfill = 4, degree = .05) {
    y <- as.matrix(x)
    for (i in seq_len(sinkfill)) {
      y <- sinkfill(y, xres(x), degree)
    }
    setValues(x, topidx(y, xres(x))$atb)
  }
