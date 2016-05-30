#' @export
fmt <- function(x, digits = 0) {
  format(round(x, digits), nsmall = digits, big.mark = ",", scientific = FALSE)
}
