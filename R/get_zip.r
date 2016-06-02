#' @importFrom httr GET write_disk
#' @importFrom utils unzip
#' @export
get_zip <-
  function(u, p, q, d) {
    GET(
      url = u,
      path = p,
      query = q,
      write_disk(d, overwrite = TRUE)
    )
    unzip(d, overwrite = TRUE, junkpaths = TRUE)
  }
