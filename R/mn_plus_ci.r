#' @export
mn_plus_ci <-
  function(x) {
    setNames(
      c(mean(x, na.rm = TRUE), quantile(x, prob = c(.025, .975), na.rm = TRUE)),
      c("mean", "lower95CI", "upper95CI")
    )
  }
