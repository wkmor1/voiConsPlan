#' @importFrom dismo maxent
#' @export
fit_maxent_model <-
  function(sp, data, bg, preds, coords = c("longitude", "latitude"),
           maxent_args = c("hinge=FALSE", "threshold=FALSE"),
           sp_col = "species", sp_grp_col = "species_group",
           bg_sp_grp_col = "species_group") {
    data <- as.data.frame(data)
    bg <- as.data.frame(bg)
    data_sp <- data[, sp_col]
    data_spp_grp <- unique(data[, sp_grp_col])
    bg_spp_grp <- bg[, bg_sp_grp_col]
    maxent(
      x = preds,
      p = data[grepl(gsub(" ", ".*", sp), data_sp), coords],
      a = bg[bg_spp_grp == data_spp_grp, coords],
      args = maxent_args
    )
  }
