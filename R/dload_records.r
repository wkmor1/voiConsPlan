#' @importFrom ALA4R occurrences
#' @importFrom dplyr %>% filter_ select_
#' @importFrom raster extract
#' @importFrom rgeos gConvexHull writeWKT
#' @importFrom sp "coordinates<-" CRS over proj4string "proj4string<-" spTransform
#' @importFrom utils globalVariables
#' @export
dload_records <-
  function(species_group, filters, poly, proj, mask, coords) {
    records <-
      lapply(
        paste0("[", LETTERS, " TO ", paste0(LETTERS, "zzzzz"), "]"),
        function(x) {
          occurrences(
            taxon  = paste0('taxon_name:', x),
            wkt    = writeWKT(gConvexHull(poly)),
            fq     = c(sprintf("species_group:%s", species_group),
                       "coordinate_uncertainty:[0 TO 100]",
                       "-duplicate_status:D", "geospatial_kosher:true",
                       "year:[1996 TO 2016]"),
            qa     = filters,
            download_reason_id = 4
          ) %>%
            getElement("data")
        }
      ) %>%
      do.call(rbind, .) %>%
      filter_(paste("!", filters, collapse = "&")) %>%
      select_("species", coords[1], coords[2])
    coordinates(records) <- coords
    proj4string(records) <- "+init=epsg:4326"
    if (!missing(mask)) {
      records <- spTransform(records, CRS(proj4string(mask)))
      records <- records[!is.na(extract(mask, records)), ]
    } else {
      records <- spTransform(records, CRS(proj4string(poly)))
      o <- over(records, poly)
      records <- records[!is.na(o), ]
    }
    spTransform(records, CRS(proj)) %>%
      as.data.frame %>%
      data.frame(species_group = species_group, ., stringsAsFactors = FALSE)
  }

globalVariables(".")
