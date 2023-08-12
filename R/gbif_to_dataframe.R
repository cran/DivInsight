#' Convert GBIF Object
#'
#' Convert a GBIF object into workable R dataframe.
#'
#' @import dplyr
#' @import rgbif
#'
#' @param GBIF_object An object queried from the GBIF database: a list containing occurrence data for a specified taxon and location.
#'
#' @return An R dataframe containing the occurrence data from the GBIF object.
#' @export
#'
#' @examples # convert the GBIF object containing occurrence plant data from 2020 and 2019
#'\donttest{
#'  library(rgbif)
#'
#'  Cambodia_code <- isocodes[grep("Cambodia", isocodes$name), "code"]
#'
#'  Plant_taxonkey <- name_backbone("Plantae")$kingdomKey
#'
#'  plant_cambodia.gbif <- occ_search(
#'    hasCoordinate = TRUE,
#'    limit = 99990,
#'    taxonKey = Plant_taxonkey,
#'    year = c("2020", "2019"),
#'    country = Cambodia_code
#'  )
#'
#'  plant_cambodia.df <- gbif_to_dataframe(plant_cambodia.gbif)
#'
#'  head(plant_cambodia.df)
#'}
gbif_to_dataframe <- function(GBIF_object){

  GBIF_object <- do.call(bind_rows, lapply(GBIF_object, function(x) x$data))

  return(GBIF_object)

}
