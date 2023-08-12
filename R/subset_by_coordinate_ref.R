#' Subset Occurrence Dataframe
#'
#' Subsets an occurrence dataframe by using a pair of reference coordinates and a radius distance.
#'
#' @import geosphere
#'
#' @param dataframe A dataframe from which occurrence data will be subset
#' @param coordinate_reference A pair of longitude/latitude coordinates that will mark the centre of the occurrence data
#' @param distance_threshold The distance from the coordinate reference that will be included in the subset
#'
#' @return A dataframe with observations that occurred within the distance threshold from the coordinate reference
#' @export
#'
#' @examples # subset the occurrence data returning only data points 50km from the reference coordinates
#'Colombia_site_1 <-  subset_by_coordinate_ref(
#'
#'  dataframe = Colombia,
#'  coordinate_reference = c(-74.083310, 7.488485),
#'  distance_threshold = 50000
#'
#')
#'
#'head(Colombia_site_1)
subset_by_coordinate_ref <- function(dataframe,
                                     coordinate_reference,
                                     distance_threshold){

  coordinates <- data.frame(longitude = dataframe$decimalLongitude,
                            latitude = dataframe$decimalLatitude)

  coordinates$is_within_range <-
    ifelse(test = distHaversine(p1 = coordinate_reference,
                                p2 = coordinates) < distance_threshold,
           yes  = "Y",
           no   = "N"
    )

  subsetted_dataframe <- subset(dataframe, coordinates$is_within_range == "Y")

  return(subsetted_dataframe)

}
