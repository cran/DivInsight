#' Add to Site Map
#'
#' Add coordinate pins to an existing interactive map.
#'
#' @import leaflet
#'
#' @param existing_map A map object that has been created using the map_start function.
#' @param clusterised_object An object created by the clusterise_sites functions.
#' @param site_name An optional label to add the the coordinate pins.
#' @param colour The colour of the coordinate pins.
#'
#' @return Coordinate pins to be added to an existing interactive map object.
#' @export
#'
#' @examples # add site coordinates to an existing map
#' Colombia_Caquetá_dataframe <- subset(Colombia, stateProvince == "Caquetá")
#'
#'clusterised_Caquetá <- clusterise_sites(dataframe = Colombia_Caquetá_dataframe,
#'                                        cluster_min_length = 30,
#'                                        group_radius = 20000
#')
#'
#'print(clusterised_Caquetá[[2]])
#'
#'Colombia_Caquetá_3 <- filter_groups_by_number(clusterised_Caquetá, 3)
#'Colombia_Caquetá_5 <- filter_groups_by_number(clusterised_Caquetá, 5)
#'Colombia_Caquetá_7 <- filter_groups_by_number(clusterised_Caquetá, 7)
#'
#'Colombia_Caquetá_map <-
#'
#'  map_start(
#'
#'    clusterised_object = Colombia_Caquetá_3,
#'    site_name = "Caquetá#3",
#'    colour = "green"
#'
#'  )
#'
#'  Colombia_Caquetá_map <-
#'
#'  map_add(
#'
#'    existing_map = Colombia_Caquetá_map,
#'
#'    clusterised_object = Colombia_Caquetá_5,
#'    site_name = "Caquetá#5",
#'    colour = "purple"
#'
#'  )
#'
#'  Colombia_Caquetá_map <-
#'
#'  map_add(
#'
#'    existing_map = Colombia_Caquetá_map,
#'
#'    clusterised_object = Colombia_Caquetá_7,
#'    site_name = "Caquetá#7",
#'    colour = "blue"
#'
#'  )
map_add <- function(

  existing_map,

  clusterised_object,
  site_name = "",
  colour = "#09f"){

  existing_map <- existing_map %>%
    addCircleMarkers(
      lat  = clusterised_object[[2]]$latitude,
      lng  = clusterised_object[[2]]$longitude,
      popup = site_name,
      color = colour
    )


  return(existing_map)


}
