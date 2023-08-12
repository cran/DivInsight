#' Create Site Map
#'
#' Create interactive map using centred site coordinates.
#'
#' @import leaflet
#'
#' @param clusterised_object An object created by the clusterise_sites function.
#' @param site_name An optional label to add to the coordinate pins.
#' @param colour The colour of the coordinate pins.
#'
#' @return A Viewer object: an interactive map.
#' @export
#'
#' @examples # create an interactive map using one of the group's coordinates
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
map_start <- function(

  clusterised_object,
  site_name = "",
  colour = "#60f"){

  map <- leaflet() %>%
    addTiles() %>%
    addCircleMarkers(
      lat   = clusterised_object[[2]]$latitude,
      lng   = clusterised_object[[2]]$longitude,
      popup = site_name,
      color = colour
    )

  return(map)

}
