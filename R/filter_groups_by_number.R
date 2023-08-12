#' Filter Group by Name
#'
#' Filter a clusterised object that has been assigned groups and remove groups that are not a specified label.
#'
#' @param clusterised_object An object created by the clusterise_sites function.
#' @param group_number A number that identifies the group to be extracted.
#'
#' @return A new clusterised object that only contains sites from the specified group.
#' @export
#'
#' @examples # extract groups 3, 5, and 7 from a clusterised object
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
#'print(Colombia_Caquetá_3[[2]])
#'print(Colombia_Caquetá_5[[2]])
#'print(Colombia_Caquetá_7[[2]])
filter_groups_by_number <- function(clusterised_object, group_number){

  split_centred_coordinates <- split(clusterised_object[[2]],
                                     clusterised_object[[2]]$site_group)

  NSites <- numeric()
  for(i in 1:length(split_centred_coordinates)){
    NSites[i] <- length(split_centred_coordinates[[i]]$site_group)
  }

  index_filter <- clusterised_object[[2]]$site_group %in% group_number

  new_dataframe_list <- clusterised_object[[1]][index_filter]
  new_centred_coordinates <- clusterised_object[[2]][index_filter,]

  return(
    list(new_dataframe_list, new_centred_coordinates)
  )

}

