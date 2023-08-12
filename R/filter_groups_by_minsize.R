#' Filter Group by Size
#'
#' Filter a clusterised object by removing groups below a specified size.
#'
#' @param clusterised_object An object created by the clusterise_sites functions.
#' @param min_group_size The minimum size of each group.
#'
#' @return A new clusterised object with groups of at least the specified minimum size.
#' @export
#'
#' @examples # return groups that have at least 10 sites
#'
#' Colombia_Huila_dataframe <- subset(Colombia, stateProvince == "Huila")
#'
#' clusterised_Huila <- clusterise_sites(dataframe = Colombia_Huila_dataframe,
#'                                       cluster_min_length = 30,
#'                                       group_radius = 20000
#' )
#'
#' clusterised_Huila_largegroups <-
#'   filter_groups_by_minsize(
#'
#'     clusterised_object = clusterised_Huila,
#'     min_group_size = 10
#'
#')
#'
#' print(clusterised_Huila_largegroups[[2]])
#'
filter_groups_by_minsize <- function(clusterised_object, min_group_size){

  split_centred_coordinates <- split(clusterised_object[[2]],
                                     clusterised_object[[2]]$site_group)

  NSites <- numeric()
  for(i in 1:length(split_centred_coordinates)){
    NSites[i] <- length(split_centred_coordinates[[i]]$site_group)
  }

  names(split_centred_coordinates[NSites >= min_group_size])

  index_filter <- clusterised_object[[2]]$site_group %in% as.numeric(names(split_centred_coordinates[NSites >= min_group_size]) )

  new_dataframe_list <- clusterised_object[[1]][index_filter]
  new_centred_coordinates <- clusterised_object[[2]][index_filter,]

  return(
    list(new_dataframe_list, new_centred_coordinates)
  )

}
