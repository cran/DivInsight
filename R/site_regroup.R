#' Regroup Sites
#'
#' Takes a set of coordinates and groups them based on a specified radius.
#'
#' @import geosphere
#'
#' @param clusterised_object An object created by the clusterise_sites function.
#' @param regroup_radius The specified radius of each group.
#'
#' @return The clusterised object with new group labels.
#' @export
#'
#' @examples # regroup the sites with a group radius of 5km instead of 20km
#'Colombia_Caquetá_dataframe <- subset(Colombia, stateProvince == "Caquetá")
#'
#'clusterised_Caquetá <- clusterise_sites(dataframe = Colombia_Caquetá_dataframe,
#'                                        cluster_min_length = 30,
#'                                        group_radius = 20000
#')
#'
#'print(clusterised_Caquetá[[2]])
#'
#'clusterised_Caquetá_5km_sites <- site_regroup(
#'
#'  clusterised_object = clusterised_Caquetá,
#'  regroup_radius = 5000
#'
#')
#'
#'print(clusterised_Caquetá_5km_sites[[2]])
site_regroup <- function(clusterised_object,
                         regroup_radius = 40075000){

  dmat <- distm(clusterised_object[[2]][, c("longitude", "latitude")], fun = distHaversine)

  site_group_newlabels <- cutree(hclust(as.dist(dmat)), h = regroup_radius)

  clusterised_object[[2]]$site_group <- site_group_newlabels

  for(i in 1:length(clusterised_object[[1]])){

    clusterised_object[[1]][[i]]$site_group <- site_group_newlabels[i]

  }

  return(clusterised_object)

}
