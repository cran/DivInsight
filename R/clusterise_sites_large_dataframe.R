#' Cluster Occurrence Data (large dataframe)
#'
#' Cluster a large occurrence dataframe by date with the option to group each cluster using a specified radius distance. Each of these clusters of data represents a site and a pair of centered coordinates for each site is generated.
#'
#' @import dplyr
#' @import vegan
#' @import geosphere
#'
#' @param dataframe A dataframe with occurrence data for the chosen taxon and location.
#' @param cluster_min_length The minimum number of observations in each cluster.
#' @param day_split_min_length  By default the function filters out days with fewer than 10 observations. This value adjusts the minimum threshold.
#' @param group_radius An optional value to have sites grouped. Group radius is measured in metres.
#'
#' @return The function returns a 'clusterised object', which is a list containing two elements: The first element is a list of data clusters. The second element is a dataframe that includes centred coordinates for each site, group number, and date.
#' @export
#'
#' @examples # clusterise sites for the entire Santander province of Colombia
#' \donttest{Colombia_Santander_dataframe <- subset(Colombia, stateProvince == "Santander")
#'
#' clusterised_Santander <- clusterise_sites_large_dataframe(
#'
#'  dataframe = Colombia_Santander_dataframe,
#'  cluster_min_length = 30
#'
#')
#'
#' print(clusterised_Santander[[2]])
#'
#'}
clusterise_sites_large_dataframe <-  function(

  dataframe,
  cluster_min_length,


  day_split_min_length = 10,
  group_radius = 40075000

){

  dataframe$date <- substr(dataframe$eventDate, 0, 10)

  dataframe_split_by_date <- split(dataframe, dataframe$date)

  dataframe_first_half <- dataframe_split_by_date[1 : round(length(dataframe_split_by_date)/2)]

  dataframe_first_half <- do.call(rbind, dataframe_first_half)

  dataframe_second_half <- dataframe_split_by_date[(round(length(dataframe_split_by_date)/2) + 1) : length(dataframe_split_by_date)]

  dataframe_second_half <- do.call(rbind, dataframe_second_half)

  clusterised_first_half <-
    clusterise_sites(
      dataframe = dataframe_first_half,
      cluster_min_length = cluster_min_length,
      group_radius = group_radius,

      day_split_min_length = day_split_min_length

    )

  clusterised_second_half <-
    clusterise_sites(
      dataframe = dataframe_second_half,
      cluster_min_length = cluster_min_length,
      group_radius = group_radius,

      day_split_min_length = day_split_min_length

    )

  rejoined_clusterised_object <- list()

  rejoined_clusterised_object[[1]] <-
    c(
      clusterised_first_half[[1]],
      clusterised_second_half[[1]]
    )

  rejoined_clusterised_object[[2]] <-
    rbind(
      clusterised_first_half[[2]],
      clusterised_second_half[[2]]
    )

  radius_for_regroups <- group_radius

  rejoined_clusterised_object <-
    site_regroup(

      clusterised_object = rejoined_clusterised_object,
      regroup_radius = radius_for_regroups
    )

  return(rejoined_clusterised_object)

}
