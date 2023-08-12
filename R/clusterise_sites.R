#' Cluster Occurrence Data
#'
#' Cluster occurrence data by date with the option to group each cluster using a specified radius distance. Each of these clusters of data represents a site and a pair of centered coordinates for each site is generated.
#'
#' @import dplyr
#' @import vegan
#' @import geosphere
#'
#' @param dataframe A dataframe with occurrence data for the chosen taxon and location.
#' @param cluster_min_length The minimum number of observations in each cluster.
#' @param group_radius An optional value to have sites grouped. Group radius is measured in metres.
#' @param day_split_min_length  By default the function filters out days with fewer than 10 observations. This value adjusts the minimum threshold.
#'
#' @return The function returns a 'clusterised object', which is a list containing two elements: The first element is a list of data clusters. The second element is a dataframe that includes centred coordinates for each site, group number, and date.
#' @export
#'
#' @examples # clusterise sites for the entire Huila province of Colombia
#' Colombia_Huila_dataframe <- subset(Colombia, stateProvince == "Huila")
#'
#' clusterised_Huila <- clusterise_sites(dataframe = Colombia_Huila_dataframe,
#'                                       cluster_min_length = 30,
#'                                       group_radius = 20000
#' )
#'
#' print(clusterised_Huila[[2]])
clusterise_sites <- function(

  dataframe,
  cluster_min_length,
  group_radius = 40075000,
  day_split_min_length = 10

){


  suppressWarnings({

    dataframe$date <- substr(dataframe$eventDate, start = 0, stop = 10)

    day_split <- split(dataframe, dataframe$date)

    NObservations <- numeric()
    for(i in 1:length(day_split)){
      NObservations[i] <-  day_split[[i]]$key  %>% length
    }

    day_split <- day_split[NObservations >= day_split_min_length]

    combined_dataframe <- do.call(rbind, day_split)

    combined_dataframe$date <- substr(combined_dataframe$eventDate, start = 0, stop = 10)

    distance_matrix <- distm(combined_dataframe[, c("decimalLongitude", "decimalLatitude")], fun = distHaversine)

    site_group <- cutree(hclust(as.dist(distance_matrix)), h = group_radius)

    combined_dataframe$site_group <- site_group

    split_dataframe <- split(combined_dataframe, list(combined_dataframe$date, combined_dataframe$site_group))

    NObservations <- numeric()
    for(i in 1:length(split_dataframe)){
      NObservations[i] <-  split_dataframe[[i]]$date  %>% length
    }

    NObservations <- data.frame(
      date = names(split_dataframe),
      observations = NObservations
    )

    split_dataframe <- split_dataframe[NObservations$observations >= cluster_min_length]

    coordinates_list <- list()
    for(i in 1:length(split_dataframe)){

      coordinates_list[[i]] <- split_dataframe[[i]][c("decimalLongitude", "decimalLatitude")]

    }

    centroid_function_1 <- function(x){
      if(
        length(unique(   as.data.frame(x)$decimalLongitude   ))  > 2 &
        length(unique(   as.data.frame(x)$decimalLatitude   ))  > 2
      ){
        return(geosphere::centroid(   as.data.frame(x)   ))
      }
      else{
        return("does not reach conditions")
      }
    }

    centroid_function_2 <- function(x){

      if(
        length(unique(   as.data.frame(x)$decimalLongitude   ))  < 3 ||
        length(unique(   as.data.frame(x)$decimalLatitude   ))  < 3
      ){
        return(lapply(as.data.frame(x), mean))
      }
      else{
        return("does not reach conditions")
      }
    }

    result_list <- lapply(coordinates_list, function(df) {
      if (
        length(unique(   df$decimalLongitude   ))  > 2 &
        length(unique(   df$decimalLatitude   ))  > 2
      ) {
        return(centroid_function_1(df))
      } else if (
        length(unique(   df$decimalLongitude   ))  < 3 ||
        length(unique(   df$decimalLatitude   ))  < 3
      ) {
        return(centroid_function_2(df))
      } else {
        return("does not reach conditions")
      }
    })

    centred_coordinates <- lapply(result_list, as.numeric)

    centred_coordinates <- unlist(centred_coordinates)

    centred_coordinates <- data.frame(

      longitude = centred_coordinates[seq(from = 1,
                                          to = length(centred_coordinates),
                                          by = 2)],

      latitude = centred_coordinates[seq(from = 2,
                                         to = length(centred_coordinates),
                                         by = 2)]

    )

    uncentred_coordinates <- lapply(centred_coordinates$longitude, is.nan) %>%
      unlist %>%
      which

    midrange <- function(x) {
      (max(x) + min(x)) / 2
    }

    for(i in uncentred_coordinates){

      centred_coordinates$longitude[i] <- as.data.frame(coordinates_list[i])$decimalLongitude %>% midrange
      centred_coordinates$latitude[i]  <- as.data.frame(coordinates_list[i])$decimalLatitude %>% midrange

    }

    for(i in 1:length(split_dataframe)){

      centred_coordinates$site_group[i] <- split_dataframe[[i]]$site_group %>% unique

    }

    for(i in 1:length(split_dataframe)){

      centred_coordinates$date[i] <- split_dataframe[[i]]$date %>% unique

    }

    return(
      list(split_dataframe, centred_coordinates)
    )



  })



}
