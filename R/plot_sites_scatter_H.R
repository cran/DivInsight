#' Plot H against Date
#'
#' Plot Shannon's H on the Y axis and dates on the X axis with scaled spacing for each date and scattered points.
#'
#' @import ggplot2
#'
#' @param clusterised_object An object created by the clusterise_sites function.
#' @param xlabel The X axis label with "Date" as default.
#' @param ylabel The Y axis label with "Diversity (H)" as default.
#' @param main_title The main label with "Shannon's H over Time" as default.
#'
#' @return Returns a ggplot2 scatter plot in the plot panel, showing the relationship between dates and diversity index values.
#' @export
#'
#' @examples # create a scatter plot showing the diversity over time at the Caquetá province
#'Colombia_Caquetá_dataframe <- subset(Colombia, stateProvince == "Caquetá")
#'
#'clusterised_Caquetá <- clusterise_sites(dataframe = Colombia_Caquetá_dataframe,
#'                                        cluster_min_length = 30,
#'                                        group_radius = 20000
#')
#'
#'plot_sites_scatter_H(clusterised_Caquetá)
plot_sites_scatter_H <- function(clusterised_object,
                                 xlabel = "Date",
                                 ylabel = "Diversity (H)",
                                 main_title = "Shannon's H over Time"){

  suppressWarnings({

  stats_dataframe <- generate_stats(clusterised_object)


    ggplot(stats_dataframe, aes(x = as.Date(stats_dataframe$date), y = stats_dataframe$H)) +
      geom_point() +
      labs(x = xlabel, y = ylabel, title = main_title)

  })

}
