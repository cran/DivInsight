#' Plot H against Date
#'
#' Plot Shannon's H on the Y axis and dates on the X axis with uniform spacing for each date and a line of best fit.
#'
#' @import dplyr
#'
#' @param clusterised_object An object created by the clusterise_sites function.
#' @param xlabel The X axis label.
#' @param ylabel The Y axis label with "Diversity (H)" as default.
#' @param main_title The main title with "Shannon's H over Time" as default.
#'
#' @return Returns a base R plot in the plot panel, depicting the trend of diversity index values over time.
#' @export
#'
#' @examples # create a trend chart showing the diversity over time at the Caquetá province
#'Colombia_Caquetá_dataframe <- subset(Colombia, stateProvince == "Caquetá")
#'
#'clusterised_Caquetá <- clusterise_sites(dataframe = Colombia_Caquetá_dataframe,
#'                                        cluster_min_length = 30,
#'                                        group_radius = 20000
#')
#'
#'plot_sites_trend_H(clusterised_Caquetá)
plot_sites_trend_H <- function(clusterised_object, xlabel = "", ylabel = "Diversity (H)", main_title = "Shannon's H over Time"){



  stats_dataframe <- generate_stats(

    clusterised_object

  )


  plot(x = stats_dataframe$date %>% as.ordered,
       y = stats_dataframe$H,

       xlab = xlabel,
       ylab = ylabel,

       main = main_title,

       cex.axis = 0.6,
       las = 2
  )

  abline(lm(stats_dataframe$H ~ stats_dataframe$date %>% as.ordered %>% as.numeric ))

}
