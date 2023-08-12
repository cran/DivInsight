## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(DivInsight)
data("Colombia")

## -----------------------------------------------------------------------------
head(Colombia[c(2:4, 31:32, 45)], 10)

## -----------------------------------------------------------------------------
# subset the dataframe by province name 
Colombia_Meta <- subset(Colombia, stateProvince == "Meta")

# cluster occurrence data by date and generate centred coordinates for each site
clusterised_Meta <- clusterise_sites(
  
  dataframe = Colombia_Meta,
  cluster_min_length = 30
  
)

## ---- view-results------------------------------------------------------------
# generate stats 
stats_Meta <- generate_stats(clusterised_Meta)

# view the stats table
print(stats_Meta)

## ---- fig.show='hold', fig.width=8, fig.height=6------------------------------
# plot trend charts 
plot_sites_trend_H(clusterised_object = clusterised_Meta, 
                   main_title = "Formicidae Diversity in the Meta Province")

