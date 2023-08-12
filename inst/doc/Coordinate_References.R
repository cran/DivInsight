## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(DivInsight)
data("Colombia")

## -----------------------------------------------------------------------------
# clusterise data for a specific location 
Colombia_coordinate_ref_1 <- clusterise_sites(
  
  dataframe =
    subset_by_coordinate_ref(
      dataframe = Colombia,
      coordinate_reference = c(-73.325377, 3.956982),
      distance_threshold = 50000
    ),
  
  cluster_min_length = 30 
  
)

# repeat the process with different coordinates
Colombia_coordinate_ref_2 <- clusterise_sites(
  
  dataframe = 
    subset_by_coordinate_ref(
      dataframe = Colombia,
      coordinate_reference = c(-71.889919, 4.470524),
      distance_threshold = 50000
    ),
  
  cluster_min_length = 30 
  
)

## ---- fig.show='hold', fig.width=4, fig.height=3------------------------------
# scatter plot for reference site 1
plot_sites_scatter_H(
  
  clusterised_object = Colombia_coordinate_ref_1,
  main = "Coordinate Reference Site #1"
  
) 

# scatter plot for reference site 2
plot_sites_scatter_H(
  
  clusterised_object = Colombia_coordinate_ref_2,
  main = "Coordinate Reference Site #2"
  
) 

## -----------------------------------------------------------------------------
# create a new map using coordinates from first reference site
Colombia_coordinate_ref_map <-
  
  map_start(

    clusterised_object = Colombia_coordinate_ref_1,
    site_name = "Ref#1",    
    colour = "purple"
    
  )

# add to the existing map using information from the second reference site
Colombia_coordinate_ref_map <-
  
  map_add(
    
    existing_map = Colombia_coordinate_ref_map,
    
    clusterised_object = Colombia_coordinate_ref_2,
    site_name = "Ref#2",
    colour = "darkred"
    
  )

## ---- fig.show='hold', fig.width=8, fig.height=6------------------------------
# view the interactive map
Colombia_coordinate_ref_map

