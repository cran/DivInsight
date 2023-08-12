## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(DivInsight)
library(dplyr)
data("Colombia")

## -----------------------------------------------------------------------------
# subset the dataframe by province name 
Colombia_Meta <- subset(Colombia, stateProvince == "Meta")

# cluster occurrence data by date and generate centred coordinates for each site
clusterised_Meta_20km_groups <- clusterise_sites(
  dataframe = Colombia_Meta,
  cluster_min_length = 30,
  
  group_radius = 20000
)

# view the information for each site/cluster
print(clusterised_Meta_20km_groups[[2]])

## -----------------------------------------------------------------------------
# use table() and sort() to see how many sites there are in each group
clusterised_Meta_20km_groups[[2]]$site_group %>% table %>% sort %>% print

## ---- fig.show='hold', fig.width=4, fig.height=3------------------------------
# store the data for each group
Colombia_Meta_1 <- filter_groups_by_number(clusterised_Meta_20km_groups, 1)
Colombia_Meta_5 <- filter_groups_by_number(clusterised_Meta_20km_groups, 5)
Colombia_Meta_6 <- filter_groups_by_number(clusterised_Meta_20km_groups, 6)
Colombia_Meta_8 <- filter_groups_by_number(clusterised_Meta_20km_groups, 8)

# plot the data in a scatter plot
plot_sites_scatter_H(Colombia_Meta_1, main = "Shannons H at Meta #1") 
plot_sites_scatter_H(Colombia_Meta_5, main = "Shannons H at Meta #5") 
plot_sites_scatter_H(Colombia_Meta_6, main = "Shannons H at Meta #6") 
plot_sites_scatter_H(Colombia_Meta_8, main = "Shannons H at Meta #8") 

## -----------------------------------------------------------------------------
# create a new map using group 1's coordinates
Colombia_Meta_map <- 
  
  map_start(
    
    clusterised_object = Colombia_Meta_1,
    site_name = "Meta #1",
    colour = "green"
  )

# add group 5's coordinates to the map object 
Colombia_Meta_map <-
  
  map_add(
    
    existing_map = Colombia_Meta_map,
    
    clusterised_object = Colombia_Meta_5,
    site_name = "Meta #5",
    colour = "purple"
  )

# add group 6's coordinates to the map object
Colombia_Meta_map <-
  
  map_add(
    
    existing_map = Colombia_Meta_map,
    
    clusterised_object = Colombia_Meta_6,
    site_name = "Meta #6",
    colour = "blue"
  )

# add group 8's coordinates to the map object
Colombia_Meta_map <-
  
  map_add(
    
    existing_map = Colombia_Meta_map,
    
    clusterised_object = Colombia_Meta_8,
    site_name = "Meta #8",
    colour = "red"
  )

## ---- fig.show='hold', fig.width=8, fig.height=6------------------------------
# view the map
Colombia_Meta_map

