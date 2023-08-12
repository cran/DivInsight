## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(DivInsight)
data("Colombia")
library(vegan)

## -----------------------------------------------------------------------------
# use coordinates to clusterise data 
Colombia_coordinate_ref <- clusterise_sites(
  
  dataframe = 
    subset_by_coordinate_ref(
      dataframe = Colombia,
      coordinate_reference = c(-73.487520, 7.539986),
      distance_threshold = 50000
    ),
  
  cluster_min_length = 30 
  
)

## -----------------------------------------------------------------------------
# regroup sites with a radius of 1km
cref_1km <- site_regroup(
  
  clusterised_object = Colombia_coordinate_ref,
  regroup_radius = 1000
  
)

## -----------------------------------------------------------------------------
cref_1km_spectables <- generate_spec_tables(
  
  clusterised_object = cref_1km,
  min_individuals = 30,
  min_species = 10
  
)

## -----------------------------------------------------------------------------
# view the names of each table
print(names(cref_1km_spectables))

# store the chosen species tables into a single list 
species_table_list <- list(
  
  cref_1km_spectables$`17.2022-04-04`,
  cref_1km_spectables$`29.2022-04-02`,
  cref_1km_spectables$`16.2022-04-01`,
  cref_1km_spectables$`26.2022-03-31`,
  cref_1km_spectables$`1.2022-03-30`,
  
  cref_1km_spectables$`28.2022-03-29`,
  cref_1km_spectables$`18.2022-03-28`,
  cref_1km_spectables$`27.2022-03-26`,
  cref_1km_spectables$`9.2022-03-25`,
  cref_1km_spectables$`25.2022-03-23`,
  
  cref_1km_spectables$`8.2022-03-21`,
  cref_1km_spectables$`7.2022-03-19`,
  cref_1km_spectables$`24.2022-03-07`,
  cref_1km_spectables$`23.2022-03-06`,
  cref_1km_spectables$`19.2022-03-05`,
  
  cref_1km_spectables$`22.2022-02-27`,
  cref_1km_spectables$`21.2022-02-15`,
  cref_1km_spectables$`20.2022-02-14`
  
)     

# generate a species composition matrix
SCM1 <- generate_speccomm(species_table_list)

## -----------------------------------------------------------------------------
# change the row names of the matrix
SCM1 <- as.data.frame(SCM1)

row.names(SCM1) <- c(
  
  "17.2022-04-04",
  "29.2022-04-02",
  "16.2022-04-01",
  "26.2022-03-31",
  "1.2022-03-30",
  
  "28.2022-03-29",
  "18.2022-03-28",
  "27.2022-03-26",
  "9.2022-03-25",
  "25.2022-03-23",
  
  "8.2022-03-21",
  "7.2022-03-19",
  "24.2022-03-07",
  "23.2022-03-06",
  "19.2022-03-05",
  
  "22.2022-02-27",
  "21.2022-02-15",
  "20.2022-02-14"

)

SCM1 <- as.matrix(SCM1)

## ---- fig.show='hold', fig.width=5, fig.height=3------------------------------
# create a species accumulation curve 
speccurve1 <- specaccum(SCM1, method = "random")

# plot the species accumulation curve
plot(speccurve1, ci.type="poly", 
     col="blue", 
     ci.col="lightblue",
     main = "Species accumulation curve for 18 Sites"
)

# view the predictions from the species accumulation curve
print(speccurve1)

