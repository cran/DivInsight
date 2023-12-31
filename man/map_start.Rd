% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/map_start.R
\name{map_start}
\alias{map_start}
\title{Create Site Map}
\usage{
map_start(clusterised_object, site_name = "", colour = "#60f")
}
\arguments{
\item{clusterised_object}{An object created by the clusterise_sites function.}

\item{site_name}{An optional label to add to the coordinate pins.}

\item{colour}{The colour of the coordinate pins.}
}
\value{
A Viewer object: an interactive map.
}
\description{
Create interactive map using centred site coordinates.
}
\examples{
# create an interactive map using one of the group's coordinates
Colombia_Caquetá_dataframe <- subset(Colombia, stateProvince == "Caquetá")

clusterised_Caquetá <- clusterise_sites(dataframe = Colombia_Caquetá_dataframe,
                                       cluster_min_length = 30,
                                       group_radius = 20000
)

print(clusterised_Caquetá[[2]])

Colombia_Caquetá_3 <- filter_groups_by_number(clusterised_Caquetá, 3)
Colombia_Caquetá_5 <- filter_groups_by_number(clusterised_Caquetá, 5)
Colombia_Caquetá_7 <- filter_groups_by_number(clusterised_Caquetá, 7)

Colombia_Caquetá_map <-

 map_start(

   clusterised_object = Colombia_Caquetá_3,
   site_name = "Caquetá#3",
   colour = "green"

 )
}
