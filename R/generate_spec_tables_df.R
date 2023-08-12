#' Generate Species Tables
#'
#' Generate a list of tables for species counts from the input dataframe.
#'
#' @param dataframe An occurrence dataframe.
#' @param min_individuals The minimum number of individuals in each table
#' @param min_species The minimum number of species in each table.
#'
#' @return A list of species counts tables.
#' @export
#'
#' @examples # generate species count tables with a minimum of 30 individuals from 5 species
#'Colombia_Caquetá_dataframe <- subset(Colombia, stateProvince == "Caquetá")
#'
#'Spec_Tables_Caquetá <- generate_spec_tables_df(
#'
#'  dataframe = Colombia_Caquetá_dataframe,
#'  min_individuals = 30,
#'  min_species = 5
#'
#')
#'
#'print(Spec_Tables_Caquetá$`2016-03-22`)
#'print(Spec_Tables_Caquetá$`2016-03-29`)
#'print(Spec_Tables_Caquetá$`2016-03-30`)
#'print(Spec_Tables_Caquetá$`2016-04-13`)
generate_spec_tables_df <- function(

  dataframe,
  min_individuals = 0,
  min_species = 0

){

  dataframe$date <- substr(dataframe$eventDate, 0, 10)
  split_dataframe <- split(dataframe, dataframe$date)
  species_tables <- lapply(split_dataframe, function(df) table(df$scientificName))

  NIndividuals <- numeric()
  for(i in 1:length(species_tables)){

    NIndividuals[i] <- sum(species_tables[[i]])

  }

  species_tables <- species_tables[NIndividuals > min_individuals]

  NSpecies <- numeric()
  for(i in 1:length(species_tables)){

    NSpecies[i] <- base::nrow(species_tables[[i]])

  }

  species_tables <- species_tables[NSpecies > min_species]

}
