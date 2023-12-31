% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/generate_spec_tables_df.R
\name{generate_spec_tables_df}
\alias{generate_spec_tables_df}
\title{Generate Species Tables}
\usage{
generate_spec_tables_df(dataframe, min_individuals = 0, min_species = 0)
}
\arguments{
\item{dataframe}{An occurrence dataframe.}

\item{min_individuals}{The minimum number of individuals in each table}

\item{min_species}{The minimum number of species in each table.}
}
\value{
A list of species counts tables.
}
\description{
Generate a list of tables for species counts from the input dataframe.
}
\examples{
# generate species count tables with a minimum of 30 individuals from 5 species
Colombia_Caquetá_dataframe <- subset(Colombia, stateProvince == "Caquetá")

Spec_Tables_Caquetá <- generate_spec_tables_df(

 dataframe = Colombia_Caquetá_dataframe,
 min_individuals = 30,
 min_species = 5

)

print(Spec_Tables_Caquetá$`2016-03-22`)
print(Spec_Tables_Caquetá$`2016-03-29`)
print(Spec_Tables_Caquetá$`2016-03-30`)
print(Spec_Tables_Caquetá$`2016-04-13`)
}
