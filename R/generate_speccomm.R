#' Generate Species Composition Matrix
#'
#' Create a species composition matrix using a list of species tables.
#'
#' @param spec_table_list A list of species tables created by the generate_spec_tables function.
#'
#' @return A species composition matrix
#' @export
#'
#' @examples # create a SCM using a list of species counts tables
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
#'
#'species_table_list <- list(
#'
#'  Spec_Tables_Caquetá$`2016-03-22`,
#'  Spec_Tables_Caquetá$`2016-03-29`,
#'  Spec_Tables_Caquetá$`2016-03-30`,
#'  Spec_Tables_Caquetá$`2016-04-13`
#'
#')
#'
#'SCM_Caquetá_2016 <- generate_speccomm(species_table_list)
#'
#'print(SCM_Caquetá_2016)
generate_speccomm <- function(spec_table_list){

  suppressWarnings({

  for(i in 1:length(spec_table_list)){

    spec_table_list[[i]] <- as.data.frame(spec_table_list[[i]])
    row.names(spec_table_list[[i]]) <- spec_table_list[[i]]$Var1
    spec_table_list[[i]]$Var1 <- NULL

  }

  for(i in 1:length(spec_table_list)){

    merged_df <- merge(spec_table_list[1], spec_table_list[2], by = "row.names", all = TRUE)

    count <- 3


    while (count <= length(spec_table_list)) {

      row.names(merged_df) <- merged_df$Row.names
      merged_df$Row.names <- NULL

      merged_df <- merge(merged_df, spec_table_list[[count]], by = "row.names", all = TRUE)

      count <- count + 1

    }

    row.names(merged_df) <- merged_df$Row.names

    merged_df$Row.names <- NULL

    merged_df[is.na(merged_df)] <- 0

    merged_df <- t(merged_df)

    row.names(merged_df) <- names(spec_table_list)

    return(merged_df)

  }

  })

}
