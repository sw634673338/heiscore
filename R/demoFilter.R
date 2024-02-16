#' @import magrittr

# filter dataset to only include selected demographic group
demoFilter <- function(dataset, sex, race_eth, age, fam_income) {
  filtered_data <- dataset %>%
    dplyr::filter(SEX %in% sex,
           RACE_ETH %in% race_eth,
           AGE >= age[1],
           AGE <= age[2],
           FAMINC %in% fam_income)

  return(filtered_data)
}
