#' Retrieve a tibble with WWEIA NHANES data
#'
#' This function retrieves a tibble containing the raw 24-hour recall data from the What We Eat in America (WWEIA), National Health and Nutrition Examination Survey (NHANES) converted to the U.S. Department of Agriculture (USDA) Food Patterns components from the NHANES cycle selected.
#'
#' @param years A single character string representing the NHANES cycle to select, choose from: "0506", "0708", "0910", "1112", "1314", "1516", or "1718".
#'
#' @return A tibble containing Food Patterns component consumption, sample weights, and demographic information for each subject in the selected NHANES cycle.
#'
#' @examples
#' # Retrieve WWEIA NHANES data for the 2015-2016 cycle
#' FPED_1516 <- selectDataset(years = "1516")
#' FPED_1516
#'
#' # Retrieve WWEIA NHANES data for the years 2017-2018
#' FPED_1718 <- selectDataset(years = "1718")
#' FPED_1718
#'
#' @export

selectDataset <- function(years) {
  years <- trimws(years)
  possible_dataset <- get0(paste("fped_", years, sep = ""), envir = asNamespace("heiscore"))

  return(possible_dataset)
}
