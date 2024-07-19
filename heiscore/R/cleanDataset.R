#' @import magrittr

cleanDataset <- function(dataset){
  # keep only subjects that participated and have a valid age (> 1 yr)
  cleanData <- dataset[dataset$WTDR2D != 0 & dataset$AGE >= 1,] %>%
    tidyr::drop_na(WTDR2D) %>%
    dplyr::mutate(
      # create ageBracket variable
      ageBracket = dplyr::case_when(
        AGE < 2 ~ "Toddler (12 - 23 mo.)",
        AGE >= 2 & AGE < 10 ~ "[2,10)",
        AGE >= 10 & AGE < 20 ~ "[10,20)",
        AGE >= 20 & AGE < 30 ~ "[20,30)",
        AGE >= 30 & AGE < 40 ~ "[30,40)",
        AGE >= 40 & AGE < 50 ~ "[40,50)",
        AGE >= 50 & AGE < 60 ~ "[50,60)",
        AGE >= 60 & AGE < 70 ~ "[60,70)",
        AGE >= 70 & AGE < 80 ~ "[70,80)",
        AGE >= 80 ~ "80+"
      )
    )

  # convert empty strings to NA values
  cleanData$FAMINC[is.na(cleanData$FAMINC)] <- "NA"

  return(cleanData)
}
