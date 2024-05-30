varToComponent <- function(target_name){

  variableList_heiComponents <- list( "Kilocalories" = "KCAL",
                                      "Total Fruit" = "F_TOTAL",
                                      "Whole Fruits" = "FWHOLEFRT",
                                      'Total Vegetables' = 'VTOTALLEG',
                                      'Greens and Beans' = 'VDRKGRLEG',
                                      'Whole Grains' = 'G_WHOLE',
                                      'Total Dairy' = 'D_TOTAL',
                                      'Total Protein' = 'PFALLPROTLEG',
                                      'Seafood/Plant Proteins' = 'PFSEAPLANTLEG',
                                      'Fatty Acids' = 'TFACIDS',
                                      'Refined Grains' = 'G_REFINED',
                                      'Sodium' = 'TSODI',
                                      'Added Sugars' = 'ADD_SUGARS',
                                      'Saturated Fat' = 'TSFAT')

  demographicList <- list( "Sex" = "SEX",
                           "Race/Ethnicity" = "RACE_ETH",
                           "Age" = "ageBracket",
                           "Family Income" = "FAMINC")

  variable_name <- NULL
  for (key in names(variableList_heiComponents)) {
    if (variableList_heiComponents[[key]] == target_name) {
      variable_name <- key
      return(variable_name)
    }
  }
  for (key in names(demographicList)) {
    if (demographicList[[key]] == target_name) {
      variable_name <- key
      return(variable_name)
    }
  }
}
