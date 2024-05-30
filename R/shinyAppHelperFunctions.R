#####Function to change variable name to component#####

variableList_heiComponents <- list( "Energy (Kilocalories)" = "KCAL",
                                    "Total Fruit" = "F_TOTAL",
                                    "Whole Fruits" = "FWHOLEFRT",
                                    'Total Vegetables' = 'VTOTALLEG',
                                    'Greens \u000Aand Beans' = 'VDRKGRLEG',
                                    'Whole Grains' = 'G_WHOLE',
                                    'Total Dairy' = 'D_TOTAL',
                                    'Total Protein' = 'PFALLPROTLEG',
                                    'Seafood/Plant \u000AProteins' = 'PFSEAPLANTLEG',
                                    'Fatty Acids' = 'TFACIDS',
                                    'Refined \u000AGrains' = 'G_REFINED',
                                    'Sodium' = 'TSODI',
                                    'Added Sugars' = 'ADD_SUGARS',
                                    'Saturated Fat' = 'TSFAT'
)

variableList_MCMC <- list( "Energy (Kilocalories)" = "KCAL",
                           "Whole Fruits" = "FWHOLEFRT",
                           "Fruit Juice" = "F_JUICE",
                           'Non-Dark Green \u000AVegetables' = 'VNONDRKGR',
                           'Dark Green \u000AVegetables' = 'V_DRKGR',
                           'Legumes \u000A(as a Vegetable)' = 'V_LEGUMES',
                           'Whole Grains' = 'G_WHOLE',
                           'Total Dairy' = 'D_TOTAL',
                           'Meat, Poultry, \u000Aand Eggs' = 'PF_MPE',
                           'Seafood, \u000ASoy, Nuts, \u000Aand Seeds' = 'PF_SSNS',
                           'Legumes \u000A(as a Protein Food)' = 'PF_LEGUMES',
                           'Total \u000AMono and Polyunsaturated \u000AFatty Acids' = 'MONOPOLY',
                           'Refined \u000AGrains' = 'G_REFINED',
                           'Sodium' = 'TSODI',
                           'Added Sugars' = 'ADD_SUGARS',
                           'Saturated Fat' = 'TSFAT'
)

demographicList <- list ( 'Sex' = 'SEX',
                          'Race' = 'RACE_ETH',
                          'Age' = 'ageBracket',
                          'Income' = 'FAMINC')

varToComponent <- function(target_name){
  variable_name <- NULL
  for (key in names(variableList_heiComponents)) {
    if (variableList_heiComponents[[key]] == target_name) {
      variable_name <- key
      return(variable_name)
    }
  }
  for (key in names(variableList_MCMC)) {
    if (variableList_MCMC[[key]] == target_name) {
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

#####Function to keep x axis constant#####


xVarMax <- function(variable, all_datasets, quartile = FALSE, kcal = FALSE){
  all_maxes <- c()
  for(years in all_datasets){
    # For adjusted by 1000 kcal
    if(kcal == TRUE){
      current_year <- years %>%
        tidyr::pivot_longer(cols = dplyr::contains(variable),
                            names_to = "day",
                            values_to = "recalls") %>%
        dplyr::mutate(kcal = dplyr::case_when(startsWith(day, "DR1") ~ DR1TKCAL,
                                              TRUE ~ DR2TKCAL)) %>%
        dplyr::select(recalls, kcal, DR1TKCAL, DR2TKCAL) %>%

        tidyr::drop_na() %>%
        dplyr::mutate(recalls = dplyr::case_when(kcal > 0 ~ recalls / kcal * 1000,
                                                 TRUE ~ 0))
    }
    else{
      current_year <- years %>%
        tidyr::pivot_longer(cols = dplyr::contains(variable),
                            names_to = "day",
                            values_to = "recalls") %>%
        dplyr::select(recalls) %>%
        tidyr::drop_na()
    }

    if(quartile == TRUE){
      maxCutoff <- stats::quantile(current_year$recalls, c(.95))
      current_year <- current_year %>%
        dplyr::filter(recalls < maxCutoff | recalls == 0)
    }

    all_maxes <- c(all_maxes, (max(current_year$recalls, na.rm = T)))

  }
  return(max(all_maxes))
}

#####Function to convert component amount to score#####
ratioToScoreApp <- function(componentName, componentRatio, scoringStandards){
  if(is.na(componentRatio)){
    return(NA)
  }


  componentType <- scoringStandards$component_type[scoringStandards$component == componentName]
  zeroScore <- scoringStandards$zero_score[scoringStandards$component == componentName]
  maxPoints <-  scoringStandards$max_points[scoringStandards$component == componentName]
  maxAmount <-  scoringStandards$max_amount[scoringStandards$component == componentName]

  if(componentType == "adequacy"){
    if(componentRatio >= maxAmount){
      return(maxPoints)
    }
    else{
      return((componentRatio - zeroScore)/(maxAmount - zeroScore) * maxPoints)
    }
  }

  else{
    if(componentRatio <= maxAmount){
      return(maxPoints)
    }
    else if(componentRatio >= zeroScore){
      return(0)
    }
    else{
      return((zeroScore - componentRatio)/(zeroScore - maxAmount) * maxPoints)
    }
  }
}

#####Simple Scoring Function#####
simpleScoreApp <- function(rawData, scoringVariable, scoringStandards){
  #choose scoring standards based on population selected

  simpleScoringData <- rawData %>%
    dplyr::select(SEQN, WTDR2D, dplyr::contains(scoringVariable), DR1TKCAL, DR2TKCAL) %>%
    dplyr::mutate(recall = rowSums(dplyr::select(.,dplyr::contains(scoringVariable))),
           KCAL = rowSums(dplyr::across(c(DR1TKCAL, DR2TKCAL))),
           ratio = dplyr::case_when(scoringVariable == "TSODI" ~ recall / KCAL,
                             scoringVariable == "TSFAT" ~ recall * 9 / KCAL * 100,
                             scoringVariable == "ADD_SUGARS" ~ recall * 16  / KCAL * 100,
                             scoringVariable == "TFACIDS" ~ recall,
                             TRUE ~ recall / KCAL * 1000)) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(score = ratioToScoreApp(scoringVariable, ratio, scoringStandards)) %>%
    dplyr::select(SEQN, WTDR2D, score)

}
#####Mean Ratio Method Function #####

meanRatioApp <- function(rawData, scoringVariable, scoringDemographicVariable, scoringStandards){

  meanRatioData <- rawData %>%
    dplyr::mutate(recall = rowSums(dplyr::select(.,dplyr::contains(scoringVariable))),
           KCAL = rowSums(dplyr::across(c(DR1TKCAL, DR2TKCAL))),
           ratio = dplyr::case_when(scoringVariable == "TSODI" ~ recall / KCAL,
                             scoringVariable == "TSFAT" ~ recall * 9 / KCAL * 100,
                             scoringVariable == "ADD_SUGARS" ~ recall * 16  / KCAL * 100,
                             scoringVariable == "TFACIDS" ~ recall,
                             TRUE ~ recall / KCAL * 1000))

  meanRatioByDemo <- meanRatioData %>%
    dplyr::group_by(!!scoringDemographicVariable) %>%
    dplyr::summarise(meanRatio = stats::weighted.mean(ratio, WTDR2D, na.rm = TRUE),
              score = ratioToScoreApp(scoringVariable, meanRatio, scoringStandards)) %>%
    dplyr::select(!meanRatio)

  return(meanRatioByDemo)

}

#####Population Ratio Method Function#####
popRatioScoreApp <- function(rawData, scoringVariable, scoringDemographicVariable, scoringStandards){
  popRatioData <- rawData %>%
    tidyr::pivot_longer(cols = dplyr::contains(scoringVariable),
                 names_to = "day",
                 values_to = "recall") %>%
    dplyr::mutate( KCAL = dplyr::case_when(grepl("DR1", day) ~ DR1TKCAL,
                             TRUE ~ DR2TKCAL)) %>%
    dplyr::select(recall, KCAL, WTDR2D, scoringDemographicVariable) %>%
    dplyr::group_by(!!scoringDemographicVariable) %>%
    dplyr::summarise(meanRecall = stats::weighted.mean(recall, WTDR2D, na.rm = TRUE),
              meanRecallSugar = stats::weighted.mean(recall*16, WTDR2D, na.rm = TRUE), #wish there was a different
              meanRecallSatFat = stats::weighted.mean(recall*9, WTDR2D, na.rm = TRUE), #way to do this
              meanEnergy = stats::weighted.mean(KCAL, WTDR2D, na.rm = TRUE))  %>%
    dplyr::ungroup() %>%
    dplyr::mutate(ratio = dplyr::case_when(scoringVariable == "TSODI" ~ meanRecall / meanEnergy,
                             scoringVariable == "TSFAT" ~ meanRecallSatFat / meanEnergy * 100,
                             scoringVariable == "ADD_SUGARS" ~ meanRecallSugar  / meanEnergy * 100,
                             scoringVariable == "TFACIDS" ~ meanRecall,
                             TRUE ~ meanRecall / meanEnergy * 1000)) %>%
    dplyr::group_by(!!scoringDemographicVariable) %>%
    dplyr::summarize(score = ratioToScoreApp(scoringVariable, ratio, scoringStandards))

  return(popRatioData)
}

varToComponent <- function(target_name){
  variable_name <- NULL
  for (key in names(variableList_heiComponents)) {
    if (variableList_heiComponents[[key]] == target_name) {
      variable_name <- key
      return(variable_name)
    }
  }
  for (key in names(variableList_MCMC)) {
    if (variableList_MCMC[[key]] == target_name) {
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

#####Function to convert component amount to score#####
ratioToScoreApp <- function(componentName, componentRatio, scoringStandards){
  if(is.na(componentRatio)){
    return(NA)
  }


  componentType <- scoringStandards$component_type[scoringStandards$component == componentName]
  zeroScore <- scoringStandards$zero_score[scoringStandards$component == componentName]
  maxPoints <-  scoringStandards$max_points[scoringStandards$component == componentName]
  maxAmount <-  scoringStandards$max_amount[scoringStandards$component == componentName]

  if(componentType == "adequacy"){
    if(componentRatio >= maxAmount){
      return(maxPoints)
    }
    else{
      return((componentRatio - zeroScore)/(maxAmount - zeroScore) * maxPoints)
    }
  }

  else{
    if(componentRatio <= maxAmount){
      return(maxPoints)
    }
    else if(componentRatio >= zeroScore){
      return(0)
    }
    else{
      return((zeroScore - componentRatio)/(zeroScore - maxAmount) * maxPoints)
    }
  }
}

#####Simple Scoring Function#####
simpleScoreApp <- function(rawData, scoringVariable, scoringStandards){
  #choose scoring standards based on population selected

  simpleScoringData <- rawData %>%
    dplyr::select(SEQN, WTDR2D, dplyr::contains(scoringVariable), DR1TKCAL, DR2TKCAL) %>%
    dplyr::mutate(recall = rowSums(dplyr::select(.,dplyr::contains(scoringVariable))),
           KCAL = rowSums(dplyr::across(c(DR1TKCAL, DR2TKCAL))),
           ratio = dplyr::case_when(scoringVariable == "TSODI" ~ recall / KCAL,
                             scoringVariable == "TSFAT" ~ recall * 9 / KCAL * 100,
                             scoringVariable == "ADD_SUGARS" ~ recall * 16  / KCAL * 100,
                             scoringVariable == "TFACIDS" ~ recall,
                             TRUE ~ recall / KCAL * 1000)) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(score = ratioToScoreApp(scoringVariable, ratio, scoringStandards)) %>%
    dplyr::select(SEQN, WTDR2D, score)

}
#####Mean Ratio Method Function #####

meanRatioApp <- function(rawData, scoringVariable, scoringDemographicVariable, scoringStandards){

  meanRatioData <- rawData %>%
    dplyr::mutate(recall = rowSums(dplyr::select(.,dplyr::contains(scoringVariable))),
           KCAL = rowSums(dplyr::across(c(DR1TKCAL, DR2TKCAL))),
           ratio = dplyr::case_when(scoringVariable == "TSODI" ~ recall / KCAL,
                             scoringVariable == "TSFAT" ~ recall * 9 / KCAL * 100,
                             scoringVariable == "ADD_SUGARS" ~ recall * 16  / KCAL * 100,
                             scoringVariable == "TFACIDS" ~ recall,
                             TRUE ~ recall / KCAL * 1000))

  meanRatioByDemo <- meanRatioData %>%
    dplyr::group_by(!!scoringDemographicVariable) %>%
    dplyr::summarise(meanRatio = stats::weighted.mean(ratio, WTDR2D, na.rm = TRUE),
              score = ratioToScoreApp(scoringVariable, meanRatio, scoringStandards)) %>%
    dplyr::select(!meanRatio)

  return(meanRatioByDemo)

}

#####Population Ratio Method Function#####
popRatioScoreApp <- function(rawData, scoringVariable, scoringDemographicVariable, scoringStandards){
  popRatioData <- rawData %>%
    tidyr::pivot_longer(cols = dplyr::contains(scoringVariable),
                 names_to = "day",
                 values_to = "recall") %>%
    dplyr::mutate( KCAL = dplyr::case_when(grepl("DR1", day) ~ DR1TKCAL,
                             TRUE ~ DR2TKCAL)) %>%
    dplyr::select(recall, KCAL, WTDR2D, scoringDemographicVariable) %>%
    dplyr::group_by(!!scoringDemographicVariable) %>%
    dplyr::summarise(meanRecall = stats::weighted.mean(recall, WTDR2D, na.rm = TRUE),
              meanRecallSugar = stats::weighted.mean(recall*16, WTDR2D, na.rm = TRUE), #wish there was a different
              meanRecallSatFat = stats::weighted.mean(recall*9, WTDR2D, na.rm = TRUE), #way to do this
              meanEnergy = stats::weighted.mean(KCAL, WTDR2D, na.rm = TRUE))  %>%
    dplyr::ungroup() %>%
    dplyr::mutate(ratio = dplyr::case_when(scoringVariable == "TSODI" ~ meanRecall / meanEnergy,
                             scoringVariable == "TSFAT" ~ meanRecallSatFat / meanEnergy * 100,
                             scoringVariable == "ADD_SUGARS" ~ meanRecallSugar  / meanEnergy * 100,
                             scoringVariable == "TFACIDS" ~ meanRecall,
                             TRUE ~ meanRecall / meanEnergy * 1000)) %>%
    dplyr::group_by(!!scoringDemographicVariable) %>%
    dplyr::summarize(score = ratioToScoreApp(scoringVariable, ratio, scoringStandards))

  return(popRatioData)
}
