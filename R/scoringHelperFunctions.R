#' @import magrittr

### Convert ratio to score ###
ratioToScore <- function(componentName, componentRatio, ageMin){

  # Choose scoring standards based on population selected
  if(ageMin < 2){
    scoringStandards <- HEI_scoring_standards_toddlers
  }
  else{
    scoringStandards <- HEI_scoring_standards
  }

  if(is.na(componentRatio)){
    return(NA)
  }

  componentType <- scoringStandards$component_type[scoringStandards$component == componentName]
  zeroScore <- scoringStandards$zero_score[scoringStandards$component == componentName]
  maxPoints <-  scoringStandards$max_points[scoringStandards$component == componentName]
  maxAmount <-  scoringStandards$max_amount[scoringStandards$component == componentName]

  # Adequacy component scoring
  if(componentType == "adequacy"){
    if(componentRatio >= maxAmount){
      return(maxPoints)
    }
    else if(componentRatio <= zeroScore){
      return(0)
    }
    else{
      return((componentRatio - zeroScore)/(maxAmount - zeroScore) * maxPoints)
    }
  }

  # Moderation component scoring
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

### Simple Scoring Function ###
simpleScore <- function(rawData, scoringVariable, ageMin){
  simpleScoringData <- rawData %>%
    dplyr::select(SEQN, WTDR2D, dplyr::contains(scoringVariable), DR1TKCAL, DR2TKCAL, SEX, AGE, RACE_ETH, FAMINC) %>%
    dplyr::mutate(recall = rowSums(dplyr::select(.,dplyr::contains(scoringVariable)), na.rm = TRUE),
           KCAL = rowSums(dplyr::across(c(DR1TKCAL, DR2TKCAL)), na.rm = TRUE),
           ratio = dplyr::case_when(scoringVariable == "TSODI" ~ recall / KCAL,
                             scoringVariable == "TSFAT" ~ recall * 9 / KCAL * 100,
                             scoringVariable == "ADD_SUGARS" ~ recall * 16  / KCAL * 100,
                             scoringVariable == "TFACIDS" ~ recall,
                             TRUE ~ recall / KCAL * 1000))

  simpleScoringData <- simpleScoringData %>%
    dplyr::rowwise() %>%
    dplyr::mutate(score = ratioToScore(scoringVariable, ratio, ageMin)) %>%
    dplyr::select(SEQN, WTDR2D, SEX, AGE, RACE_ETH, FAMINC, score)

}

### Mean Ratio Function ###
meanRatioScore <- function(rawData, scoringVariable, scoringDemographicVariable, ageMin){

  meanRatioData <- rawData %>%
    dplyr::mutate(recall = rowSums(dplyr::select(.,dplyr::contains(scoringVariable)), na.rm = TRUE),
           KCAL = rowSums(dplyr::across(c(DR1TKCAL, DR2TKCAL)), na.rm = FALSE),
           ratio = dplyr::case_when(scoringVariable == "TSODI" ~ recall / KCAL,
                             scoringVariable == "TSFAT" ~ recall * 9 / KCAL * 100,
                             scoringVariable == "ADD_SUGARS" ~ recall * 16  / KCAL * 100,
                             scoringVariable == "TFACIDS" ~ recall,
                             TRUE ~ recall / KCAL * 1000))

  meanRatioByDemo <- meanRatioData %>%
    dplyr::group_by(!!scoringDemographicVariable) %>%
    dplyr::summarise(meanRatio = stats::weighted.mean(ratio, WTDR2D, na.rm = TRUE),
              score = ratioToScore(scoringVariable, meanRatio, ageMin)) %>%
    dplyr::select(!meanRatio)

  return(meanRatioByDemo)

}

### Population Ratio Function ###
popRatioScore <- function(rawData, scoringVariable, scoringDemographicVariable, ageMin){

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
    dplyr::summarize(score = ratioToScore(scoringVariable, ratio, ageMin))

  return(popRatioData)
}
