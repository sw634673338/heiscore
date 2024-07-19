#' Calculate Healthy Eating Index (HEI) scores from NHANES data
#'
#' This function calculates HEI component or total scores using the inputted scoring method. The user can subset the data to only include subjects in specific demographic groups

#' @param method A single character string with the HEI scoring method to use. Choose from "simple", "pop ratio", or "mean ratio".
#' @param years A single character string representing the NHANES cycle to select, choose from: "0506", "0708", "0910", "1112", "1314", "1516", or "1718".
#' @param component A single character string with the HEI component to score and plot. Options include "total score", "total fruit", "whole fruits", "total vegetables", "greens and beans", "whole grains", "total dairy", "total protein", "seafood and plant proteins", "fatty acids", "refined grains", "sodium", "added sugars", and "saturated fat".
#' @param demo A single character string with the demographic grouping by which the data should be scored or NULL. If method = "simple", choose NULL as the demo. Otherwise, choose from "sex", "race", "age", or "income".
#' @param sex A vector of the sexes in the desired subpopulation. Provide a vector with the character strings "Female", "Male", or both.
#' @param race A vector of races/ethnicities in the desired subpopulation. Provide a vector including any combination of the following character strings: "Asian", "White", "Black", "Other", "Mexican American", and "Other Hispanic".
#' @param age A vector in the form c(min, max) with two integers specifying the desired age range to analyze. Both integers should either be ones (to represent the toddler age group including ages 12-23 months) or 2 and above.
#' @param income  A vector of family income brackets in the desired subpopulation. Provide a vector including any combination of the following character strings: "[0, 5000)","[5000, 10000)","[10000, 15000)","[15000, 20000)","[20000, 25000)","[25000, 35000)", "[35000, 45000)","[45000, 55000)","[55000, 65000)","[65000, 75000)","[75000, 100000)", "75000+",">100000", ">20000","<20000","Refused","Don't know", "NA".
#'
#' @return A tibble with HEI scores for the selected component for each individual (when method = "simple") or by demographic grouping specified in demo.
#'
#' @examples
#' # Calculate the Total Fruit component score from the 2017-18 NHANES data
#' # using the 'simple' method.
#' score(method = "simple",
#'                     years = "1718",
#'                     component = "Total Fruit",
#'                     demo = NULL)
#'
#' # Calculate the total HEI score by sex using the population ratio method for
#' # White and Black individuals between ages 5 and 10 with a family income
#' # between 5000abd 15000 in the 2011-12 NHANES cycle.
#' score(method = "pop ratio",
#'                     years = "1112",
#'                     component = "total score",
#'                     demo = "sex",
#'                     race = c("White", "Black"),
#'                     age = c(5,10),
#'                     income = c("[5000, 10000)", "[10000, 15000)"))
#'
#' @import magrittr
#'
#' @export

score <- function(method, years, component, demo = NULL, sex = c("Female", "Male"), race = c("Asian", "White", "Black", "Other", "Mexican American", "Other Hispanic"), age = c(2, 100), income = c("[0, 5000)","[5000, 10000)","[10000, 15000)","[15000, 20000)","[20000, 25000)","[25000, 35000)", "[35000, 45000)","[45000, 55000)","[55000, 65000)","[65000, 75000)","[75000, 100000)", "75000+",">100000", ">20000","<20000","Refused","Don't know", "NA")){

  ### VERIFY INPUTS ###
  # method
  methodOptions <- c("pop ratio", "mean ratio", "simple")
  method <- tolower(trimws(method))
  if(!(method %in% methodOptions)){
    stop("Enter a valid scoring method.")
  }

  # years
  years <- trimws(years)
  possible_dataset <- get0(paste("fped_", years, sep = ""), envir = asNamespace("heiscore"))
  if(!is.null(possible_dataset)){
    rawDataset <- possible_dataset
  }
  else{
    stop("Enter a valid cycle.")
  }

  # component
  variableList_heiComponents <- list( "kilocalories" = "KCAL",
                                      "total fruit" = "F_TOTAL",
                                      "whole fruits" = "FWHOLEFRT",
                                      'total vegetables' = 'VTOTALLEG',
                                      'greens and beans' = 'VDRKGRLEG',
                                      'whole grains' = 'G_WHOLE',
                                      'total dairy' = 'D_TOTAL',
                                      'total protein' = 'PFALLPROTLEG',
                                      'seafood and plant proteins' = 'PFSEAPLANTLEG',
                                      'fatty acids' = 'TFACIDS',
                                      'refined grains' = 'G_REFINED',
                                      'sodium' = 'TSODI',
                                      'added sugars' = 'ADD_SUGARS',
                                      'saturated fat' = 'TSFAT'
  )
  heiComponent <- tolower(trimws(component))
  if(!(heiComponent %in% c(names(variableList_heiComponents)[-c(1)], "total score"))){
    stop("Enter a valid HEI Component.")
  }

  # demographic option
  demoOptions = c("sex", "race", "age", "income")
  demographicGroup <- demo
  if(is.null(demographicGroup)){
    if(method!="simple"){
      stop("Enter a valid demographic option.")
    }
  }
  else{
    demographicGroup <- tolower(trimws(demographicGroup))
    if(method == "simple"){
      stop("Simple scoring requires NULL as the demographic option.")
    }
    else if(!(demographicGroup %in% demoOptions)){
      stop("Enter a valid demographic option.")
    }
  }

  demo_list <- list( "sex" = "SEX",
                     "race" = "RACE_ETH",
                     "age" = "ageBracket",
                     "income" = "FAMINC")

  # sex
  sexOptions <- c("Female", "Male")
  sex <- stringr::str_to_title(trimws(sex))
  if(!(all(sex %in% sexOptions))){
    stop("Enter a valid sex subset option.")
  }

  # race ethnicity
  raceEthOptions <- c("Asian", "White", "Black", "Other", "Mexican American", "Other Hispanic")
  raceEthnicity <-  stringr::str_to_title(trimws(race))
  if(!(all(raceEthnicity %in% raceEthOptions))){
    stop("Enter a valid race/ethnicity subset option.")
  }

  # age
  if(!is.vector(age) | !length(age) == 2){
    stop("Enter a valid age subset option.")
  }
  else if(any(age < 1)){
    stop("Age subset option must be greater than or equal to 1.")
  }
  else if(age[1] < 2 & age[2] >=2){
    stop("Age subset cannot include both toddlers (< 2yrs) and non-toddlers (> 2yrs)")
  }
  else if(age[1] > age[2]){
    stop("Age minimum must be less than or equal to age maximum.")
  }

  # family income
  famIncOptions <- c("[0, 5000)","[5000, 10000)","[10000, 15000)","[15000, 20000)","[20000, 25000)","[25000, 35000)", "[35000, 45000)","[45000, 55000)","[55000, 65000)","[65000, 75000)", "75000+","[75000, 100000)", ">100000", ">20000","<20000","Refused","Don't know", "NA")
  familyIncome <- trimws(income)
  if(!(all(familyIncome %in% famIncOptions))){
    stop("Enter a valid family income subset option.")
  }

  scoringData <- cleanDataset(rawDataset) %>%
    demoFilter(., sex, raceEthnicity, age, familyIncome)

  ### Scoring ###
  scoringVariable<- unlist(variableList_heiComponents[heiComponent])

  if(heiComponent == "total score" | heiComponent == "fatty acids"){
    scoringData <- scoringData %>%
      dplyr::mutate(TOT_TFACIDS = (DR1_MONOPOLY + DR2_MONOPOLY) / (DR1TSFAT + DR2TSFAT)) %>%
      dplyr::select(-c(DR1_TFACIDS, DR2_TFACIDS))
  }

  if(method == "simple"){

    # simple scoring, total score
    if(heiComponent == "total score"){
      finalSimpleScores <- scoringData %>%
        dplyr::select(SEQN, WTDR2D, SEX, AGE, RACE_ETH, FAMINC) %>%
        tibble::add_column(score = 0)

      for(variables in names(variableList_heiComponents)[-c(1)]){

        scoringVariable<- unlist(variableList_heiComponents[variables])
        variableSimpleScores <- simpleScore(scoringData, scoringVariable, age[1])

        finalSimpleScores <- finalSimpleScores %>%
          dplyr::left_join(., variableSimpleScores, by = c("SEQN", "WTDR2D", "SEX", "AGE", "RACE_ETH", "FAMINC")) %>%
          dplyr::select(SEQN, WTDR2D, SEX, AGE, RACE_ETH, FAMINC, dplyr::contains("score"))
      }

      finalSimpleScores <- finalSimpleScores[-c(7)]
      colnames(finalSimpleScores)[-c(1:6)] <- names(variableList_heiComponents[-c(1)])

      finalSimpleScores <- finalSimpleScores %>%
        dplyr::mutate(score = rowSums(.[setdiff(names(.),c("SEQN", "WTDR2D", "SEX", "AGE", "RACE_ETH", "FAMINC"))])) %>%
        dplyr::mutate(dplyr::across(c(SEX, AGE, RACE_ETH, FAMINC), as.factor)) %>%
        tidyr::drop_na(score)
      colnames(finalSimpleScores)[-c(1:6, 20)] <- unname(unlist(variableList_heiComponents))[-c(1)]
      return(finalSimpleScores)

    }
    # simple scoring, single variables
    else{
      finalScoringData <- scoringData %>%
        dplyr::select(SEQN, dplyr::contains(scoringVariable), dplyr::contains("KCAL"), WTDR2D, SEX, AGE, RACE_ETH, FAMINC)

      finalSimpleScores <- simpleScore(finalScoringData, scoringVariable, age[1]) %>%
        dplyr::select(SEQN, WTDR2D, SEX, AGE, RACE_ETH, FAMINC, score) %>%
        dplyr::mutate(dplyr::across(c(SEX, AGE, RACE_ETH, FAMINC), as.factor)) %>%
        tidyr::drop_na(score)
      return(finalSimpleScores)
    }

  }
  else{
    # choose mean or pop ratio as function to use
    if(method == "mean ratio"){
      scoringFunction <- meanRatioScore
    }
    else{
      scoringFunction <- popRatioScore
    }
    # mean/pop ratio, total score
    if(heiComponent == "total score"){
      demographicGroup <- demo_list[[demographicGroup]]

      scoresTable <- scoringData %>%
        dplyr::select(demographicGroup) %>%
        unique()
      demoVar <- rlang::sym(demographicGroup)

      for(variables in variableList_heiComponents[-c(1)]){
        scoringDataByVariable <- scoringData %>%
          dplyr::select(dplyr::contains(variables), dplyr::contains("KCAL"), WTDR2D, demographicGroup) %>%
          scoringFunction(., variables, demoVar, age[1])

        scoresTable <- scoresTable %>%
          dplyr::right_join(., scoringDataByVariable, by = colnames(scoresTable)[1]) %>%
          dplyr::select(demoVar, dplyr::contains("score"))
        colnames(scoresTable)[length(colnames(scoresTable))] <- paste("score", variables, sep = "_")
      }
      scoresTable <- scoresTable %>%
        dplyr::mutate(score = rowSums(dplyr::across(dplyr::where(is.numeric)))) %>%
        tidyr::drop_na(score)

      # set order of ageBracket variable
      if(demographicGroup == "ageBracket"){
        scoresTable$ageBracket <- factor(scoresTable$ageBracket, levels = c("Toddler (12 - 23 mo.)", "[2,10)", "[10,20)", "[20,30)", "[30,40)", "[40,50)", "[50,60)", "[60,70)", "[70,80)", "80+" ))
        scoresTable <- scoresTable %>% dplyr::arrange(ageBracket)
      }

      # set order of income variable
      else if(demographicGroup == "FAMINC"){
        scoresTable$FAMINC <- factor(scoresTable$FAMINC, levels = c("[0, 5000)","[5000, 10000)","[10000, 15000)","[15000, 20000)","[20000, 25000)","[25000, 35000)", "[35000, 45000)","[45000, 55000)","[55000, 65000)","[65000, 75000)", "75000+" ,"[75000, 100000)", ">100000", "<20000", ">20000","Refused","Don't know", "NA"))
        scoresTable <- scoresTable %>% dplyr::arrange(FAMINC)
      }

      else if(demographicGroup == "SEX"){
        scoresTable$SEX = as.factor(scoresTable$SEX)
      }

      else if(demographicGroup == "RACE_ETH"){
        scoresTable$RACE_ETH = as.factor(scoresTable$RACE_ETH)
      }

      return(scoresTable)

      # mean/pop ratio, individual component score
    }
    else{demographicGroup <- unlist(demo_list[demographicGroup])
    finalScoringData <- scoringData %>%
      dplyr::select(dplyr::contains(scoringVariable), dplyr::contains("KCAL"), WTDR2D, dplyr::contains(demographicGroup))

    demoVar <- colnames(finalScoringData)[length(colnames(finalScoringData))]
    demoVar <- rlang::sym(demoVar)
    scoresTable <- scoringFunction(finalScoringData, scoringVariable, demoVar, age[1]) %>%
      tidyr::drop_na(score)

    # set order of ageBracket variable
    if(demographicGroup == "ageBracket"){
      scoresTable$ageBracket <- factor(scoresTable$ageBracket, levels = c("Toddler (12 - 23 mo.)", "[2,10)", "[10,20)", "[20,30)", "[30,40)", "[40,50)", "[50,60)", "[60,70)", "[70,80)", "80+" ))
      scoresTable <- scoresTable %>% dplyr::arrange(ageBracket)
    }

    # set order of demographicGroup variable
    else if(demographicGroup == "FAMINC"){
      scoresTable$FAMINC <- factor(scoresTable$FAMINC, levels = c("[0, 5000)","[5000, 10000)","[10000, 15000)","[15000, 20000)","[20000, 25000)","[25000, 35000)", "[35000, 45000)","[45000, 55000)","[55000, 65000)","[65000, 75000)", "75000+" ,"[75000, 100000)", ">100000", "<20000", ">20000","Refused","Don't know", "NA"))
      scoresTable <- scoresTable %>% dplyr::arrange(FAMINC)
    }

    else if(demographicGroup == "SEX"){
      scoresTable$SEX = as.factor(scoresTable$SEX)
    }

    else if(demographicGroup == "RACE_ETH"){
      scoresTable$RACE_ETH = as.factor(scoresTable$RACE_ETH)
    }
    return(scoresTable)
    }
  }
}
