#' Calculate Healthy Eating Index (HEI) scores from NHANES data
#'
#' This function calculates HEI component or total scores using the inputted scoring method. The user can subset the data to only include subjects in specific demographic groups

#' @param scoringMethod The HEI scoring method to use. Choose from "Simple," "Pop Ratio," or "Mean Ratio."
#' @param years The NHANES cycle to select, choose from: "0506," "0708," "0910," "1112," "1314," "1516," or "1718. etc.
#' @param heiComponent The HEI component to score and plot. Options include "Total Score", "Total Fruit", "Whole Fruits", "Total Vegetables", "Greens and Beans", "Whole Grains", "Total Dairy", "Total Protein", "Seafood and Plant Proteins", "Fatty Acids", "Refined Grains", "Sodium", "Added Sugars", and "Saturated Fat".
#' @param demographicGroup The demographic grouping by which the data should be scored. If scoringMethod = "Simple", choose NULL as the demographicGroup. Otherwise, choose from "Sex", "Race/Ethnicity", "Age", or "Family Income".
#' @param sex a vector of the sexes in desired subpopulation. Provide a vector with "Female", "Male", or both.
#' @param raceEthnicity a vector of races/ethnicities in desired subpopulation. Provide a vector including any combination of the following: "Asian", "White", "Black", "Other", "Mexican American", "Other Hispanic",
#' @param age a vector in the form c(min, max) with two numbers specifying the desired age range to analyze. Both numbers should either be ones (to represent the toddler age group including ages 12-23 months) or 2 and above.
#' @param familyIncome  a vector of family income brackets in the desired subpopulation. Provide a vector including any combination of the following: "[0, 5000)","[5000, 10000)","[10000, 15000)","[15000, 20000)","[20000, 25000)","[25000, 35000)", "[35000, 45000)","[45000, 55000)","[55000, 65000)","[65000, 75000)","[75000, 100000)", "75000+",">100000", ">20000","<20000","Refused","Don't know", "NA"
#'
#' @return A tibble with HEI scores for the selected component for each individual (when scoringMethod = "Simple") or by demographic grouping specified in demographicGroup.
#'
#' @examples
#' library(magrittr)
#' library(dplyr)
#' library(tibble)
#' library(rlang)
#' library(stats)
#' library(tidyr)
#'
#' # Calculate the Total Fruit component score from the 2017-18 NHANES data
#' # using the 'Simple' method.
#' score(scoringMethod = "Simple",
#'                     years = "1718",
#'                     heiComponent = "Total Fruit",
#'                     demographicGroup = NULL)
#'
#' # Calculate the total HEI score by sex using the population ratio method for
#' # White and Black individuals between ages 5 and 10 with a family income
#' # between 5000abd 15000 in the 2011-12 NHANES cycle.
#' score(scoringMethod = "Pop Ratio",
#'                     years = "1112",
#'                     heiComponent = "Total Score",
#'                     demographicGroup = "Sex",
#'                     raceEthnicity = c("White", "Black"),
#'                     age = c(5,10),
#'                     familyIncome = c("[5000, 10000)", "[10000, 15000)"))
#'
#' @import magrittr
#'
#' @export

score <- function(scoringMethod, years, heiComponent, demographicGroup = NULL, sex = c("Female", "Male"), raceEthnicity = c("Asian", "White", "Black", "Other", "Mexican American", "Other Hispanic"), age = c(2, 100), familyIncome = c("[0, 5000)","[5000, 10000)","[10000, 15000)","[15000, 20000)","[20000, 25000)","[25000, 35000)", "[35000, 45000)","[45000, 55000)","[55000, 65000)","[65000, 75000)","[75000, 100000)", "75000+",">100000", ">20000","<20000","Refused","Don't know", "NA")){

  ### VERIFY INPUTS ###
  # scoringMethod
  methodOptions <- c("Pop Ratio", "Mean Ratio", "Simple")
  if(!(scoringMethod %in% methodOptions)){
    stop("Enter a valid scoring method.")
  }

  # years
  possible_dataset <- get0(paste("fped_", years, sep = ""), envir = asNamespace("heiscore"))
  if(!is.null(possible_dataset)){
    rawDataset <- possible_dataset
  }
  else{
    stop("Enter a valid cycle.")
  }

  # heiComponent
  variableList_heiComponents <- list( "Kilocalories" = "KCAL",
                                      "Total Fruit" = "F_TOTAL",
                                      "Whole Fruits" = "FWHOLEFRT",
                                      'Total Vegetables' = 'VTOTALLEG',
                                      'Greens and Beans' = 'VDRKGRLEG',
                                      'Whole Grains' = 'G_WHOLE',
                                      'Total Dairy' = 'D_TOTAL',
                                      'Total Protein' = 'PFALLPROTLEG',
                                      'Seafood and Plant Proteins' = 'PFSEAPLANTLEG',
                                      'Fatty Acids' = 'TFACIDS',
                                      'Refined Grains' = 'G_REFINED',
                                      'Sodium' = 'TSODI',
                                      'Added Sugars' = 'ADD_SUGARS',
                                      'Saturated Fat' = 'TSFAT'
  )

  if(!(heiComponent %in% c(names(variableList_heiComponents)[-c(1)], "Total Score"))){
    stop("Enter a valid HEI Component.")
  }

  # demographic option
  demoOptions = c("Sex", "Race/Ethnicity", "Age", "Family Income")
  if(is.null(demographicGroup)){
    if(scoringMethod!="Simple"){
      stop("Enter a valid demographic option.")
    }
  }
  else if(scoringMethod == "Simple"){
    stop("Simple scoring requires NULL as the demographic option.")
  }
  else if(!(demographicGroup %in% demoOptions)){
    stop("Enter a valid demographic option.")
  }

  demo_list <- list( "Sex" = "SEX",
                     "Race/Ethnicity" = "RACE_ETH",
                     "Age" = "ageBracket",
                     "Family Income" = "FAMINC")

  # sex
  sexOptions <- c("Female", "Male")
  if(!(all(sex %in% sexOptions))){
    stop("Enter a valid sex subset option.")
  }

  # race ethnicity
  raceEthOptions <- c("Asian", "White", "Black", "Other", "Mexican American", "Other Hispanic")
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
  if(!(all(familyIncome %in% famIncOptions))){
    stop("Enter a valid family income subset option.")
  }

  scoringData <- cleanDataset(rawDataset) %>%
    demoFilter(., sex, raceEthnicity, age, familyIncome)

  ### Scoring ###
  scoringVariable<- unlist(variableList_heiComponents[heiComponent])

  if(scoringMethod == "Simple"){

    # simple scoring, total score
    if(heiComponent == "Total Score"){
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
        dplyr::mutate(score = rowSums(.[setdiff(names(.),c("SEQN", "WTDR2D", "SEX", "AGE", "RACE_ETH", "FAMINC"))]))
      colnames(finalSimpleScores)[-c(1:6, 20)] <- unname(unlist(variableList_heiComponents))[-c(1)]
      return(finalSimpleScores)

    }
    # simple scoring, single variables
    else{
      finalScoringData <- scoringData %>%
        dplyr::select(SEQN, dplyr::contains(scoringVariable), dplyr::contains("KCAL"), WTDR2D, SEX, AGE, RACE_ETH, FAMINC)

      finalSimpleScores <- simpleScore(finalScoringData, scoringVariable, age[1]) %>%
        dplyr::select(SEQN, WTDR2D, SEX, AGE, RACE_ETH, FAMINC, score)
      return(finalSimpleScores)
    }

  }
  else{
    # choose mean or pop ratio as function to use
    if(scoringMethod == "Mean Ratio"){
      scoringFunction <- meanRatioScore
    }
    else{
      scoringFunction <- popRatioScore
    }
    # mean/pop ratio, total score
    if(heiComponent == "Total Score"){
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
        dplyr::mutate(score = rowSums(dplyr::across(dplyr::where(is.numeric))))

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

      return(scoresTable)

      # mean/pop ratio, individual component score
    }
    else{demographicGroup <- unlist(demo_list[demographicGroup])
    finalScoringData <- scoringData %>%
      dplyr::select(dplyr::contains(scoringVariable), dplyr::contains("KCAL"), WTDR2D, dplyr::contains(demographicGroup))

    demoVar <- colnames(finalScoringData)[length(colnames(finalScoringData))]
    demoVar <- rlang::sym(demoVar)
    scoresTable <- scoringFunction(finalScoringData, scoringVariable, demoVar, age[1])

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
    return(scoresTable)
    }
  }
}
