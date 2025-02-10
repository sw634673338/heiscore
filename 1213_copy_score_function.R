# not use heiscore canned package
# just copy and revise their function

library(tidyr)
suppressPackageStartupMessages(library(dplyr))
library(stringr)
library(shiny)
library(shinythemes)
library(ggplot2)
library(tibble)
library(ggpubr)
library(grDevices)
library(RColorBrewer)
library(rlang)
library(stats)
library(graphics)
library(fmsb)
library(dplyr)
library(haven)
library(fs) # path


#setwd("D:\\OneDrive\\work\\NHANES_FPED")
#setwd("C:\\work\\OneDrive - purdue.edu\\work\\RA\\hei_figure\\output")
# Retrieve NHANES dietary data converted to Food Patterns

#demographic vars
dir_demo <- path("C:\\work\\OneDrive - purdue.edu\\data\\nhanes\\demo")
dir_security <- path("C:\\work\\OneDrive - purdue.edu\\data\\nhanes\\food_security")
#dir_demo <- path("D:\\OneDrive\\data\\nhanes\\food_security\\")
dir_output <- path("C:\\work\\OneDrive - purdue.edu\\work\\RA\\hei_figure\\output")

# load packages

selectDataset <- function(years) {
  years <- trimws(years)
  possible_dataset <- get0(paste("fped_", years, sep = ""), envir = asNamespace("heiscore"))

  return(possible_dataset)
}

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



# 1 build index for a component
component_index <- function(component_selected, year){

  # drop lines startting with DR2
  #df <- df_raw %>%
    #select(-starts_with("DR2"))

    df_component <- score(method = "Simple",  years = year,  component = component_selected) # they rename the arguments
    # rename column
    new_name <- paste0("HEI_", component_selected)
    df_component <- df_component %>%
#    rename(new_name = score)
    rename(!!new_name := score)

    # Keeping only selected columns
    df_component <- df_component %>%
    select(SEQN, !!new_name)
    return(df_component)
}


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
  if(years == "1720"){
    demoOptions = c("sex", "race", "age")
  }
  else{
    demoOptions = c("sex", "race", "age", "income")
  }

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



# they also rename it into lower case
#component_list <- c("Total Score","Total Fruit","Whole Fruits","Total Vegetables",
#    "Greens and Beans","Whole Grains","Total Dairy", "Total Protein",
#    "Seafood and Plant Proteins", "Fatty Acids", "Refined Grains",
#    "Sodium", "Added Sugars", "Saturated Fat")
component_list <- c("total score","total fruit","whole fruits","total vegetables",
    "greens and beans","whole grains","total dairy", "total protein",
    "seafood and plant proteins", "fatty acids", "refined grains",
    "sodium", "added sugars", "saturated fat")
#YEARS <- c("0506", "0708", "0910", "1112", "1314", "1516", "1718")

YEARS <- c("1718")

#demo <- read_xpt("D:\\OneDrive\\data\\nhanes\\1718\\DEMO_J.XPT")
#test2 <- read_xpt("D:\\OneDrive\\data\\nhanes\\2017-2018\\DR2IFF_J.XPT")



# RUN
for (year in YEARS){
    # Load NHANCE datasets
    # we can pick DAY1 or DAY2 or BOTH
    df_raw = selectDataset(year = year)
    df <- df_raw %>%
    select(SEQN)
    # build index and merge the data
    for (component_selected in component_list) {
        df_one = component_index(component_selected, year)
        df <- left_join(df, df_one, by = "SEQN")
    }
    #merge demographic variables
    file_name <- dir_demo / paste0("DEMO_",
    year, ".XPT")

    demo <- read_xpt(file_name)
    df <- left_join(df, demo, by = "SEQN")

    #merge food security variables
    file_name <- dir_security / paste0("FSQ_",
    year, ".XPT")
    demo <- read_xpt(file_name)
    df <- left_join(df, demo, by = "SEQN")

    # Modify variable names: make them lowercase and remove spaces
    names(df) <- names(df) %>%
    tolower() %>%
    str_replace_all(" ", "_")

    # write csv file
    #file_name <- paste0(year, "nhanes_hei.csv")
    #write.csv(df, file_name, row.names = FALSE)



    # write dta
    #stata_name <- dir_output / paste0(year, "nhanes_hei.dta")
    #write_dta(df, stata_name)#, row.names = FALSE)
}
#test = selectDataset("1718")
#column_names <- names(test)



#df_defaultHEI <- df #it's a deep copy
############ WAIT FOR IT ####################
# RUN the deep copy, then drop "DR2" calcuate

# Step 3: attatch datasets from different years
# wait for it
#df['a'] <- 1

#' Calculate Healthy Eating Index (HEI) scores from NHANES data

#' This function calculates HEI component or total scores using the inputted scoring method. The user can subset the data to only include subjects in specific demographic groups
