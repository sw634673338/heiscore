#' Plot Healthy Eating Index (HEI) scores from NHANES data
#'
#' This function plots HEI component or total scores using the inputted scoring method and graph type. The user can subset the data to only include subjects in specific demographic groups
#' @param graph The desired graph type. Choose "Histogram" when scoringMethod = "Simple". Choose "Bar" when scoringMethod = "Pop Ratio" or "Mean Ratio" and heiComponent is not "Total Score". Choose "Bar" or "Radar" when scoringMethod = "Pop Ratio" or "Mean Ratio" and heiComponent = "Total Score"
#' @param scoringMethod The HEI scoring method to use. Choose from "Simple," "Pop Ratio," or "Mean Ratio."
#' @param years The NHANES cycle to select, choose from: "0506," "0708," "0910," "1112," "1314," "1516," or "1718. etc.
#' @param heiComponent The HEI component to score and plot. Options include "Total Score", "Total Fruit", "Whole Fruits", "Total Vegetables", "Greens and Beans", "Whole Grains", "Total Dairy", "Total Protein", "Seafood and Plant Proteins", "Fatty Acids", "Refined Grains", "Sodium", "Added Sugars", and "Saturated Fat".
#' @param demographicGroup The demographic grouping by which the data should be scored. If scoringMethod = "Simple", choose NULL as the demographicGroup. Otherwise, choose from "Sex", "Race/Ethnicity", "Age", or "Family Income".
#' @param sex a vector of the sexes in desired subpopulation. Provide a vector with "Female", "Male", or both.
#' @param raceEthnicity a vector of races/ethnicities in desired subpopulation. Provide a vector including any combination of the following: "Asian", "White", "Black", "Other", "Mexican American", "Other Hispanic",
#' @param age a vector in the form c(min, max) with two numbers specifying the desired age range to analyze. Both numbers should either be between 0 and 1 or 2 and above.
#' @param familyIncome  a vector of family income brackets in the desired subpopulation. Provide a vector including any combination of the following: "[0, 5000)","[5000, 10000)","[10000, 15000)","[15000, 20000)","[20000, 25000)","[25000, 35000)", "[35000, 45000)","[45000, 55000)","[55000, 65000)","[65000, 75000)","[75000, 100000)", "75000+",">100000", ">20000","<20000","Refused","Don't know", "NA"
#'
#' @return A base R plot or a ggplot object with the specified plot
#'
#' @examples
#' library(ggplot2)
#' library(grDevices)
#' library(RColorBrewer)
#' library(rlang)
#' library(magrittr)
#' library(dplyr)
#' library(tibble)
#' library(rlang)
#' library(stats)
#' library(tidyr)
#'
#' # Plot the Total Dairy component score from the 2005-06 NHANES data using the
#' # "Simple" method.
#'
#' dairy_plot <- plotScore(graph = "Histogram",
#'                         scoringMethod = "Simple",
#'                         years = "0506",
#'                         heiComponent = "Total Dairy")
#' dairy_plot
#'
#' # Create a radar plot to display the total HEI score by race/ethnicity using
#' # the mean ratio method for subjects that are male, more than 50 years old,
#' # with a family income in the range [65000, 75000) for the 2015-16 NHANES
#' # cycle.
#' radar_example_plot <- plotScore(graph = "Radar",
#'                     scoringMethod = "Mean Ratio",
#'                     years = "1516",
#'                     heiComponent = "Total Score",
#'                     demographicGroup = "Race/Ethnicity",
#'                     sex = "Male",
#'                     age = c(50, 150),
#'                     familyIncome = "[65000, 75000)")
#' radar_example_plot
#'
#' @export
plotScore <- function(graph = NULL, scoringMethod, years, heiComponent, demographicGroup = NULL, sex = c("Female", "Male"), raceEthnicity = c("Asian", "White", "Black", "Other", "Mexican American", "Other Hispanic"), age = c(2, 100), familyIncome = c("[0, 5000)","[5000, 10000)","[10000, 15000)","[15000, 20000)","[20000, 25000)","[25000, 35000)", "[35000, 45000)","[45000, 55000)","[55000, 65000)","[65000, 75000)", "75000+","[75000, 100000)", ">100000", ">20000","<20000","Refused","Don't know", "NA")){
  ### VERIFY INPUTS ###
  # graph input
  graphOptions <- c("Radar", "Bar", "Histogram")

  if(!(graph %in% graphOptions)){
    stop("Enter a valid plotting option.")
  }
  else if(scoringMethod == "Simple" & graph != "Histogram"){
    stop("Simple scoring requires 'Histogram' as the graph option.")
  }
  else if((scoringMethod == "Mean Ratio" | scoringMethod == "Pop Ratio") & !(graph %in% graphOptions[1:2])){
    stop("Mean and Pop Ratio scoring require 'Bar' or 'Radar' as the graph option.")
  }
  else if(heiComponent != "Total Score" & graph == "Radar"){
    stop("The radar plot option is only allowed when heiComponent is 'Total Score'.")
  }

  # scoringMethod Input
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
                                      "Total Vegetables" = "VTOTALLEG",
                                      "Greens \u000Aand Beans" = "VDRKGRLEG",
                                      "Whole Grains" = "G_WHOLE",
                                      "Total Dairy" = "D_TOTAL",
                                      "Total Protein" = "PFALLPROTLEG",
                                      "Seafood/Plant \u000AProteins" = "PFSEAPLANTLEG",
                                      "Fatty Acids" = "TFACIDS",
                                      "Refined Grains" = "G_REFINED",
                                      "Sodium" = "TSODI",
                                      "Added Sugars" = "ADD_SUGARS",
                                      "Saturated Fat" = "TSFAT")

  if(!(heiComponent %in% c(names(variableList_heiComponents)[-c(1)], "Total Score"))){
    stop("Enter a valid HEI Component.")
  }

  # demographic option
  demo_list <- list( "Sex" = "SEX",
                     "Race/Ethnicity" = "RACE_ETH",
                     "Age" = "ageBracket",
                     "Family Income" = "FAMINC")

  if(is.null(demographicGroup)){

    if(scoringMethod!="Simple"){
      stop("Enter a valid demographic option.")
    }
  }
  else if(scoringMethod == "Simple"){
    stop("Simple scoring requires NULL as the demographic option.")
  }
  else if(!(demographicGroup %in% names(demo_list))){
    stop("Enter a valid demographic option.")
  }

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
  else if(any(age < 0)){
    stop("Age subset option must be greater than 0.")
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

  ### SCORE OUTPUT ###
  score_output <- score(scoringMethod, years, heiComponent, demographicGroup, sex, raceEthnicity, age, familyIncome)

  ### PLOT SCORE OUTPUT ###
  # set correct max axis value
  if(age[1] < 2){
    scoringStandards <- HEI_scoring_standards_toddlers
  }
  else{
    scoringStandards <- HEI_scoring_standards
  }

  if(heiComponent == "Total Score"){
    axisMax = 100
  }
  else{
    axisMax = scoringStandards$max_points[scoringStandards$component == unlist(variableList_heiComponents[heiComponent])]
  }

  # plot simple score
  if(scoringMethod== "Simple"){
    score_output <- score_output %>%
      tidyr::drop_na()
    result_plot <- ggplot2::ggplot(score_output) +
      ggplot2::geom_histogram(ggplot2::aes(x=score, y=ggplot2::after_stat(count/sum(count)), weight = WTDR2D), boundary=0, bins = 20) +
      ggplot2::ylab("Proportion") +
      ggplot2::xlab(heiComponent) +
      ggplot2::theme_classic() +
      ggplot2::theme(axis.text=ggplot2::element_text(color="black", size=11),
                     axis.title = ggplot2::element_text(face="bold", size=15)) +
      ggplot2::xlim(0, axisMax) +
      if(heiComponent != "Total Score"){
        ggplot2::ggtitle(paste0("Weighted Histogram of ", heiComponent, " Scores"))
      } else{
        ggplot2::ggtitle(paste0("Weighted Histogram of Total Scores"))
      }

  } # end plotting Simple Score

  else{
    # Set colors for radar and bar graphs
    if((demographicGroup)=="Family Income"){
      radarColors = grDevices::colorRampPalette(c("cadetblue3", "blue4"))
      radarColors2 = grDevices::colorRampPalette(c("coral", "coral4"))
      radarColors = c(radarColors(12), radarColors2(5))
    } else if(demographicGroup=="Race/Ethnicity" | demographicGroup=="Sex"){
      radarColors = RColorBrewer::brewer.pal(n=(nrow(score_output)+1), name="Dark2")
    } else{
      radarColors = grDevices::colorRampPalette(c("cadetblue3", "blue4"))
      radarColors = radarColors(nrow(score_output))
    }

    if(graph == "Bar"){
      demoVar <- rlang::sym(colnames(score_output)[1])
      score <- rlang::sym("score")

      result_plot <- ggplot2::ggplot(score_output) +
        ggplot2::geom_bar(ggplot2::aes(x=!!demoVar, y=!!score), stat="identity") +
        ggplot2::labs(title = paste(scoringMethod, "Scores by", demographicGroup), x = demographicGroup, y = "Score") +
        ggplot2::theme_classic() +
        ggplot2::scale_fill_manual(values=radarColors) +
        ggplot2::ylim(0, axisMax)  +
        ggplot2::guides(fill=ggplot2::guide_legend(title=demographicGroup)) +
        if(demographicGroup == "Family Income"){
          ggplot2::theme(axis.text=ggplot2::element_text(color="black", size=11),
                         axis.title = ggplot2::element_text(face="bold", size=15),
                         axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust=1))
        } else{
          ggplot2::theme(axis.text=ggplot2::element_text(color="black", size=11),
                         axis.title = ggplot2::element_text(face="bold", size=15))
        }
      return(result_plot)

    } # end bar

    else if(graph == "Radar"){
      max_points <- as.vector(scoringStandards$max_points)
      min_points <- c(rep(0, 13))
      final_radar_data <- rbind(max_points, min_points, score_output[-c(1, 15)]) %>%
        as.data.frame()
      rownames(final_radar_data) <- c("Max", "Min", as.vector(unlist(score_output[,1])))

      colnames(final_radar_data) <- names(variableList_heiComponents[-c(1)])
      demoVar <- rlang::sym(colnames(final_radar_data)[1])


      fmsb::radarchartcirc(final_radar_data, vlcex = .7, plwd = 3, plty = 1, pcol = radarColors, axistype = 2, axislabcol = "red", palcex = .7, cglcol = "gray", cglty = 3)
      graphics::legend(
        x = "left", legend = rownames(final_radar_data[-c(1,2),]), horiz = F,
        bty = "n", pch = 20 , col = radarColors,
        text.col = "black", cex = 1, pt.cex = 1.5
      )

    } # end radar
  }
}
