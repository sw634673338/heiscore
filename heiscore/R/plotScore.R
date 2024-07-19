#' Plot Healthy Eating Index (HEI) scores from NHANES data
#'
#' This function plots HEI component or total scores using the inputted scoring method and graph type. The user can subset the data to only include subjects in specific demographic groups
#' @param graph A single character string with the desired graph type. Choose "histogram" when method = "simple". Choose "bar" when method = "pop ratio" or "mean ratio" and component is not "total score". Choose "bar" or "radar" when method = "pop ratio" or "mean ratio" and component = "total score"
#' @param method A single character string with the HEI scoring method to use. Choose from "simple", "pop ratio", or "mean ratio".
#' @param years A single character string representing the NHANES cycle to select, choose from: "0506", "0708", "0910", "1112", "1314", "1516", or "1718".
#' @param component A single character string with the HEI component to score and plot. Options include "total score", "total fruit", "whole fruits", "total vegetables", "greens and beans", "whole grains", "total dairy", "total protein", "seafood and plant proteins", "fatty acids", "refined grains", "sodium", "added sugars", and "saturated fat".
#' @param demo A single character string with the demographic grouping by which the data should be scored or NULL. If method = "simple", choose NULL as the demo. Otherwise, choose from "sex", "race", "age", or "income".
#' @param sex A vector of the sexes in the desired subpopulation. Provide a vector with the character strings "Female", "Male", or both.
#' @param race A vector of races/ethnicities in the desired subpopulation. Provide a vector including any combination of the following character strings: "Asian", "White", "Black", "Other", "Mexican American", and "Other Hispanic".
#' @param age A vector in the form c(min, max) with two integers specifying the desired age range to analyze. Both integers should either be ones (to represent the toddler age group including ages 12-23 months) or 2 and above.
#' @param income  A vector of family income brackets in the desired subpopulation. Provide a vector including any combination of the following character strings: "[0, 5000)","[5000, 10000)","[10000, 15000)","[15000, 20000)","[20000, 25000)","[25000, 35000)", "[35000, 45000)","[45000, 55000)","[55000, 65000)","[65000, 75000)","[75000, 100000)", "75000+",">100000", ">20000","<20000","Refused","Don't know", "NA".
#'
#' @return A base R plot or a ggplot object with the specified plot
#'
#' @examples
#' # Plot the Total Dairy component score from the 2005-06 NHANES data using the
#' # "simple" method.
#'
#' dairy_plot <- plotScore(graph = "histogram",
#'                         method = "simple",
#'                         years = "0506",
#'                         component = "total dairy")
#' dairy_plot
#'
#' # Create a radar plot to display the total HEI score by race/ethnicity using
#' # the mean ratio method for subjects that are male, more than 50 years old,
#' # with a family income in the range [65000, 75000) for the 2015-16 NHANES
#' # cycle.
#' radar_example_plot <- plotScore(graph = "radar",
#'                     method = "mean ratio",
#'                     years = "1516",
#'                     component = "total score",
#'                     demo = "race",
#'                     sex = "Male",
#'                     age = c(50, 150),
#'                     income = "[65000, 75000)")
#' radar_example_plot
#'
#' @export
plotScore <- function(graph = NULL, method, years, component, demo = NULL, sex = c("Female", "Male"), race = c("Asian", "White", "Black", "Other", "Mexican American", "Other Hispanic"), age = c(2, 100), income = c("[0, 5000)","[5000, 10000)","[10000, 15000)","[15000, 20000)","[20000, 25000)","[25000, 35000)", "[35000, 45000)","[45000, 55000)","[55000, 65000)","[65000, 75000)", "75000+","[75000, 100000)", ">100000", ">20000","<20000","Refused","Don't know", "NA")){
  ### VERIFY INPUTS ###
  # method Input
  methodOptions <- c("pop ratio", "mean ratio", "simple")
  scoringMethod <- tolower(trimws(method))
  if(!(scoringMethod %in% methodOptions)){
    stop("Enter a valid scoring method.")
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
                                      'saturated fat' = 'TSFAT')

  heiComponent <- tolower(trimws(component))
  if(!(heiComponent %in% c(names(variableList_heiComponents)[-c(1)], "total score"))){
    stop("Enter a valid HEI Component.")
  }

  # graph input
  graphOptions <- c("radar", "bar", "histogram")
  graph <- tolower(trimws(graph))
  if(!(graph %in% graphOptions)){
    stop("Enter a valid plotting option.")
  }
  else if(scoringMethod == "simple" & graph != "histogram"){
    stop("simple scoring requires 'histogram' as the graph option.")
  }
  else if((scoringMethod == "mean ratio" | scoringMethod == "pop ratio") & !(graph %in% graphOptions[1:2])){
    stop("Mean and pop ratio scoring require 'bar' or 'radar' as the graph option.")
  }
  else if(heiComponent != "total score" & graph == "radar"){
    stop("The radar plot option is only allowed when heiComponent is 'total score'.")
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

  # demographic option
  demo_list <- list( "sex" = "SEX",
                     "race" = "RACE_ETH",
                     "age" = "ageBracket",
                     "income" = "FAMINC")

  demographicGroup <- demo
  if(is.null(demographicGroup)){
    if(scoringMethod!="simple"){
      stop("Enter a valid demographic option.")
    }
  }
  else{
    demographicGroup <- tolower(trimws(demographicGroup))
    if(method == "simple"){
      stop("Simple scoring requires NULL as the demographic option.")
    }
    else if(!(demographicGroup %in% names(demo_list))){
      stop("Enter a valid demographic option.")
    }
  }

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
  familyIncome <- trimws(income)
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

  if(heiComponent == "total score"){
    axisMax = 100
  }
  else{
    axisMax = scoringStandards$max_points[scoringStandards$component == unlist(variableList_heiComponents[heiComponent])]
  }

  # plot simple score
  if(scoringMethod== "simple"){
    score_output <- score_output %>%
      tidyr::drop_na()
    result_plot <- ggplot2::ggplot(score_output) +
      ggplot2::geom_histogram(ggplot2::aes(x=score, y=ggplot2::after_stat(count/sum(count)), weight = WTDR2D), boundary=0, bins = 20) +
      ggplot2::ylab("Proportion") +
      ggplot2::xlab(stringr::str_to_title(heiComponent)) +
      ggplot2::theme_classic() +
      ggplot2::theme(axis.text=ggplot2::element_text(color="black", size=11),
                     axis.title = ggplot2::element_text(face="bold", size=15)) +
      ggplot2::xlim(0, axisMax) +
      if(heiComponent != "total score"){
        ggplot2::ggtitle(paste0("Weighted Histogram of ", stringr::str_to_title(heiComponent), " Scores"))
      } else{
        ggplot2::ggtitle(paste0("Weighted Histogram of Total Scores"))
      }
    return(result_plot)

  } # end plotting simple Score

  else{
    # Set colors for radar and bar graphs
    if((demographicGroup)=="income"){
      radarColors = grDevices::colorRampPalette(c("#FFBE6A","#40B0A6"))
      radarColors2 = grDevices::colorRampPalette(c("#0072B2","#000000"))
      radarColors = c(radarColors(12), radarColors2(5))
    }
    else if (demographicGroup=="race" | demographicGroup=="sex"){
      n = nrow(score_output)+1
      colorblind_pal_set = c("#648FFF", "#FFB000", "#DC267F", "#FE6100", "#785EF0", "#000000")
      radarColors = colorblind_pal_set[1:n]
    }
    else {
        radarColors = viridis::mako(nrow(score_output))
      }

    if(graph == "bar"){
      demoVar <- rlang::sym(colnames(score_output)[1])
      score <- rlang::sym("score")

      result_plot <- ggplot2::ggplot(score_output) +
        ggplot2::geom_bar(ggplot2::aes(x=!!demoVar, y=!!score, fill = !!demoVar), stat="identity") +
        ggplot2::labs(title = paste(stringr::str_to_title(scoringMethod), "Scores by", stringr::str_to_title(demographicGroup)), x = stringr::str_to_title(demographicGroup), y = "Score") +
        ggplot2::theme_classic() +
        ggplot2::scale_fill_manual(values=radarColors) +
        ggplot2::ylim(0, axisMax)  +
        ggplot2::guides(fill=ggplot2::guide_legend(title=stringr::str_to_title(demographicGroup))) +
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

    else if(graph == "radar"){
      max_points <- as.vector(scoringStandards$max_points)
      min_points <- c(rep(0, 13))
      final_radar_data <- rbind(max_points, min_points, score_output[-c(1, 15)]) %>%
        as.data.frame()
      rownames(final_radar_data) <- c("Max", "Min", as.vector(unlist(score_output[,1])))

      colnames(final_radar_data) <- stringr::str_to_title(names(variableList_heiComponents[-c(1)]))
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
