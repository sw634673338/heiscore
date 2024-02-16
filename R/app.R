#' Launch Shiny app to visualize Healthy Eating Index (HEI) scores
#'
#' This function launches a Shiny application that allows users to visualize HEI scores calculated from National Health and Nutrition Examination Survey (NHANES) 24-hour dietary recall data.
#'
#' @return NULL
#'
#' @examples
#' library(tidyr)
#' library(dplyr)
#' library(stringr)
#' library(shiny)
#' library(shinythemes)
#' library(ggplot2)
#' library(tibble)
#' library(ggpubr)
#' library(grDevices)
#' library(RColorBrewer)
#' library(rlang)
#'
#' runShinyApp()
#'
#' @export

runShinyApp <- function(){
  if (interactive()) {

    #### Select/clean data, create ageBracket variable ####
    files <- c("fped_0506", "fped_0708", "fped_0910", "fped_1112", "fped_1314", "fped_1516", "fped_1718")

    for(file in files){
      #read in file
      na_file <- get0(file, envir = asNamespace("heiscore"))

      #mutate ages and drop NA values in weights for 2 day weights
      comp_file <- na_file[na_file$WTDR2D != 0 & na_file$AGE >= 1,] %>%
        tidyr::drop_na(WTDR2D) %>%
        dplyr::mutate(
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

      comp_file$ageBracket <- factor(comp_file$ageBracket, levels = c("Toddler (12 - 23 mo.)", "[2,10)", "[10,20)", "[20,30)", "[30,40)", "[40,50)", "[50,60)", "[60,70)", "[70,80)", "80+" ))

      comp_file$FAMINC[comp_file$FAMINC==''] <- "NA"

      comp_file$FAMINC <- factor(comp_file$FAMINC, levels = c("[0, 5000)","[5000, 10000)","[10000, 15000)","[15000, 20000)","[20000, 25000)","[25000, 35000)", "[35000, 45000)","[45000, 55000)","[55000, 65000)","[65000, 75000)", "75000+", "[75000, 100000)", ">100000", "<20000", ">20000","Refused","Don't know", "NA"))

      #assign unique name for subset to call
      subsetName <- stringr::str_sub(file, -4, -1)
      assign(paste0("hei_components_", subsetName), comp_file)
    }

    #scoring standards files
    hei_standards_2020 <- HEI_scoring_standards
    hei_standards_toddler_2020 <- HEI_scoring_standards_toddlers

    # list of all datasets to reference
    all_datasets <-  list(hei_components_0506, hei_components_0708, hei_components_0910, hei_components_1112, hei_components_1314, hei_components_1516,  hei_components_1718)

    #####UI#####
    ui <- shiny::fluidPage(
      theme = shinythemes::shinytheme("cosmo"),
      shiny::titlePanel('HEI Component Histograms'),
      shiny::tabsetPanel(

        #### RECALLS PANEL ####
        shiny::tabPanel('Recalls',
                        shiny::sidebarLayout(
                          shiny::sidebarPanel(
                            shiny::selectInput("dataset", "Select Dataset", choices = c("2005-06", "2007-08", "2009-10", "2011-12", "2013-14", "2015-16", "2017-18"), selected = "2017-18"),
                            shiny::radioButtons("componentType", "Select Component Type",
                                                choices = list("Dietary Constituents" = 1,
                                                               "Dietary Components" = 2),
                                                selected = 1),
                            shiny::selectInput("variable", "Select Variable",
                                               choices = variableList_heiComponents),
                            shiny::checkboxGroupInput("sex", "Select Sex", choices = NULL),
                            shiny::checkboxGroupInput("race", "Select Race", choices = NULL),
                            shiny::checkboxGroupInput("age", "Select Age Bracket", choices = NULL),
                            shiny::checkboxGroupInput("income", "Select Income Bracket", choices = NULL)
                          ),
                          shiny::mainPanel(shiny::plotOutput("myPlot"),
                                           shiny::textOutput("num_observations"),
                                           shiny::checkboxInput("adjusted_checkbox", 'Adjusted per 1000 calories', value=FALSE),
                                           shiny::checkboxInput("average_checkbox", "Plot Average", value = FALSE),
                                           shiny::radioButtons("x_axis_scaling", "Choose x-axis scale",
                                                               choices = list("Keep X-Axis Constant for Recall Component (Regardless of Year or Demographic Group)" = 1,
                                                                              "Make X-Axis Proportional to Maximum" = 2,
                                                                              "Normal" = 3),
                                                               selected = 1))
                        )
        ),

        #### DEMOGRAPHICS PANEL ####

        shiny::tabPanel('Demographics',
                        #useShinyjs(),
                        shiny::sidebarLayout(
                          shiny::sidebarPanel(
                            shiny::selectInput("demoDataset", "Select Dataset", choices = c("2005-06", "2007-08", "2009-10", "2011-12", "2013-14", "2015-16", "2017-18"), selected = "2017-18"),
                            shiny::selectInput('selectDemo', 'Choose a Demographic',
                                               choices=c("Sex", "Race", "Age", "Income")),
                            shiny::checkboxGroupInput("demoSex", "Select Sex", choices = NULL),
                            shiny::checkboxGroupInput("demoRace", "Select Race/Ethnicity", choices = NULL),
                            shiny::checkboxGroupInput("demoAge", "Select Age", choices = NULL),
                            shiny::checkboxGroupInput("demoIncome", "Select Income", choices = NULL),
                          ),
                          shiny::mainPanel(shiny::plotOutput('demoPlot'),
                                           shiny::textOutput('demoNum_observations'))
                        )
        ),

        #### SCORING PANEL ####

        shiny::tabPanel('Scoring',
                        shiny::sidebarLayout(
                          shiny::sidebarPanel(
                            shiny::selectInput('scoringMethod', 'Choose a Scoring Method',
                                               choices=c('Simple', 'Mean Ratio', 'Population Ratio')),
                            shiny::selectInput("scoringDataset", "Select Dataset", choices = c("2005-06", "2007-08", "2009-10", "2011-12", "2013-14", "2015-16", "2017-18"), selected = "2017-18"),
                            shiny::selectInput('secondDataset', 'Compare with a Second Dataset', choices=c('None', "2005-06", "2007-08", "2009-10", "2011-12", "2013-14", "2015-16", "2017-18"), selected='None'),
                            shiny::selectInput('scoringVariable', 'Select Variable', choices = c("Total Score", variableList_heiComponents[-c(1)])),
                            shiny::selectInput('scoringAgeChoice', 'Select Age Group', choices=c('Population 2 Years and Older', 'Toddlers (12 through 23 Months)')),
                            shiny::selectInput('scoringDemographic', 'Choose a Demographic',
                                               choices= list("Sex" = "SEX",
                                                             "Race" = "RACE_ETH",
                                                             "Age" = "ageBracket",
                                                             "Income" = "FAMINC")),
                            shiny::checkboxGroupInput('scoringSex', 'Select Sex', choices=NULL),
                            shiny::checkboxGroupInput('scoringRace', 'Select Race/Ethnicity', choices=NULL),
                            shiny::checkboxGroupInput('scoringAge', 'Select Age', choices=NULL),
                            shiny::checkboxGroupInput('scoringIncome', 'Select Income', choices=NULL)
                          ),
                          shiny::mainPanel(shiny::plotOutput('scoringPlot'),
                                           shiny::textOutput('scoringNum_observations'),
                                           shiny::selectInput("scoringDisplay", "Select a scoring display option", choices = NULL))
                        )
        ),

        #### VARIABLE INFORMATION PANEL ####
        shiny::tabPanel('Variable Information',
                        shiny::h3("Healthy Eating Index Components"),
                        shiny::p("The HEI is made up of 13 components. 9 of these are adequacy components (foods to eact more of for good health), while the other 4 are moderations components (foods to limit for good health. Below are the 13 components and the foods they include."),
                        shiny::br(),
                        shiny::h4(shiny::strong("Adequacy")),
                        shiny::p(shiny::strong("Total Fruits:"), "All fruit, including 100% fruit juice"),
                        shiny::p(shiny::strong("Whole Fruits:"), "All fruit, excluding 100% fruit juice"),
                        shiny::p(shiny::strong("Total Vegetables:"), "All vegetables, including legumes"),
                        shiny::p(shiny::strong("Greens and Beans:"), "Dark green vegetables and legumes"),
                        shiny::p(shiny::strong("Whole Grains")),
                        shiny::p(shiny::strong("Dairy:"), "Milk and milk products, such as fluid milk, yogurt, and cheese, and fortified soy beverages"),
                        shiny::p(shiny::strong("Total Protein Foods:"), "All protein foods including legumes"),
                        shiny::p(shiny::strong("Seafood and Plant Proteins:"), "Seafood, nuts, seeds, soy products (other than beverages), and legumes"),
                        shiny::p(shiny::strong("Fatty Acids:"), "Ratio of poly- and monounsaturated fatty acids to saturated fatty acids"),
                        shiny::br(),
                        shiny::h4(shiny::strong("Moderation")),
                        shiny::p(shiny::strong("Refined Grains")),
                        shiny::p(shiny::strong("Sodium")),
                        shiny::p(shiny::strong("Added Sugars")),
                        shiny::p(shiny::strong("Saturated Fats")),
                        shiny::br(),
                        shiny::h3("Markov Chain Monte Carlo (MCMC) Method Components"),
                        shiny::p("The MCMC method is used to estimate distributions of usual intake. For this method, each dietary constituent must be treated separately. For example, instead of the Greens and Beans HEI componenent, Dark Green Vegetables and Legumes are counted separately."),
                        shiny::br(),
                        shiny::p("\n*Note that recall data for Kcal and Fatty Acids ratio is not available when data is adjusted per 1000 calories")
        )
      )
    )

    ####SERVER####

    server <- function(input, output, session) {

      #switch for recall dataset selection
      selected_dataset <- shiny::reactive({
        switch(input$dataset,
               "2005-06" = hei_components_0506,
               "2007-08" = hei_components_0708,
               "2009-10" = hei_components_0910,
               "2011-12" = hei_components_1112,
               "2013-14" = hei_components_1314,
               "2015-16" = hei_components_1516,
               "2017-18" = hei_components_1718)
      })

      #switch for scoring dataset selection
      selected_scoringDataset <- shiny::reactive({
        switch(input$scoringDataset,
               "2005-06" = hei_components_0506,
               "2007-08" = hei_components_0708,
               "2009-10" = hei_components_0910,
               "2011-12" = hei_components_1112,
               "2013-14" = hei_components_1314,
               "2015-16" = hei_components_1516,
               "2017-18" = hei_components_1718)
      })

      #switch for second scoring dataset selection
      secondSelected_scoringDataset <- shiny::reactive({
        switch(input$secondDataset,
               "2005-06" = hei_components_0506,
               "2007-08" = hei_components_0708,
               "2009-10" = hei_components_0910,
               "2011-12" = hei_components_1112,
               "2013-14" = hei_components_1314,
               "2015-16" = hei_components_1516,
               "2017-18" = hei_components_1718
        )
      })

      #switch for demo dataset selection
      selected_demoDataset <- shiny::reactive({
        switch(input$demoDataset,
               "2005-06" = hei_components_0506,
               "2007-08" = hei_components_0708,
               "2009-10" = hei_components_0910,
               "2011-12" = hei_components_1112,
               "2013-14" = hei_components_1314,
               "2015-16" = hei_components_1516,
               "2017-18" = hei_components_1718
        )
      })

      #switch for correct scoring standards
      selected_scoringStandards <- shiny::reactive({
        switch(input$scoringAgeChoice,
               'Population 2 Years and Older' = hei_standards_2020,
               'Toddlers (12 through 23 Months)' = hei_standards_toddler_2020
        )
      })

      ###change recall variable choices based on MCMC or HEI component and when adjusted for kcal checkbox is selected (remove kcal and fatty acids)###
      shiny::observeEvent(list(input$adjusted_checkbox, input$componentType), {
        if(input$componentType == 1){
          variableList = variableList_MCMC
          adjChoices = 1
        }
        else{
          variableList = variableList_heiComponents
          adjChoices = c(1, 10)
        }

        if(input$adjusted_checkbox){
          shiny::updateSelectInput(session, "variable", choices = variableList[-(adjChoices)])
        }
        else{
          shiny::updateSelectInput(session, "variable", choices = variableList)
        }
      })

      ###change demographic checkbox options based on year selected in recall panel###
      shiny::observeEvent(input$dataset, {
        selected_dataset <- input$dataset

        # Get the dataset based on the selected option
        dataset <- selected_dataset()

        # Get the unique observations of demographic variables and update their respective checkbox options

        #recall panel
        sex_choices <- unique(dataset$SEX)
        shiny::updateCheckboxGroupInput(session, "sex", choices = sort(sex_choices), selected = sex_choices)
        race_choices <- unique(dataset$RACE_ETH)
        shiny::updateCheckboxGroupInput(session, "race", choices = sort(race_choices), selected = race_choices)
        age_choices <- unique(dataset$ageBracket)
        shiny::updateCheckboxGroupInput(session, "age", choices = sort(age_choices), selected = age_choices)
        inc_choices <- unique(dataset$FAMINC)
        shiny::updateCheckboxGroupInput(session, "income", choices = sort(inc_choices), selected = inc_choices)
      })

      # get correct demographic options based on the year selected for scoring
      shiny::observeEvent(list(input$scoringDataset, input$scoringAgeChoice), {
        # Get the dataset based on the selected option
        dataset <- selected_scoringDataset()

        #Get the unique observations of demographic variables and update their respective checkbox options

        sex_choices <- unique(dataset$SEX)
        shiny::updateCheckboxGroupInput(session, "scoringSex", choices = sort(sex_choices), selected = sex_choices)
        race_choices <- unique(dataset$RACE_ETH)
        shiny::updateCheckboxGroupInput(session, "scoringRace", choices = sort(race_choices), selected = race_choices)
        age_choices <- unique(dataset$ageBracket)
        if(input$scoringAgeChoice == 'Population 2 Years and Older'){
          shiny::updateCheckboxGroupInput(session, "scoringAge", choices = sort(age_choices)[-c(1)], selected = age_choices)
        }
        else{
          shiny::updateCheckboxGroupInput(session, "scoringAge", choices = sort(age_choices)[1], selected = age_choices)
        }
        inc_choices <- unique(dataset$FAMINC)
        shiny::updateCheckboxGroupInput(session, "scoringIncome", choices = sort(inc_choices), selected = inc_choices)
      })

      # allow user to display mean/pop ratio with a radar plot when on total score variable
      shiny::observeEvent(list(input$scoringVariable, input$scoringMethod), {
        if(input$scoringVariable == "Total Score" & (input$scoringMethod == "Mean Ratio"| input$scoringMethod == "Population Ratio")){
          shiny::updateSelectInput(session, "scoringDisplay", choices = c("Bar Plot", "Radar Plot"))
        }
        else{
          shiny::updateSelectInput(session, "scoringDisplay", choices = "Bar Plot")
        }
      })

      ###change demographic checkbox options based on year selected for demographic panel###
      shiny::observeEvent(input$demoDataset, {
        selected_demoDataset <- input$demoDataset

        # Get the dataset based on the selected option
        dataset <- selected_demoDataset()

        sex_choices <- unique(dataset$SEX)
        race_choices <- unique(dataset$RACE_ETH)
        age_choices <- unique(dataset$ageBracket)
        inc_choices <- unique(dataset$FAMINC)

        shiny::updateCheckboxGroupInput(session, "demoSex", choices = sort(sex_choices), selected = sex_choices)
        shiny::updateCheckboxGroupInput(session, "demoRace", choices = sort(race_choices), selected = race_choices)
        shiny::updateCheckboxGroupInput(session, "demoAge", choices = sort(age_choices), selected = age_choices)
        shiny::updateCheckboxGroupInput(session, "demoIncome", choices = sort(inc_choices), selected = inc_choices)
      })

      ###Create the plot from subset selected###
      output$myPlot <- shiny::renderPlot({

        # Filter to demographics selected and only keep weight and selected recall columns
        filtered_data <- selected_dataset() %>%
          dplyr::filter(SEX %in% input$sex,
                        RACE_ETH %in% input$race,
                        ageBracket %in% input$age,
                        FAMINC %in% input$income) %>%
          dplyr::select(WTDR2D, dplyr::contains(input$variable), DR1TKCAL, DR2TKCAL)

        if(input$adjusted_checkbox){
          #kcal per 1000 data
          filtered_data <- filtered_data
          filtered_data[,2] <- filtered_data[,2]/((filtered_data$DR1TKCAL)/1000)
          filtered_data[,3] <- filtered_data[,3]/((filtered_data$DR2TKCAL)/1000)
        }

        filtered_data <- filtered_data %>%
          dplyr::select(WTDR2D, dplyr::contains(input$variable)) %>%
          tidyr::drop_na()

        # If option to keep x-axis scale constant is selected
        if(input$x_axis_scaling == 1){
          x_upper_limit <- xVarMax(input$variable, all_datasets)
        } else{
          x_upper_limit <- NA
        }

        # if option to make x axis proportional to max is selected
        if(input$x_axis_scaling == 2 & nrow(filtered_data) != 0){
          max_recall <- max(filtered_data[,2:3], na.rm = TRUE)
          filtered_data <- filtered_data %>%
            dplyr::mutate_at(dplyr::vars(dplyr::contains(input$variable)), ~ (. / max_recall) * 20)
        }

        # If plot average is selected
        if (input$average_checkbox) {

          # Calculate average recalls
          selected_columns <- ({
            filtered_data %>%
              dplyr::mutate(averages = rowMeans(dplyr::across(-1), na.rm = TRUE))
          })

          # Plot the average recalls
          ggplot2::ggplot(selected_columns) +
            ggplot2::geom_histogram(ggplot2::aes(x = averages, y=ggplot2::after_stat(count/sum(count)), weight = WTDR2D), bins = 30, boundary=0) +
            ggplot2::ylab('Proportion') +
            ggplot2::xlab(varToComponent(input$variable)) +
            ggplot2::xlim(0, x_upper_limit) +
            ggplot2::theme_classic() +
            ggplot2::ggtitle(paste0('Weighted Histogram of ', varToComponent(input$variable), ' Recalls'))

        }

        # If plot averages is NOT selected
        else{
          # Create long a dataset with all recalls the selected component in one column
          selected_columns <- ({
            filtered_data %>%
              tidyr::pivot_longer(!WTDR2D, names_to = "day", values_to = "recall") %>%
              dplyr::select(recall, WTDR2D)
          })

          # Plot as individual recalls
          ggplot2::ggplot(selected_columns) +
            ggplot2::geom_histogram(ggplot2::aes(x = recall, y=ggplot2::after_stat(count/sum(count)), weight = WTDR2D), bins = 30, boundary=0) +
            ggplot2::ylab('Proportion') +
            ggplot2::xlab(varToComponent(input$variable)) +
            ggplot2::xlim(0, x_upper_limit) +
            ggplot2::theme_classic() +
            ggplot2::ggtitle(paste0('Weighted Histogram of ', varToComponent(input$variable), ' Recalls'))

        }
      })

      ###Print number of observations on Recall Panel###
      output$num_observations <- shiny::renderText({

        # Filter to demographics selected and only keep weight and selected recall columns
        filtered_data <- selected_dataset() %>%
          dplyr::filter(SEX %in% input$sex,
                        RACE_ETH %in% input$race,
                        ageBracket %in% input$age,
                        FAMINC %in% input$income)

        # display number of subjects used
        paste("Number of Subjects Selected:", nrow(filtered_data))
      })

      #### Scoring Plots ####
      output$scoringPlot <- shiny::renderPlot({

        scoringStandards <- selected_scoringStandards()
        #allows us to call the demographic variable regardless of which one it is
        scoringDemographicVariable <- rlang::sym(input$scoringDemographic)

        #update the subset based on the demographic options selected
        filtered_scoringData <- selected_scoringDataset() %>%
          dplyr::filter(SEX %in% input$scoringSex,
                        RACE_ETH %in% input$scoringRace,
                        ageBracket %in% input$scoringAge,
                        FAMINC %in% input$scoringIncome)

        #set y limit for plotting based on input variable
        if(input$scoringVariable == "Total Score"){
          ymax = 100
        }
        else{
          ymax = hei_standards_2020$max_points[hei_standards_2020$component == input$scoringVariable]
        }

        ##### Simple Scoring #####
        if(input$scoringMethod == "Simple"){
          if(nrow(filtered_scoringData) == 0){
            finalSimpleScores <- data.frame(
              SEQN = numeric(),
              WTDR2D = numeric(),
              score = numeric()
            )
          }
          else{
            if(input$scoringVariable == "Total Score"){
              finalSimpleScores <- filtered_scoringData %>%
                dplyr::select(SEQN, WTDR2D) %>%
                tibble::add_column(score = 0)

              for(variables in variableList_heiComponents[-c(1)]){
                variableSimpleScores <- simpleScoreApp(filtered_scoringData, variables, scoringStandards)

                finalSimpleScores <- finalSimpleScores %>%
                  dplyr::left_join(., variableSimpleScores, by = c("SEQN", "WTDR2D")) %>%
                  dplyr::mutate(score = rowSums(dplyr::select(.,dplyr::contains("score")))) %>%
                  dplyr::select(SEQN, WTDR2D, score) %>%
                  tidyr::drop_na(score)
              }
            }

            else{
              finalSimpleScores <- simpleScoreApp(filtered_scoringData, input$scoringVariable, scoringStandards) %>%
                tidyr::drop_na()
            }
          }

          first_plot <- ggplot2::ggplot(finalSimpleScores) +
            ggplot2::geom_histogram(ggplot2::aes(x = score, y=ggplot2::after_stat(count/sum(count)), weight = WTDR2D), bins = 30, boundary=0) +
            ggplot2::ylab('Proportion') +
            ggplot2::xlab(varToComponent(input$scoringVariable)) +
            ggplot2::theme_classic() +
            ggplot2::theme(axis.text=ggplot2::element_text(color='black', size=11),
                           axis.title = ggplot2::element_text(face="bold", size=15)) +
            ggplot2::labs(x = "Scores") +
            ggplot2::theme_classic() +
            if(input$secondDataset == 'None'){
              if(input$scoringVariable != 'Total Score'){
                ggplot2::ggtitle(paste0('Weighted Histogram of ', varToComponent(input$scoringVariable), ' Scores'))
              } else{
                ggplot2::ggtitle(paste0('Weighted Histogram of Total Scores'))
              }
            }

          print(first_plot)

          if(input$secondDataset != 'None'){

            filtered_scoringData <- secondSelected_scoringDataset() %>%
              dplyr::filter(SEX %in% input$scoringSex,
                            RACE_ETH %in% input$scoringRace,
                            ageBracket %in% input$scoringAge,
                            FAMINC %in% input$scoringIncome)

            if(nrow(filtered_scoringData) == 0){
              finalSimpleScores <- data.frame(
                SEQN = numeric(),
                WTDR2D = numeric(),
                score = numeric()
              )
            }

            else{
              if(input$scoringVariable == "Total Score"){
                finalSimpleScores <- filtered_scoringData %>%
                  dplyr::select(SEQN, WTDR2D) %>%
                  tibble::add_column(score = 0)

                for(variables in variableList_heiComponents[-c(1)]){
                  variableSimpleScores <- simpleScoreApp(filtered_scoringData, variables, scoringStandards)

                  finalSimpleScores <- finalSimpleScores %>%
                    dplyr::left_join(., variableSimpleScores, by = c("SEQN", "WTDR2D")) %>%
                    dplyr::mutate(score = rowSums(dplyr::select(.,dplyr::contains("score")))) %>%
                    dplyr::select(SEQN, WTDR2D, score) %>%
                    tidyr::drop_na(score)
                }
              }

              else{
                finalSimpleScores <- simpleScoreApp(filtered_scoringData, input$scoringVariable, scoringStandards)
                finalSimpleScores <- finalSimpleScores %>%
                  tidyr::drop_na()
              }
            }

            second_plot <- ggplot2::ggplot(finalSimpleScores) +
              ggplot2::geom_histogram(ggplot2::aes(x = score, y=ggplot2::after_stat(count/sum(count)), weight = WTDR2D), bins = 30, boundary=0) +
              ggplot2::ylab('Proportion') +
              ggplot2::xlab(varToComponent(input$scoringVariable)) +
              ggplot2::theme_classic() +
              ggplot2::theme(axis.text=ggplot2::element_text(color='black', size=11),
                             axis.title = ggplot2::element_text(face="bold", size=15)) +
              ggplot2::labs(x = "Scores") +
              ggplot2::theme_classic()

            both_plots <- ggpubr::ggarrange(first_plot,second_plot, ncol=2, nrow=1)

            #add title to both
            ggpubr::annotate_figure(both_plots, top = ggpubr::text_grob(
              if(input$scoringVariable != 'Total Score'){
                paste0('Weighted Histogram of ', varToComponent(input$scoringVariable), ' Scores')
              } else{
                paste0('Weighted Histogram of Total Scores')
              },
              color = "black", face = "bold", size = 14)
            )
          }
        }

        ##### Mean Ratio AND Population Ratio Method Scoring #####

        else{
          #choose scoring method
          if(input$scoringMethod == "Mean Ratio"){
            scoringFunction <- meanRatioApp
          } else if(input$scoringMethod == "Population Ratio"){
            scoringFunction <- popRatioScoreApp
          }

          if(input$scoringVariable == "Total Score"){

            #initialize an empty table for scoring data with correct demographic options
            scoresTable <- filtered_scoringData %>%
              dplyr::select(scoringDemographicVariable) %>%
              unique()

            #for loop calculates score for each variable and adds it to the total score
            for(variables in variableList_heiComponents[-c(1)]){
              if(input$scoringDisplay == "Radar Plot"){
                divForProportion = hei_standards_2020$max_points[hei_standards_2020$component == variables]
              }
              else{
                divForProportion = 1
              }

              scoringDataByVariable <- filtered_scoringData %>%
                dplyr::select(dplyr::contains(variables), dplyr::contains("KCAL"), WTDR2D, scoringDemographicVariable) %>%
                scoringFunction(., variables, scoringDemographicVariable, scoringStandards) %>%
                dplyr::mutate(score = score / divForProportion)

              #add component score to total score
              scoresTable <- scoresTable %>%
                dplyr::right_join(., scoringDataByVariable, by = input$scoringDemographic) %>%
                dplyr::select(scoringDemographicVariable, dplyr::contains("score")) %>%
                tidyr::drop_na()
            }

            #colors for plots by demographic
            if(input$scoringDemographic=='FAMINC'){
              radarColors = grDevices::colorRampPalette(c('cadetblue3', 'blue4'))
              radarColors2 = grDevices::colorRampPalette(c('coral', 'coral4'))
              radarColors = c(radarColors(12), radarColors2(5))
            } else if(input$scoringDemographic=='RACE_ETH' | input$scoringDemographic=='SEX'){
                if(nrow(scoringDataByVariable) >= 3){radarColors = RColorBrewer::brewer.pal(n=nrow(scoringDataByVariable), name='Dark2')}
                else{
                  radarColors = RColorBrewer::brewer.pal(n=3, name='Dark2')
                }
            } else{
              radarColors = grDevices::colorRampPalette(c('cadetblue3', 'blue4'))
              radarColors = radarColors(nrow(scoringDataByVariable))
            }

            #plot as a radar plot
            if(input$scoringDisplay == "Radar Plot"){
              colnames(scoresTable)[-c(1)] <- names(variableList_heiComponents[-c(1)]) #column names in proper display names from list

              radar1 <- ggradar::ggradar(scoresTable, group.colours=radarColors, axis.label.size = 3, grid.label.size = 5) + ggplot2::guides(fill = ggplot2::guide_colorbar()) +
                ggplot2::labs(title=paste0(input$scoringMethod, ' Scores for Dietary Components by ', varToComponent(input$scoringDemographic))) +
                ggplot2::theme(legend.text = ggplot2::element_text(size = 10),
                               plot.margin = ggplot2::margin(.25,0,0,0,'cm'),
                               plot.title = ggplot2::element_text(size=15),
                               plot.title.position = 'plot')

              radarPlotsList = list(radar1)
              print(radar1)

              ############################
              #### TEST SECOND RADAR
              ############################
              if(input$secondDataset != 'None'){

                filtered_scoringData2 <- secondSelected_scoringDataset() %>%
                  dplyr::filter(SEX %in% input$scoringSex,
                                RACE_ETH %in% input$scoringRace,
                                ageBracket %in% input$scoringAge,
                                FAMINC %in% input$scoringIncome)

                scoresTable <- filtered_scoringData2 %>%
                  dplyr::select(scoringDemographicVariable) %>%
                  tidyr::drop_na() %>%
                  unique()

                ############################
                #### TEST CHUNK FOR LOOP
                ############################

                #for loop calculates score for each variable and adds it to the total score
                for(variables in variableList_heiComponents[-c(1)]){
                  if(input$scoringDisplay == "Radar Plot"){
                    divForProportion = hei_standards_2020$max_points[hei_standards_2020$component == variables]
                  }
                  else{
                    divForProportion = 1
                  }

                  scoringDataByVariable <- filtered_scoringData2 %>%
                    dplyr::select(dplyr::contains(variables), dplyr::contains("KCAL"), WTDR2D, scoringDemographicVariable) %>%
                    scoringFunction(., variables, scoringDemographicVariable, scoringStandards) %>%
                    dplyr::mutate(score = score / divForProportion) %>%
                    tidyr::drop_na()

                  #add component score to total score
                  scoresTable <- scoresTable %>%
                    dplyr::right_join(., scoringDataByVariable, by = input$scoringDemographic) %>%
                    dplyr::select(scoringDemographicVariable, dplyr::contains("score")) %>%
                    tidyr::drop_na()

                }
                ############################
                #### TEST CHUNK FOR LOOP END
                ############################

                colnames(scoresTable)[-c(1)] <- names(variableList_heiComponents[-c(1)]) #column names in proper display names from list

                radar2 <- ggradar::ggradar(scoresTable, group.colours=radarColors, axis.label.size = 3, grid.label.size = 5) + ggplot2::guides(fill = ggplot2::guide_colorbar()) +
                  ggplot2::theme(legend.text = ggplot2::element_text(size = 10),
                                 plot.margin = ggplot2::margin(.25,0,0,0,'cm'),
                                 plot.title = ggplot2::element_text(size=15),
                                 plot.title.position = 'plot')

                radar2
                radarPlotsList = list(radar1, radar2)
                ggpubr::ggarrange(plotlist = radarPlotsList, nrow = 1)

              }
              ############################
              #### TEST CHUNK SECOND RADAR END
              ############################

            }
            #plot as a bar plot
            else{
              scoresTable <- scoresTable %>%
                dplyr::mutate(score = rowSums(.[-c(1)])) %>%
                dplyr::select(scoringDemographicVariable, score) %>%
                tidyr::drop_na()

              plot1_mean <- ggplot2::ggplot(scoresTable, ggplot2::aes(x = !!scoringDemographicVariable, y = score, fill=!!scoringDemographicVariable)) +
                ggplot2::geom_bar(stat = "identity") +
                ggplot2::ylim(0, ymax) +
                ggplot2::labs(x = varToComponent(input$scoringDemographic), y = "score") +
                ggplot2::theme_classic() +
                ggplot2::theme(axis.text=ggplot2::element_text(color='black', size=11),
                               axis.title = ggplot2::element_text(face="bold", size=15)) +
                ggplot2::theme_classic() +
                ggplot2::scale_fill_manual(values=radarColors) +
                ggplot2::guides(fill=ggplot2::guide_legend(title=varToComponent(input$scoringDemographic))) +
                if(input$secondDataset == 'None'){
                  ggplot2::ggtitle(paste0(input$scoringMethod, ' Scores by ', varToComponent(input$scoringDemographic)))
                }

              print(plot1_mean)

              ####PLOT 2 FOR MEANS
              if(input$secondDataset != 'None'){

                filtered_scoringData2 <- secondSelected_scoringDataset() %>%
                  dplyr::filter(SEX %in% input$scoringSex,
                                RACE_ETH %in% input$scoringRace,
                                ageBracket %in% input$scoringAge,
                                FAMINC %in% input$scoringIncome)

                scoresTable <- filtered_scoringData2 %>%
                  dplyr::select(scoringDemographicVariable) %>%
                  tidyr::drop_na() %>%
                  unique()

                ############################
                #### TEST CHUNK FOR LOOP
                ############################

                #for loop calculates score for each variable and adds it to the total score
                for(variables in variableList_heiComponents[-c(1)]){
                  if(input$scoringDisplay == "Radar Plot"){
                    divForProportion = hei_standards_2020$max_points[hei_standards_2020$component == variables]
                  }
                  else{
                    divForProportion = 1
                  }

                  scoringDataByVariable <- filtered_scoringData2 %>%
                    dplyr::select(dplyr::contains(variables), dplyr::contains("KCAL"), WTDR2D, scoringDemographicVariable) %>%
                    scoringFunction(., variables, scoringDemographicVariable, scoringStandards) %>%
                    dplyr::mutate(score = score / divForProportion)

                  #add component score to total score
                  scoresTable <- scoresTable %>%
                    dplyr::right_join(., scoringDataByVariable, by = input$scoringDemographic) %>%
                    dplyr::select(scoringDemographicVariable, dplyr::contains("score")) %>%
                    tidyr::drop_na()
                }

                ############################
                #### TEST CHUNK FOR LOOP END
                ############################

                scoresTable <- scoresTable %>%
                  dplyr::mutate(score = rowSums(.[-c(1)])) %>%
                  dplyr::select(scoringDemographicVariable, score) %>%
                  tidyr::drop_na()

                plot2_mean <- ggplot2::ggplot(scoresTable, ggplot2::aes(x = !!scoringDemographicVariable, y = score, fill=!!scoringDemographicVariable)) +
                  ggplot2::geom_bar(stat = "identity") +
                  ggplot2::ylim(0, ymax) +
                  ggplot2::labs(x = varToComponent(input$scoringDemographic), y = "Score") +
                  ggplot2::theme_classic() +
                  ggplot2::theme(axis.text=ggplot2::element_text(color='black', size=11),
                                 axis.title = ggplot2::element_text(face="bold", size=15)) +
                  ggplot2::theme_classic() +
                  ggplot2::scale_fill_manual(values=radarColors) +
                  ggplot2::guides(fill=ggplot2::guide_legend(title=varToComponent(input$scoringDemographic)))

                both_plots <- ggpubr::ggarrange(plot1_mean, plot2_mean, ncol=2, nrow=1)

                #add title to both
                ggpubr::annotate_figure(both_plots, top = ggpubr::text_grob(
                  ggplot2::ggtitle(paste0(input$scoringMethod, ' Scores by ', varToComponent(input$scoringDemographic))),
                  color = "black", face = "bold", size = 14)
                )
              }
            }
          }

          #plot individual components
          else{
            filtered_scoringData <- filtered_scoringData %>%
              dplyr::select(dplyr::contains(input$scoringVariable), dplyr::contains("KCAL"), WTDR2D, scoringDemographicVariable) %>%
              tidyr::drop_na()
            scoresTable <- scoringFunction(filtered_scoringData, input$scoringVariable, scoringDemographicVariable, scoringStandards)

            #colors for plots by demographic
            if(input$scoringDemographic=='FAMINC'){
              radarColors = grDevices::colorRampPalette(c('cadetblue3', 'blue4'))
              radarColors2 = grDevices::colorRampPalette(c('coral', 'coral4'))
              radarColors = c(radarColors(12), radarColors2(5))
            } else if(input$scoringDemographic=='RACE_ETH' | input$scoringDemographic=='SEX'){
              if(nrow(scoresTable) >= 3){radarColors = RColorBrewer::brewer.pal(n=nrow(scoresTable), name='Dark2')}
              else{
                radarColors = RColorBrewer::brewer.pal(n=3, name='Dark2')
              }
            } else{
              radarColors = grDevices::colorRampPalette(c('cadetblue3', 'blue4'))
              radarColors = radarColors(nrow(scoresTable))
            }

            plot1_mean_individual <- ggplot2::ggplot(scoresTable, ggplot2::aes(x = !!scoringDemographicVariable, y = score, fill=!!scoringDemographicVariable)) +
              ggplot2::geom_bar(stat = "identity") +
              ggplot2::ylim(0, ymax) +
              ggplot2::labs(x = varToComponent(input$scoringDemographic), y = "Score") +
              ggplot2::theme_classic() +
              ggplot2::theme(axis.text=ggplot2::element_text(color='black', size=11),
                             axis.title = ggplot2::element_text(face="bold", size=15)) +
              ggplot2::scale_fill_manual(values=radarColors) +
              ggplot2::guides(fill=ggplot2::guide_legend(title=varToComponent(input$scoringDemographic)))

            print(plot1_mean_individual)

            if(input$secondDataset != 'None'){

              filtered_scoringData <- secondSelected_scoringDataset() %>%
                dplyr::filter(SEX %in% input$scoringSex,
                              RACE_ETH %in% input$scoringRace,
                              ageBracket %in% input$scoringAge,
                              FAMINC %in% input$scoringIncome)

              filtered_scoringData <- filtered_scoringData %>%
                dplyr::select(dplyr::contains(input$scoringVariable), dplyr::contains("KCAL"), WTDR2D, scoringDemographicVariable) %>%
                tidyr::drop_na()
              scoresTable <- scoringFunction(filtered_scoringData, input$scoringVariable, scoringDemographicVariable, scoringStandards)

              plot2_mean_individual <- ggplot2::ggplot(scoresTable, ggplot2::aes(x = !!scoringDemographicVariable, y = score, fill=!!scoringDemographicVariable)) +
                ggplot2::geom_bar(stat = "identity") +
                ggplot2::ylim(0, ymax) +
                ggplot2::labs(x = varToComponent(input$scoringDemographic), y = "Score") +
                ggplot2::theme_classic() +
                ggplot2::theme(axis.text=ggplot2::element_text(color='black', size=11),
                               axis.title = ggplot2::element_text(face="bold", size=15)) +
                ggplot2::scale_fill_manual(values=radarColors) +
                ggplot2::guides(fill=ggplot2::guide_legend(title=varToComponent(input$scoringDemographic)))

              both_plots <- ggpubr::ggarrange(plot1_mean_individual, plot2_mean_individual, ncol=2, nrow=1)
              #add title to both
              ggpubr::annotate_figure(both_plots, top = ggpubr::text_grob(
                ggplot2::ggtitle(paste0(input$scoringMethod, ' Scores by ', varToComponent(input$scoringDemographic))),
                color = "black", face = "bold", size = 14)
              )
            }
          }
        }
      })

      output$scoringNum_observations <- shiny::renderText({

        if(input$scoringVariable == "Total Score"){
          filtered_scoringData <- selected_scoringDataset() %>%
          dplyr::filter(SEX %in% input$scoringSex,
                        RACE_ETH %in% input$scoringRace,
                        ageBracket %in% input$scoringAge,
                        FAMINC %in% input$scoringIncome) %>%
          dplyr::select(SEQN, WTDR2D, dplyr::contains(unlist(variableList_heiComponents)), DR1TKCAL, DR2TKCAL) %>%
          tidyr::drop_na()

        subject_num <- nrow(filtered_scoringData)

        if(input$secondDataset != 'None'){

          filtered_scoringData2 <- secondSelected_scoringDataset() %>%
            dplyr::filter(SEX %in% input$scoringSex,
                          RACE_ETH %in% input$scoringRace,
                          ageBracket %in% input$scoringAge,
                          FAMINC %in% input$scoringIncome) %>%
            dplyr::select(SEQN, WTDR2D, dplyr::contains(unlist(variableList_heiComponents)), DR1TKCAL, DR2TKCAL) %>%
            tidyr::drop_na()

          subject_num <- subject_num + nrow(filtered_scoringData2)
        }}

        # For individual components
        else{
          filtered_scoringData <- selected_scoringDataset() %>%
          dplyr::filter(SEX %in% input$scoringSex,
                        RACE_ETH %in% input$scoringRace,
                        ageBracket %in% input$scoringAge,
                        FAMINC %in% input$scoringIncome) %>%
            dplyr::select(SEQN, WTDR2D, dplyr::contains(input$scoringVariable), DR1TKCAL, DR2TKCAL) %>%
            tidyr::drop_na()


        subject_num <- nrow(filtered_scoringData)

        if(input$secondDataset != 'None'){

          filtered_scoringData2 <- secondSelected_scoringDataset() %>%
            dplyr::filter(SEX %in% input$scoringSex,
                          RACE_ETH %in% input$scoringRace,
                          ageBracket %in% input$scoringAge,
                          FAMINC %in% input$scoringIncome) %>%
            dplyr::select(SEQN, WTDR2D, dplyr::contains(input$scoringVariable), DR1TKCAL, DR2TKCAL) %>%
            tidyr::drop_na()

          subject_num <- subject_num + nrow(filtered_scoringData2)
        }}

        paste("Number of Subjects Selected:", subject_num)
        })

      ###Demographics Plots###
      output$demoPlot <- shiny::renderPlot({

        #filter dataset for demographic panel
        filtered_demoData <- selected_demoDataset() %>%
          dplyr::filter(SEX %in% input$demoSex,
                        RACE_ETH %in% input$demoRace,
                        ageBracket %in% input$demoAge,
                        FAMINC %in% input$demoIncome)

        if(input$selectDemo == "Sex"){
          ggplot2::ggplot(filtered_demoData, ggplot2::aes(x = SEX, y = ggplot2::after_stat(prop), group = 1), stat = "count") +
            ggplot2::geom_bar(ggplot2::aes(weight = WTDR2D)) +
            ggplot2::theme_classic() +
            ggplot2::labs(title = "Weighted Distribution of Sex",
                          x = "Sex",
                          y = "Proportion")
        }

        else if(input$selectDemo == "Race"){
          ggplot2::ggplot(filtered_demoData, ggplot2::aes(x = RACE_ETH, y = ggplot2::after_stat(prop), group = 1), stat = "count") +
            ggplot2::geom_bar(ggplot2::aes(weight = WTDR2D)) +
            ggplot2::theme_classic() +
            ggplot2::labs(title = "Weighted Distribution of Race/Ethnicity",
                          x = "Race/Ethnicity",
                          y = "Proportion")
        }

        else if(input$selectDemo == "Age"){
          ggplot2::ggplot(filtered_demoData, ggplot2::aes(x = ageBracket, y = ggplot2::after_stat(prop), group = 1), stat = "count") +
            ggplot2::geom_bar(ggplot2::aes(weight = WTDR2D)) +
            ggplot2::theme_classic() +
            ggplot2::labs(title = "Weighted Distribution of Age",
                          x = "Age Bracket",
                          y = "Proportion")
        }

        else if(input$selectDemo=='Income'){
          ggplot2::ggplot(filtered_demoData, ggplot2::aes(x = factor(FAMINC, levels = c("[0, 5000)","[5000, 10000)","[10000, 15000)","[15000, 20000)","[20000, 25000)","[25000, 35000)", "[35000, 45000)","[45000, 55000)","[55000, 65000)","[65000, 75000)","[75000, 100000)", ">100000","Refused","Don't know",">20000","<20000")), y = ggplot2::after_stat(prop), group = 1), stat = "count") +
            ggplot2::geom_bar(ggplot2::aes(weight = WTDR2D)) +
            ggplot2::theme_classic() +
            ggplot2::labs(title = "Weighted Distribution of Family Income",
                          x = "Income Bracket",
                          y = "Proportion") +
            ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust=1))
        }
      })

      output$demoNum_observations <- shiny::renderText({

        filtered_demoData <- selected_demoDataset() %>%
          dplyr::filter(SEX %in% input$demoSex,
                        RACE_ETH %in% input$demoRace,
                        ageBracket %in% input$demoAge,
                        FAMINC %in% input$demoIncome) %>%
          dplyr::select(WTDR2D, dplyr::contains(input$selectDemo)) %>%
          tidyr::drop_na()

        paste("Number of Subjects Selected:", nrow(filtered_demoData))
      })
    }

    shiny::shinyApp(ui, server)
  }

  else {
    print("Shiny app must be run in an interactive session.")
  }
}
