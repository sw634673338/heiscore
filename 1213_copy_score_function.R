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

# 1 build index for a component
component_index <- function(component_selected, year){

  # drop lines startting with DR2
  #df <- df_raw %>%
    #select(-starts_with("DR2"))

    df_component <- score(method = "Simple",  years = year,  component = component_selected, dr = 1) # they rename the arguments
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
