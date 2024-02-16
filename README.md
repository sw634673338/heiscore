
<!-- README.md is generated from README.Rmd. Please edit that file -->

# heiscore

<!-- badges: start -->
<!-- badges: end -->

## Overview

The heiscore package aims to increase the accessibility of evaluating a
population’s dietary habits using the Healthy Eating Index (HEI) and
enable straightforward comparisons of the diet quality of different
demographic subgroups. It allows users with minimal technical experience
to obtain preloaded dietary recall data from the National Health and
Nutrition Examination Survey (NHANES) and use it calculate HEI scores
that are representative of the U.S. population using three distinct
methods prescribed by the National Cancer Institute. Additionally,
heiscore includes functions that visualize this multidimensional diet
quality data via various graphing techniques including histograms, bar
charts, and radar charts. These plots facilitate clear comparisons of
dietary patterns between sub-populations in the U.S. and across several
years.

heiscore’s four functions:

- `selectDataset()` retrieves a tibble containing NHANES dietary recall
  data converted to relevant food groups (the U.S. Department of
  Agriculture’s Food Patterns Components) and demographic data for the
  chosen years.
- `score()` calculates HEI scores for the chosen time period and
  demographic subgroups.
- `plotScore()` graphs HEI score distributions for the chosen time
  period and demographic subgroups.
- `runShinyApp()` launches an interactive R shiny app that plots the
  distributions of raw dietary data, demographic information, and HEI
  scores with options to compare years and demographic subgroups of
  interest

## Installation

``` r
# Install the heiscore package from GitHub
install.packages("devtools")
library(devtools)
install_github("author/package")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(heiscore)
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

# Retrieve NHANES dietary data converted to Food Patterns components and 
# demographic data for 2017 and 2018 
selectDataset(year = '1718')
#> # A tibble: 8,704 × 51
#>     SEQN WTDRD1 WTDR2D SEX    RACE_ETH           AGE FAMINC    DR1TKCAL DR2TKCAL
#>    <dbl>  <dbl>  <dbl> <chr>  <chr>            <dbl> <chr>        <dbl>    <dbl>
#>  1 93703     0     NA  Female Asian                2 ">100000"       NA       NA
#>  2 93704 81714. 82443. Male   White                2 ">100000"     1230     1356
#>  3 93705  7186.  5640. Female Black               66 "[10000,…     1202     1235
#>  4 93706  6464.     0  Male   Asian               18 ""            1987       NA
#>  5 93707 15334. 22707. Male   Other               13 "[65000,…     1775     1794
#>  6 93708 10826. 22482. Female Asian               66 "[25000,…     1251      842
#>  7 93709     0     NA  Female Black               75 "[5000, …       NA       NA
#>  8 93710  8616.  7185. Female White                0 ">100000"      900     1195
#>  9 93711  9098.  8230. Male   Asian               56 ">100000"     2840     2819
#> 10 93712 60947. 89066. Male   Mexican American    18 "[15000,…     2045     3348
#> # ℹ 8,694 more rows
#> # ℹ 42 more variables: DR1T_F_TOTAL <dbl>, DR2T_F_TOTAL <dbl>,
#> #   DR1_FWHOLEFRT <dbl>, DR2_FWHOLEFRT <dbl>, DR1T_F_JUICE <dbl>,
#> #   DR2T_F_JUICE <dbl>, DR1_VTOTALLEG <dbl>, DR2_VTOTALLEG <dbl>,
#> #   DR1_VDRKGRLEG <dbl>, DR2_VDRKGRLEG <dbl>, DR1_VNONDRKGR <dbl>,
#> #   DR2_VNONDRKGR <dbl>, DR1T_V_DRKGR <dbl>, DR2T_V_DRKGR <dbl>,
#> #   DR1T_V_LEGUMES <dbl>, DR2T_V_LEGUMES <dbl>, DR1T_G_WHOLE <dbl>, …

# Produce 2011-12 HEI scores for the Total Fruit component using the Mean Ratio 
# scoring method. Only include white and black women aged 50 to 100 with a 
# family income of $75,000 or more. Display the results by race/ethnicity. 
score(scoringMethod = "Mean Ratio",
      years = "1112",
      heiComponent = "Total Fruit",
      demographicGroup = "Race/Ethnicity",
      sex = ("Female"),
      raceEthnicity = c("White", "Black"),
      age = c(50, 100),
      familyIncome = c("[75000, 100000)", "75000+", ">100000"))
#> # A tibble: 2 × 2
#>   RACE_ETH score
#>   <chr>    <dbl>
#> 1 Black     4.30
#> 2 White     4.18

# Create a radar plot that displays the breakdown of HEI score components by 
# family income level. Use 2017-18 NHANES data and the Population Ratio Method 
# for scoring.
plotScore(graph = "Radar", 
          scoringMethod = "Pop Ratio", 
          years = "1718", 
          heiComponent = "Total Score", 
          demographicGroup = "Family Income", 
          familyIncome = c("[0, 5000)", "[15000, 20000)", "[35000, 45000)", "[45000, 55000)", 
                           "[55000, 65000)", "[65000, 75000)", "[75000, 100000)", ">100000"))
```

<img src="man/figures/README-example-1.png" width="100%" />

``` r
# Launch the interactive Shiny app
runShinyApp()
```
