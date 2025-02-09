
# Retrieve NHANES dietary data converted to Food Patterns components and
# demographic data for 2017 and 2018
head(selectDataset(years = '1718'))
#>    SEQN    WTDRD1    WTDR2D    SEX RACE_ETH AGE         FAMINC DR1TKCAL
#> 1 93703     0.000        NA Female    Asian   2        >100000       NA
#> 2 93704 81714.005 82442.869   Male    White   2        >100000     1230
#> 3 93705  7185.561  5640.391 Female    Black  66 [10000, 15000)     1202
#> 4 93706  6463.883     0.000   Male    Asian  18           <NA>     1987
#> 5 93707 15333.777 22707.067   Male    Other  13 [65000, 75000)     1775
#> 6 93708 10825.545 22481.854 Female    Asian  66 [25000, 35000)     1251
#>   DR2TKCAL DR1T_F_TOTAL DR2T_F_TOTAL DR1_FWHOLEFRT DR2_FWHOLEFRT DR1T_F_JUICE
#> 1       NA           NA           NA            NA            NA           NA
#> 2     1356         0.99         1.94          0.00          1.07         0.99
#> 3     1235         0.00         0.03          0.00          0.00         0.00
#> 4       NA         0.00           NA          0.00            NA         0.00
#> 5     1794         0.00         0.00          0.00          0.00         0.00
#> 6      842         0.49         0.00          0.49          0.00         0.00
#>   DR2T_F_JUICE DR1_VTOTALLEG DR2_VTOTALLEG DR1_VDRKGRLEG DR2_VDRKGRLEG
#> 1           NA            NA            NA            NA            NA
#> 2         0.87          0.82          0.02          0.00          0.00
#> 3         0.03          1.65          0.52          0.46          0.23
#> 4           NA          0.72            NA          0.00            NA
#> 5         0.00          0.94          0.70          0.00          0.00
#> 6         0.00          2.33          0.26          0.00          0.00
#>   DR1_VNONDRKGR DR2_VNONDRKGR DR1T_V_DRKGR DR2T_V_DRKGR DR1T_V_LEGUMES
#> 1            NA            NA           NA           NA             NA
#> 2          0.82          0.02         0.00            0           0.00
#> 3          1.19          0.29         0.31            0           0.15
#> 4          0.72            NA         0.00           NA           0.00
#> 5          0.94          0.70         0.00            0           0.00
#> 6          2.33          0.26         0.00            0           0.00
#>   DR2T_V_LEGUMES DR1T_G_WHOLE DR2T_G_WHOLE DR1T_D_TOTAL DR2T_D_TOTAL
#> 1             NA           NA           NA           NA           NA
#> 2           0.00         0.00         0.72         1.73         1.74
#> 3           0.23         0.00         0.00         0.22         0.52
#> 4             NA         0.00           NA         2.03           NA
#> 5           0.00         4.12         0.90         0.26         0.04
#> 6           0.00         0.34         0.00         0.06         1.14
#>   DR1_PFALLPROTLEG DR2_PFALLPROTLEG DR1_PFSEAPLANTLEG DR2_PFSEAPLANTLEG
#> 1               NA               NA                NA                NA
#> 2             3.52             0.46              1.18              0.00
#> 3             1.38             3.66              1.37              1.31
#> 4             8.79               NA              0.00                NA
#> 5             5.61            10.30              0.00              0.07
#> 6             4.73             2.34              2.57              1.42
#>   DR1_PF_MPE DR2_PF_MPE DR1_PF_SSNS DR2_PF_SSNS DR1T_PF_LEGUMES DR2T_PF_LEGUMES
#> 1         NA         NA          NA          NA              NA              NA
#> 2       0.22       0.46        1.18        0.00            0.00            0.00
#> 3       0.01       2.35        0.79        0.40            0.58            0.91
#> 4       0.00         NA        0.00          NA            0.00              NA
#> 5       3.55       8.94        0.00        0.07            0.00            0.00
#> 6       2.16       0.03        2.57        1.42            0.00            0.00
#>   DR1_TFACIDS DR2_TFACIDS DR1T_G_REFINED DR2T_G_REFINED DR1TSODI DR2TSODI
#> 1          NA          NA             NA             NA       NA       NA
#> 2      2.3601      2.2851           3.03           4.28     2198     1345
#> 3      2.2037      2.6248           2.10           5.39     3574     2447
#> 4      2.7205          NA           4.51             NA     3657       NA
#> 5      1.3875      1.7117           1.14           2.04     2450     2581
#> 6      2.5119      1.4630           3.19           2.59     2135     1241
#>   DR1T_ADD_SUGARS DR2T_ADD_SUGARS DR1TSFAT DR2TSFAT DR1_MONOPOLY DR2_MONOPOLY
#> 1              NA              NA       NA       NA           NA           NA
#> 2            9.12           17.25   11.372   10.368       26.839       23.692
#> 3           19.42            8.67   16.435   14.079       36.218       36.954
#> 4            1.64              NA   35.169       NA       95.678           NA
#> 5           18.65           18.15   33.252   31.098       46.136       53.232
#> 6            6.00            6.44   17.446   14.962       43.823       21.889

# Produce 2017-March 2020 Pre-pandemic HEI scores for the Total Fruit component
# using the Mean Ratio scoring method. Only include white and black women aged
# 50 to 100. Display the results by race/ethnicity.
df = score(method = "Simple",
      years = "1720",
      component = "Total Fruit")
#> # A tibble: 2 × 2
#>   RACE_ETH score
#>   <fct>    <dbl>
#> 1 Black     3.57
#> 2 White     3.43
