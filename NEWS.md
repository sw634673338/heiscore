# heiscore 0.1.3

* Reduced the size of the data directory by rounding raw dietary intake values without impacting the accuracy of HEI calculations. 

# heiscore 0.1.2

* Added color to bar plots created by `plotScore()`.

* Capitalized HEI components in `plotScore()` and `runShinyApp()`.

* Eliminated rounding error of the Fatty Acids and Saturated Fats HEI components in `plotScore()`, `runShinyApp()` and `score()`.

* Fixed error in calculation of the Fatty Acids HEI component in `plotScore()`, `runShinyApp()` and `score()`.

# heiscore 0.1.1

* `plotScore()` and `runShinyApp()` now use colorblind safe color schemes for all plots.

* `runShinyApp()` now uses dropdown menus rather than checkboxes to subset the population. Additionally, a "Select All" button is provided for ease.

* `score()` and `plotScore()` are no longer sensitive to incorrect capitalization or extra white space for arguments that take character strings.

* In `score()` and `plotScore()`, `scoringMethod`, `heiComponent`, `demographicGroup`, `raceEthnicity`, and `familyIncome` have been shortened to `method`, `component`, `demo`, `race`, and `income`, respectively. 

* `score()` now returns the factor class for demographic variables in the scoring output.

# heiscore 0.0.1

* Initial CRAN submission.
