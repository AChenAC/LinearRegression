# LinearRegression

<!-- badges: start -->
  [![R-CMD-check](https://github.com/AChenAC/LinearRegression/workflows/R-CMD-check/badge.svg)](https://github.com/AChenAC/LinearRegression/actions)
[![codecov](https://codecov.io/gh/AChenAC/LinearRegression/branch/main/graph/badge.svg)](https://codecov.io/gh/AChenAC/LinearRegression)
<!-- badges: end -->

The goal of LinearRegression is to mimic the existing R functions that are closely related to fitting a linear regression model and anova. You functions that are mimicked includes the following: `lm()`, `summary()`, `confint()`, `hatvalues()`, `rstudent()`, `rstandard()`, `anova()`, `Anova(Model, Type = "III")` and some output could be extract with `$` from `lm()` and `summary()` such as residuals, fitted values and etc. 

## Installation 
Run the following code to install the package:
```{r}
install.packages('devtools')
install.packages("bench")
install.packages("ggbeeswarm")
devtools::install_github('AChenAC/LinearRegression', build_vignettes = T)
library("LinearRegression")
```

## Features 
`LinearRegression::LR()` is used to fit simple linear regression or multiple linear regression. You can decide whether to include an intercept or exclude the intercept with option `include.intercept` when fitting the model. The function does not explicitly present any output until you extract the desired output with `$` followed by its name. To learn more about items which could be extracted from this function, please refer to the help page via `?LR` for detailed information. 

`LinearRegression::ANOVA()` is used to obtain ANOVA table. You can either choose to obtain sequential sums of squares ANOVA table or partial sums of squares ANOVA table. The output is in a data.frame. For more detailed usage about this function, please refer to the help page via `?ANOVA`.


## Example
```{r}
LR(mpg ~ cyl + wt, mtcars)$coefficients ## Fit a model with intercept & extract coefficients
ANOVA(mpg ~ cyl + wt + qsec, mtcars, type = "Sequential") ## Obtain sequential sums of squares 
```
For more detailed tutorial and comparisons against the existing R function, please refer to the vignettes via the following code `browseVignettes(package = 'LinearRegression')`. 

