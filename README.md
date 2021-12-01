# LinearRegression

<!-- badges: start -->
  [![R-CMD-check](https://github.com/AChenAC/LinearRegression/workflows/R-CMD-check/badge.svg)](https://github.com/AChenAC/LinearRegression/actions)
[![codecov](https://codecov.io/gh/AChenAC/LinearRegression/branch/main/graph/badge.svg)](https://codecov.io/gh/AChenAC/LinearRegression)
<!-- badges: end -->

The goal of LinearRegression is to mimic the existing R functions that are closely related to fitting a linear regression model and anova. You functions that are mimicked includes the following: `lm()`, `summary()`, `confint()`, `hatvalues()`, `rstudent()`, `rstandard()`, `anova()`, `Anova(Model, Type = "III")` and some output could be extract with `$` from `lm()` and `summary()` such as residuals, fitted values and etc. 

## Installation 
Run the following code to install the package:
```{r}
devtools::install_github('AChenAC/LinearRegression', build_vignettes = T)
library("LinearRegression")
```

## Features 
`LinearRegression::LR()` is used to fit simple linear regression or multiple linear regression with either including an intercept or without an intercept. The function does not explicitly return any output until you extract the desired output with `$`. Please refer to the help page `?LR` for more detailed features about this function. 

`LinearRegression::ANOVA()` is used to obtain ANOVA table. You can either choose to obtain sequential sums of squares ANOVA table or partial sums of squares ANOVA table. The output is in a data.frame. For more detailed usage about this function, please refer to the help page `?ANOVA`.


## Example
```{r}
LR(mpg ~ cyl + wt, mtcars)$coefficients ## Fitting a model with intercept included and extract its coefficients
ANOVA(mpg ~ cyl + wt + qsec, mtcars, type = "Sequential") ## Obtain sequential sums of squares 
```
For more detailed tutorial and comparisons against the existing R function, please refer to the vignettes via the following code `browseVignettes(package = 'LinearRegression')`. 

