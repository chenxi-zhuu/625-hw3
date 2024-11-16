# Logistic Regression model

<!-- badges: start -->
  [![R-CMD-check](https://github.com/chenxi-zhuu/625-hw3/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/chenxi-zhuu/625-hw3/actions/workflows/R-CMD-check.yaml)
[![Codecov test coverage](https://codecov.io/gh/chenxi-zhuu/625-hw3/graph/badge.svg)](https://app.codecov.io/gh/chenxi-zhuu/625-hw3)
<!-- badges: end -->

The goal of this R package is to build a logistic regression model `my_logistic_regression` based on the Newton algorithm to estimate coefficients, print `print.my_logistic_regression`, predict `preidtc.my_logistic_regression`, and draw ROC curves `plor_roc` based on the S3 method, and calculate the accuracy `accuracy` of the model based on C++.

## Installation

You can install the development version of `GLMhw` (Logisitic regression model) from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("chenxi-zhuu/625-hw3")
```

## Loading packages

``` r
library(GLMhw)
```

You can find more details tutorial about the “GLMhw” package (Logistic Regression Model) in Vignettes folder or use the following code:

``` r
browseVignettes("GLMhw")
```

## Examples

You can use the following code to test my Logistic Regression Model:

``` r
data("test_data")
model <- my_logistic_regression(y~x1+x2,data=test_data)
print(model)
pre <- predict(model,test_data)
plot_roc(model)
accuracy(pre$prob,test_data$y)
```
