test_that("logistic works", {
  expect_equal(
    {model_true <- glm(y~x1+x2,data=test_data, family=binomial)
    round(as.vector(coef(model_true)),5)},
    {
      model <- my_logistic_regression(y~x1+x2, data=test_data)
      round(as.vector(t(coef(model))),5)
    })
})
