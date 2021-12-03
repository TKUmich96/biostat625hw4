test_that("lm2: test default na.handle & res_display work", {
  expect_output(lm2(formula = Rape ~ Murder+Assault+UrbanPop, data = USArrests))
})

test_that("lm2: test na.fail na.action works", {
  expect_error(lm2(formula = Rape ~ Murder+Assault+UrbanPop, data = USArrests, na.handle = "na.fail"))
})

test_that("lm2: missing.N Correct", {
  expect_equal(lm2(formula = Rape ~ Murder+Assault+UrbanPop, data = USArrests)$missing.N, 0)
})

test_that("lm2: Residual Degrees of Freedom Correct", {
  expect_equal(lm2(formula = Rape ~ Murder+Assault+UrbanPop, data = USArrests)$df, 46)
})

test_that("lm2: Residuals Match LM output", {
  expect_equal(lm2(formula = Rape ~ Murder+Assault+UrbanPop, data = USArrests)$residuals, lm(formula = Rape ~ Murder+Assault+UrbanPop, data = USArrests)$residuals)
})

test_that("lm2: Coefficent Match LM output", {
  expect_equal(lm2(formula = Rape ~ Murder+Assault+UrbanPop, data = USArrests)$coefficients, lm(formula = Rape ~ Murder+Assault+UrbanPop, data = USArrests)$coefficients)
})

test_that("lm2: Check that input for formula argument is required", {
  expect_error(lm2(data = USArrest))
})

test_that("lm2: Check that input for data argument is required", {
  expect_error(lm2(formula = Rape ~ Murder+Assault+UrbanPop))
})




