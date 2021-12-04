test_that("summary_lm2: good significant level", {
  expect_output(summary_lm2(lm2(formula = Rape ~ Murder+Assault, data = USArrests, res_display = F)))
})

test_that("summary_lm2: bad significant level", {
  expect_output(summary_lm2(lm2(formula = Murder~Assault+UrbanPop, data = USArrests, res_display = F)))
})

test_that("summary_lm2: report missing", {
  expect_output(summary_lm2(lm2(formula = Temp ~ Wind + Solar.R, data = airquality, na.handle = "na.omit", res_display = F)))
})

test_that("summary_lm2: Residuals Correct", {
  expect_equal(summary_lm2(lm2(formula = Rape ~ Murder+Assault+UrbanPop, data = USArrests, res_display = F))$residuals,
               summary(lm(formula = Rape ~ Murder+Assault+UrbanPop, data = USArrests))$residuals)
})

test_that("summary_lm2: Covariance Matrix Correct", {
  expect_equal(as.matrix(summary_lm2(lm2(formula = Rape ~ Murder+Assault+UrbanPop, data = USArrests, res_display = F))$coefficients),
               as.matrix(summary(lm(formula = Rape ~ Murder+Assault+UrbanPop, data = USArrests))$coefficients))
})

test_that("summary_lm2: R square Correct", {
  expect_equal(as.matrix(summary_lm2(lm2(formula = Rape ~ Murder+Assault+UrbanPop, data = USArrests, res_display = F))$r.squared),
               as.matrix(summary(lm(formula = Rape ~ Murder+Assault+UrbanPop, data = USArrests))$r.squared))
})


test_that("summary_lm2: Adjusted R Square Correct", {
  expect_equal(summary_lm2(lm2(formula = Rape ~ Murder+Assault+UrbanPop, data = USArrests, res_display = F))$adj.r.squared,
               summary(lm(formula = Rape ~ Murder+Assault+UrbanPop, data = USArrests))$adj.r.squared)
})


test_that("summary_lm2: Fstatistic Correct", {
  expect_equal(summary_lm2(lm2(formula = Rape ~ Murder+Assault+UrbanPop, data = USArrests, res_display = F))$fstatistic,
               summary(lm(formula = Rape ~ Murder+Assault+UrbanPop, data = USArrests))$fstatistic)
})


test_that("summary_lm2: Unscaled Covariance Matrix Correct", {
  expect_equal(summary_lm2(lm2(formula = Rape ~ Murder+Assault+UrbanPop, data = USArrests, res_display = F))$cov.unscaled,
               summary(lm(formula = Rape ~ Murder+Assault+UrbanPop, data = USArrests))$cov.unscaled)
})
