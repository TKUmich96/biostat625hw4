\name{lm2Wsmry-package}
\alias{lm2Wsmry-package}
\alias{lm2Wsmry}
\docType{package}
\title{
\packageTitle{lm2Wsmry}
}
\description{
\packageDescription{lm2Wsmry}
}
\details{

The DESCRIPTION file:
\packageDESCRIPTION{lm2Wsmry}
\packageIndices{lm2Wsmry}
lm2Wsmry is a package for homework project process. By this package, a linear regression can be fitted, and a related summary output contains information like residuals, coefficient, standard errors, R^2, adjusted R^2, F statistics, related p-value and some other statistics value can be achieved. This package is trying to reproduce the output result given by lm() and summary(lm()) in base, by lm2() and summary_lm2() respectively. To have more details on each function, please check the help page for each function.
}
\author{
\packageAuthor{lm2Wsmry}

Maintainer: \packageMaintainer{lm2Wsmry}
}

\examples{
lm2(formula = Temp ~ Wind + Solar.R, data = airquality, na.handle = "na.omit")
result = lm2(formula = Rape ~ Murder+Assault+UrbanPop, data = USArrests, res_display = F)
result$coefficients

summary_lm2(lm2(formula = Rape ~ Murder+Assault+UrbanPop, data = USArrests, res_display = F))

result = summary_lm2(lm2(formula = Rape ~ Murder+Assault+UrbanPop, data = USArrests, res_display = F), res_display = F)
result$resd.tb
result$rank
}
