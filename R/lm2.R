#'lm2
#'
#'lm2 is used to fit linear models with regression.
#'
#'@param formula An object of class "formula" that models regression to be fit.
#'
#'@param data Data that perform the linear regression on.
#'
#'@param na.handle Opition to deal with NA. Two options valid: "na.omit"-- omit NA in data, and "na.fail" -- Report an error and stop the function. The default value is set to "na.omit".
#'
#'@param res_display Bool value that used to contorl display lm2 result or not. The default value is set to TRUE. Set it to FALSE if displaying is not required.
#'
#'
#'@examples
#'lm2(formula = Temp ~ Wind + Solar.R, data = airquality, na.handle = "na.omit")
#'
#'result = lm2(formula = Rape ~ Murder+Assault+UrbanPop, data = USArrests, res_display = F)
#'result$coefficients
#'
#'@return A list that contains the following values: call(formula), coefficients, residuals, fitted.values, x(independent variables), y(dependent variable),missing.N(missing numbers of data due to NA), df(residual degree of freedom) and betas(coefficient in other format)
#'
#'@importFrom stats model.matrix
#'
#'@export

lm2 = function(formula, data, na.handle = "na.omit", res_display = TRUE){
  library(Rcpp)

  otcm_var = all.vars(formula)[1]
  dpdt_var = labels(terms(formula))
  covariates = c(otcm_var, dpdt_var)

  ########## Deal with interaction term ##########
  covariates = unique(unlist(strsplit(covariates,":")))
  covariates = unique(unlist(strsplit(covariates,"\\*")))

  extr_ind = sapply(1:length(covariates),
                    function(i) which(colnames(data) == covariates[i]))
  data = data[,extr_ind]
  org_n = nrow(data)

  ########## check NA and perform na.handle's default with na.omit ##########
  if(any(is.na(data))){
    if(na.handle == "na.omit"){
      data = data[complete.cases(data), ]
    } else if(na.handle == "na.fail"){
      stop("Missing values contains in study, please handle!")
    }
  }
  missing_n = org_n - nrow(data)

  y <- as.matrix(data[,which(colnames(data) == otcm_var)])
  x <- model.matrix(formula, data = data)

  ########## Calculate coeffficents, fitted value, residuals and degree of freedom  By RCPP ##########
  sourceCpp("CPP_CalValue.cpp")
  res = get_Cal_val(x,y)

  beta = res$beta
  rownames(beta) = c('(Intercept)',dpdt_var)
  coef = beta[,1]
  
  resid_val = res$resid_val[,1]
  names(resid_val) = row.names(data)
  
  fitted_val = res$fitted_val[,1]
  names(fitted_val) = row.names(data)
  
  df.residual = res$df
  
  # noquote could take string from quotation marks
  call = noquote(paste(c('lm(formula = ', formula, ')'), collapse = ''))

  output <- list(call,coef,resid_val,fitted_val,x,y,missing_n,df.residual, beta)
  names(output) <- c("call", "coefficients","residuals","fitted.values","x", "y","missing.N","df", "betas")

  if(res_display == T){
    cat("Call: ", "\n",
        output$call, "\n",
        ' ', "\n",
        "Coefficients: ","\n",sep ="")
    print(round(output$coefficients,4))
  }

  return(invisible(output))
}
