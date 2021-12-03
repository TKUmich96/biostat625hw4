#'lm2
#'
#'lm2 is used to fit linear models with regression
#'
#'@param formula An object of class "formula" that models regression to be fit
#'
#'@param data Data that perform the linear regression on
#'
#'@param res_display Bool value that used to contorl display lm2 result or not
#'
#'@examples
#'lm2(formula = fixed.acidity ~ volatile.acidity + citric.acid * residual.sugar, data = wine)
#'example <- lm2(formula = quality ~ free.sulfur.dioxide:pH + pH, data = wine)
#'example$coefficients
#'
#'@return a list that contains the following values: coefficients, call, x, y, Missing.N
#'
#'@importFrom stats model.matrix
#'
#'@export

lm2 = function(formula, data, res_display = TRUE){
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

  ########## check NA and perform na.action's default with na.omit ##########
  if(any(is.na(data))){
    data = data[complete.cases(data), ]
  } else {
    data = data
  }
  missing_n = org_n - nrow(data)

  y <- as.matrix(data[,which(colnames(data) == otcm_var)])
  x <- model.matrix(formula, data = data)

  ########## Calculate coeffficents, fitted value, residuals and degree of freedom  By RCPP ##########
  sourceCpp("CPP_CalValue.cpp")
  res = get_Cal_val(x,y)

  beta = res$beta
  rownames(beta) = c('(Intercept)',dpdt_var)

  # noquote could take string from quotation marks
  call = noquote(paste(c('lm(formula = ', formula, ')'), collapse = ''))

  output <- list(call,beta[,1],x,y,missing_n)
  names(output) <- c("call", "coefficients","x", "y","missing.N")

  if(res_display == T){
    cat("Call: ", "\n",
        output$call, "\n",
        ' ', "\n",
        "Coefficients: ","\n",sep ="")
    print(round(output$coefficients,4))
  }

  return(invisible(output))
}
