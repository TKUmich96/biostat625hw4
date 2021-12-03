#'summary_lm2
#'
#'summary_lm2 is used to summarize fit for linear regression models
#'
#'@param lm_mod return of lm2.
#'
#'@param res_display Bool value that used to contorl display summary_lm2 result or not
#'
#'@return a list that contains the following values: call, residuals, coefficients, RSE, df.residual, r.squared, adj.r.squared, fstatistic, f.pval, missing.N, cov.unscaled
#'
#'@examples
#'
#'@importFrom stats model.matrix
#'
#'@export

summary_lm2 = function(lm_mod, res_display = TRUE){
  beta_coef = lm_mod$coefficients
  resid_val = lm_mod$residuals
  fitted_val = lm_mod$fitted.values
  df.residual = lm_mod$df
  betas = lm_mod$betas
  
  ########## Residuals ##########
  resid_val.tb = round(quantile(resid_val),4)
  names(resid_val.tb) = c("Min", "1Q", "Median", "3Q", "Max")

  ########## Standard Error ##########
  SSyy = sum((lm_mod$y-mean(lm_mod$y))^2)
  SSE = sum(resid_val^2)
  RSE = sqrt(SSE/df.residual)
  SES = sqrt(diag(res$xtx)) * sqrt(SSE/df.residual)
  
  ########## T statistics and its related P-values ########## 
  tstats = betas / SES
  pvalues = 2*pt(abs(tstats), df = df.residual, lower.tail=FALSE)
  coef_res = data.frame("Estimate" = betas, "Std. Error" = SES,
                       "t value" = tstats, "Pr(>|t|)" = pvalues, check.names = F)
  sig = c()
  sig = sapply(1:length(pvalues), function(i) {
    if(pvalues[i] < 0.001){sig[i] = '***'} 
    else if (pvalues[i] < 0.01){sig[i] = '** '}
    else if (pvalues[i] < 0.05){sig[i] = '*  '}
    else if (pvalues[i] < 0.1){sig[i] = '.  '} 
    else{sig[i] = ''}
  })
  coef.tb = cbind(signif(coef_res,4), sig)
  colnames(coef.tb)[5] = ""

  ########## R-Squared ##########
  R2  = 1- sum(resid_val^2) / SSyy
  R2adj  = 1 - (1-R2)* (nrow(lm_mod$x)-1)/df.residual
  
  ########## F statistics ##########
  S1sq = (SSyy-SSE)/(ncol(lm_mod$x)-1)
  S2sq = SSE/df.residual
  Fstat  = S1sq/S2sq
  F.stat3  = c(Fstat, ncol(lm_mod$x)-1, df.residual)
  names(F.stat3)  = c("value", "numdf", "dendf")
  
  F.p_val  = pf(Fstat, F.stat3[2], F.stat3[3], lower.tail = FALSE)
  
  ########## Unscaled var-covarience matrix ##########
  var_cov_mat  = solve(t(lm_mod$x) %*% lm_mod$x)
  
  ########## Rank ##########
  rk = length(beta_coef)
  
  output  = list(lm_mod$call, resid_val, coef_res, fitted_val, RSE, df.residual, R2, R2adj, 
                 F.stat3, F.p_val, lm_mod$missing.N, var_cov_mat, rk,resid_val.tb, coef.tb)
  names(output)  = c("call", "residuals", "coefficients", "fitted.values","RSE", "df","r.squared", "adj.r.squared", 
                     "fstatistic","f.pval", "missing.N","cov.unscaled","rank", "resd.tb", "coef.tb")
  
  if(res_display == T){
    cat("Call: ", "\n", output$call, "\n", ' ', "\n", "Residuals: ","\n",sep ="")
    print(output$resid_val.tb)
    cat("\n","Coefficients: ", "\n",sep="")
    print(output$coef.tb)
    cat("---","\n", "Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1","\n",sep ="")
    cat("\n","Residual standard error: ", signif(output$RSE,4), " on ", output$df, " degrees of freedom","\n",sep ="")
    if(output$missing.N>0){
      cat("  (",output$missing.N," observation deleted due to missingness)","\n",sep="")
    }
    
    cat(c("Multiple R-Squared: ", signif(output$r.squared,4), ", Adjusted R-squared: ", signif(output$adj.r.squared,4), "\n",
          "F-Statistic: ", round(output$fstatistic[1],3), " on ", output$fstatistic[2], " and ", output$fstatistic[3],
          " DF, p-value: ", signif(output$f.pval,4)), sep = "")
  }

  return(invisible(output))
}
