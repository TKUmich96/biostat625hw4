#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]
//' CPP_CalValue
//' 
//' Use RCPPArmadillo to speed up calculation for 
//' inside function, do not need to export
//' @param x matrix of independent variables
//' @param y matrix of dependent variables
//' @return beta a vector of estimated coefficients
//' @return fitted_val a vector of estimated fitted value
//' @return resid_val a vector of estimated residuals
//' @return xtx a matrix calculate (X^T*X)^(-1)
//' @return df a double value represents degree of freedom
//' @useDynLib lm2, Summary_lm2
//' @export
// [[Rcpp::export]]
Rcpp::List get_Cal_val(arma::mat x, arma::mat y) {
  // get matrix transpose
  arma::mat xt = x.t();
  // get matrix inverse
  arma::mat xtx = (xt*x).i();
  // get beta by formula (X^T*X)^(-1)*(X^T*Y)
  arma::vec beta = xtx * xt * y;
  arma::vec fitted_val = x * beta;
  arma::vec resid_val = y - fitted_val;
  
  double df = x.n_rows - x.n_cols;
  
  return Rcpp::List::create(Rcpp::Named("beta")=beta,
                            Rcpp::Named("fitted_val")=fitted_val,
                            Rcpp::Named("resid_val")=resid_val,
                            Rcpp::Named("xtx")=xtx,
                            Rcpp::Named("df")=df);
}
