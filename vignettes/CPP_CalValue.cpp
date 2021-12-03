#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]
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
