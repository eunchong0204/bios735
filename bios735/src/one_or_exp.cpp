#include <RcppArmadillo.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector one_or_exp(NumericVector x){
        NumericVector y = ifelse(x < 0.0, 1.0, exp(x));
        return y;
}
