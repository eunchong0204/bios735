#include <RcppArmadillo.h>
using namespace Rcpp;

// [[Rcpp::export]]
arma::mat armadillo_solve(arma::mat A, arma::vec b){
        return inv(A.t() * A) * A.t() * b;
}
