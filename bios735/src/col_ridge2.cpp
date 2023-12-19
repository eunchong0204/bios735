#include <RcppArmadillo.h>
using namespace Rcpp;

// [[Rcpp::export]]
arma::mat col_ridge2(arma::mat Y, arma::mat X, arma::vec lambda){
        arma::mat I = eye(size(X.t()*X));
        arma::mat beta = arma::mat(arma::as_scalar(X.n_cols),
                                   arma::as_scalar(Y.n_cols));

        for (int i = 0; i < 100; i++){
                beta.col(i) =  inv(X.t() * X + arma::as_scalar(lambda(i)) * I) * X.t() * Y.col(i);
        }

        return beta;
}
