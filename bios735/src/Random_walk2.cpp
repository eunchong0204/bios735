#include <RcppArmadillo.h>
using namespace Rcpp;

// [[Rcpp::export]]
List random_walk2(double niter,
                  double lambda){
        NumericVector x (niter);
        NumericVector y (niter);

        for (int i = 1; i < niter; i++){
                x[i] = x[i-1] + lambda * (2.0 * Rf_rbinom(1, 0.5) - 1.0);
        }
        for (int i = 1; i < niter; i++){
                y[i] = y[i-1] + lambda * (2.0 * Rf_rbinom(1, 0.5) - 1.0);
        }
        return List::create(Named("x") = x, Named("y") = y);
}
