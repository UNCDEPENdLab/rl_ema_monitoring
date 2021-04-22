#include <RcppArmadillo.h>
using namespace Rcpp;
using namespace arma;
using namespace std;

// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export]]

arma::mat timings2samples_cpp(const arma::vec& timings, int HRstep=10) {
  //this version is a more direct port of the MATLAB/R code, but is inefficient because it iterates
  //over each element of times (the sampling grid) rather than filling in the vector in chunks
  double start = std::ceil(timings[0]/HRstep) * HRstep; //consider min(timings) if ever discontiguous

  arma::vec times = regspace(start, HRstep, max(timings));
  int n_times = times.n_elem;
  arma::vec intervals(n_times);
  arma::vec rates(n_times);
  intervals.fill(datum::nan); //default to missing
  rates.fill(datum::nan);

  for (int i=0; i < n_times; i++) {
    uvec ind = find(timings > times(i), 1);
    if (ind.n_elem > 0) {
      intervals(i) = timings(ind(0)) - timings(ind(0)-1);
    }
  }

  rates = 60000/intervals;

  arma::mat out = join_horiz(times, intervals, rates);
  return out;
}

/*** R
timings2samples_cpp(c(100, 504, 900), 10)
*/
