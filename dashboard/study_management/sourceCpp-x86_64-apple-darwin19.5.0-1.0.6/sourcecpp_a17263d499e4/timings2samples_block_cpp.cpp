#include <RcppArmadillo.h>
using namespace Rcpp;
using namespace arma;
using namespace std;

// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export]]

arma::mat timings2samples_block_cpp(const arma::vec& timings, int HRstep=10) {
  double start = std::ceil(timings[0]/HRstep) * HRstep; //consider min(timings) if ever discontiguous
  arma::vec times = regspace(start, HRstep, max(timings));
  int n_times = times.n_elem;
  arma::vec intervals(n_times); //preallocate intervals and rates
  arma::vec rates(n_times);
  intervals.fill(datum::nan); //default to missing
  rates.fill(datum::nan);

  //uvec last(1); //only for attempt at the find last approach
  //cout << "n_times" << n_times << ", n_timings: " << timings.n_elem << endl;

  int offset = 0; //block offset
  int last = 0;
  arma::vec tdiff = diff(timings); //differences in timings are what we fill in intervals

  for (int i=0; i < timings.n_elem - 1; i++) {
    arma::uvec next_times = find(times < timings[i+1]);
    if (next_times.is_empty()) { continue; } //skip out if there are no future times
    last = max(next_times); //end of this timing run

    //last = find(times < timings[i+1], 1, "last"); //this approach does not seem to be faster and is less intuitive
    intervals.elem(regspace<arma::uvec>(offset, last)).fill(tdiff[i]); //fill in positions with tdiff[i]
    offset = last + 1; //update offset position
  }

  rates = 60000/intervals; //use vectorized calculation

  arma::mat out = join_horiz(times, intervals, rates); //equivalent to cbind
  return out;
}

/*** R
#uncomment to test on simple case
#timings2samples_block_cpp(c(100, 504, 900), 10)
*/


#include <Rcpp.h>
// timings2samples_block_cpp
arma::mat timings2samples_block_cpp(const arma::vec& timings, int HRstep);
RcppExport SEXP sourceCpp_1_timings2samples_block_cpp(SEXP timingsSEXP, SEXP HRstepSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const arma::vec& >::type timings(timingsSEXP);
    Rcpp::traits::input_parameter< int >::type HRstep(HRstepSEXP);
    rcpp_result_gen = Rcpp::wrap(timings2samples_block_cpp(timings, HRstep));
    return rcpp_result_gen;
END_RCPP
}
