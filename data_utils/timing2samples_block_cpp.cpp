#include <RcppArmadillo.h>
using namespace Rcpp;
using namespace arma;
using namespace std;

// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export]]

arma::mat timings2samples_block(const arma::vec& timings, int HRstep=10) {
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
    last = max(find(times < timings[i+1])); //end of this timing run
    //last = find(times < timings[i+1], 1, "last"); //this approach does not seem to be faster and is less intuitive
    intervals.elem(regspace<arma::uvec>(offset, last)).fill(tdiff[i]); //fill in positions with tdiff[i]
    offset = last + 1; //update offset position
  }

  rates = 60000/intervals; //use vectorized calculation

  arma::mat out = join_horiz(times, intervals, rates); //equivalent to cbind
  return out;
}

/*** R
timings2samples_block(c(100, 504, 900), 10)
*/
