#include <util.h>

IntegerVector which_positive(NumericVector x) {
  // return indexes of vector elements > 0
  IntegerVector v = seq(0, x.size()-1);
  return v[x > 0];
}

IntegerVector which_equal(IntegerVector x, int y) {
  // return indexes of vector elements == y
  IntegerVector v = seq(0, x.size()-1);
  return v[x==y];
}

double log_sum(double log_a, double log_b) {
  double v;
  
  if (log_a < log_b) {
    v = log_b+log(1 + exp(log_a-log_b));
  }
  else {
    v = log_a+log(1 + exp(log_b-log_a));
  }
  return(v);
}