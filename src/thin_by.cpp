#include <algorithm>
#include <numeric>
#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
std::vector<int> C_thin_by(const NumericVector &x, const NumericVector &y,
               const double &xtol, const double &ytol) {
  if (x.length() == 0) return std::vector<int> {};
  if (x.length() != y.length()) {
    stop("");
  }
  const int n = y.length();
  
  // return variable
  std::vector<int> index = {1};
  if (n == 1) return index;
  
  // loop variables
  NumericVector::const_iterator it_x = x.begin();
  NumericVector::const_iterator it_y = y.begin();
  long double x_current = 0.0;
  long double y_current = 0.0;
  
  for (int i = 2; i <= n; ++i) {
    ++it_x;
    ++it_y;
    if (std::abs(*it_y) < 1e-8 || 
          std::abs(*it_x - x_current) > xtol ||
          std::abs(*it_y - y_current) > ytol) {
      if (index.back() != (i - 1)) index.push_back(i - 1);
      index.push_back(i);
      x_current = *it_x;
      y_current = *it_y;
    }
  }
  
  return index;
}
