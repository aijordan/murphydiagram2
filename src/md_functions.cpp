#include <algorithm>
#include <numeric>
#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
List C_md_expect(const NumericVector &x, const NumericVector &y,
                 const double &level, const IntegerVector &ordery) {
  if (x.length() == 0) return 0;
  if (x.length() != y.length() || y.length() != ordery.length()) {
    stop("");
  } 
  const int n = y.length();
  
  // return variables
  long double max = 0.0;
  std::vector<long double> knots;
  std::vector<long double> lefts = {0.0};
  std::vector<long double> rights;
  std::vector<long double> a = {0.0};
  std::vector<long double> b = {0.0};
  knots.reserve(2 * n);
  lefts.reserve(2 * n);
  rights.reserve(2 * n);
  a.reserve(2 * n + 1);
  b.reserve(2 * n + 1);
  
  // loop variables
  long double a_cum = 0.0;
  long double b_cum = 0.0;
  IntegerVector::const_iterator it_ordery = ordery.begin();
  std::vector<int> orderx(n);
  std::iota(orderx.begin(), orderx.end(), 0);
  std::sort(orderx.begin(), orderx.end(), [&](int i, int j) {
    return x[i] < x[j];
  });
  std::vector<int>::const_iterator it_orderx = orderx.begin();
  
  // One-based indexing in ordery (from R)
  knots.push_back(std::min(y[*it_ordery - 1], x[*it_orderx]));
  
  while (it_orderx != orderx.end() || it_ordery != ordery.end()) {
    // choose new knot from x or y
    bool isYchosen = (it_ordery != ordery.end()) &&
      ((it_orderx == orderx.end()) || (y[*it_ordery - 1] < x[*it_orderx]));
    int index = isYchosen ? (*it_ordery++) - 1 : (*it_orderx++);
    long double knot_new = isYchosen ? y[index] : x[index];
    // stash accumulated knot results
    if (knot_new > knots.back()) { 
      long double right = a_cum + b_cum * knots.back();
      long double left = a_cum + b_cum * knot_new;
      knots.push_back(knot_new);
      rights.push_back(right);
      lefts.push_back(left);
      if (right > max) max = right;
      if (left > max) max = left;
      a.push_back(a_cum);
      b.push_back(b_cum);
    }
    /* accumulate knot results
       S_eta(x, y) = ((eta <= x) - (eta <= y)) * V(eta, y)
       V(eta, y) = 4 * abs((y < eta) - level) * (eta - y) */
    bool isYlessthanX = y[index] < x[index];
    long double incr = isYlessthanX ? 1.0 : 0.0;
    incr = 4.0 * (incr - level) / n;
    incr = (isYchosen == isYlessthanX) ? incr : -incr;
    a_cum -= incr * y[index];
    b_cum += incr;
  }
  // stash results for final knot
  rights.push_back(0.0);
  a.push_back(0.0);
  b.push_back(0.0);
  
  return List::create(
    _["x"] = x,
    _["knots"] = knots,
    _["values"] = List::create(
      _["left"] = lefts,
      _["right"] = rights,
      _["max"] = max),
    _["coefs"] = List::create(
      _["a"] = a,
      _["b"] = b)
  );
}


// [[Rcpp::export]]
List C_md_quant(const NumericVector &x, const NumericVector &y,
                const double &level, const IntegerVector &ordery) {
  
  if (x.length() == 0) return 0;
  if ((x.length() != y.length()) || (y.length() != ordery.length())) {
    stop("");
  }
  const int n = y.length();
  
  // return variables
  long double max = 0.0;
  std::vector<long double> knots;
  std::vector<long double> lefts = {0.0};
  std::vector<long double> rights;
  std::vector<long double> a = {0.0};
  knots.reserve(2 * n);
  lefts.reserve(2 * n);
  rights.reserve(2 * n);
  a.reserve(2 * n + 1);
  
  // loop variables
  long double a_cum = 0.0;
  IntegerVector::const_iterator it_ordery = ordery.begin();
  std::vector<int> orderx(n);
  std::iota(orderx.begin(), orderx.end(), 0);
  std::sort(orderx.begin(), orderx.end(), [&](int i, int j) {
    return x[i] < x[j];
  });
  std::vector<int>::const_iterator it_orderx = orderx.begin();
  
  // One-based indexing in ordery (from R)
  knots.push_back(std::min(y[*it_ordery - 1], x[*it_orderx]));
  
  while (it_orderx != orderx.end() || it_ordery != ordery.end()) {
    // choose new knot from x or y
    bool isYchosen = (it_ordery != ordery.end()) &&
      ((it_orderx == orderx.end()) || (y[*it_ordery - 1] < x[*it_orderx]));
    int index = isYchosen ? (*it_ordery++) - 1 : (*it_orderx++);
    long double knot_new = isYchosen ? y[index] : x[index];
    // stash accumulated knot results
    if (knot_new > knots.back()) {
      knots.push_back(knot_new);
      lefts.push_back(a_cum);
      rights.push_back(a_cum);
      if (a_cum > max) max = a_cum;
      a.push_back(a_cum);
    }
    // accumulate knot results
    // S_eta(x, y) = ((eta <= x) - (eta <= y)) * V(eta, y)
    // V(eta, y) = 2 * ((y < eta) - level) */
    bool isYlessthanX = y[index] < x[index];
    long double incr = isYlessthanX ? 1.0 : 0.0;
    incr = 2.0 * std::abs(incr - level) / n;
    a_cum += (isYchosen == isYlessthanX) ? incr : -incr;
  }
  // stash results for final knot
  rights.push_back(0.0);
  a.push_back(0.0);
  
  return List::create(
    _["x"] = x,
    _["knots"] = knots,
    _["values"] = List::create(
      _["left"] = lefts,
      _["right"] = rights,
      _["max"] = max),
    _["coefs"] = List::create(
      _["a"] = a)
  );
}