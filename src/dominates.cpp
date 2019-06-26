#include <algorithm>
#include <numeric>
#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
bool C_dominates_expect(const List &x, const List &y, const double tol) {
  const List &x_values = as<List>(x["values"]);
  const List &y_values = as<List>(y["values"]);
  const double x_max = as<double>(x_values["max"]);
  const double y_max = as<double>(y_values["max"]);
  if (x_max > y_max) return FALSE;
  
  const List &x_coefs = as<List>(x["coefs"]);
  const List &y_coefs = as<List>(y["coefs"]);
  
  const NumericVector &x_knots = as<NumericVector>(x["knots"]);
  const NumericVector &x_left = as<NumericVector>(x_values["left"]);
  const NumericVector &x_right = as<NumericVector>(x_values["right"]);
  const NumericVector &x_a = as<NumericVector>(x_coefs["a"]);
  const NumericVector &x_b = as<NumericVector>(x_coefs["b"]);
  const NumericVector &y_knots = as<NumericVector>(y["knots"]);
  const NumericVector &y_left = as<NumericVector>(y_values["left"]);
  const NumericVector &y_right = as<NumericVector>(y_values["right"]);
  const NumericVector &y_a = as<NumericVector>(y_coefs["a"]);
  const NumericVector &y_b = as<NumericVector>(y_coefs["b"]);
  
  const int x_n = x_knots.length();
  const int y_n = y_knots.length();

  int x_pos = 0;
  int y_pos = 0;
  while(x_pos < x_n && y_pos < y_n) {
    bool violated;
    if (x_knots[x_pos] < y_knots[y_pos]) {
      double y_val = y_a[y_pos] + y_b[y_pos] * x_knots[x_pos];
      violated = (x_left[x_pos] - y_val > tol) ||
        (x_right[x_pos] - y_val > tol);
      ++x_pos;
    } else if (y_knots[y_pos] < x_knots[x_pos]) {
      double x_val = x_a[x_pos] + x_b[x_pos] * y_knots[y_pos];
      violated = (x_val - y_left[y_pos] > tol) ||
        (x_val - y_right[y_pos] > tol);
      ++y_pos;
    } else {
      violated = (x_left[x_pos] - y_left[y_pos] > tol) ||
        (x_right[x_pos] - y_right[y_pos] > tol);
      ++x_pos;
      ++y_pos;
    }
    if (violated) return FALSE;
  }

  return TRUE;
}


// [[Rcpp::export]]
bool C_dominates_quant(const List &x, const List &y, const double tol) {
  const List &x_values = as<List>(x["values"]);
  const List &y_values = as<List>(y["values"]);
  const double x_max = as<double>(x_values["max"]);
  const double y_max = as<double>(y_values["max"]);
  if (x_max > y_max) return FALSE;
  
  const NumericVector &x_knots = as<NumericVector>(x["knots"]);
  const NumericVector &x_left = as<NumericVector>(x_values["left"]);
  const NumericVector &x_right = as<NumericVector>(x_values["right"]);
  const NumericVector &y_knots = as<NumericVector>(y["knots"]);
  const NumericVector &y_left = as<NumericVector>(y_values["left"]);
  const NumericVector &y_right = as<NumericVector>(y_values["right"]);
  
  const int x_n = x_knots.length();
  const int y_n = y_knots.length();
  
  int x_pos = 0;
  int y_pos = 0;
  while(x_pos < x_n && y_pos < y_n) {
    bool violated;
    if (x_knots[x_pos] < y_knots[y_pos]) {
      violated = (x_left[x_pos] - y_left[y_pos] > tol) ||
        (x_right[x_pos] - y_left[y_pos] > tol);
      ++x_pos;
    } else if (y_knots[y_pos] < x_knots[x_pos]) {
      violated = (x_left[x_pos] - y_left[y_pos] > tol) ||
        (x_left[x_pos] - y_right[y_pos] > tol);
      ++y_pos;
    } else {
      violated = (x_left[x_pos] - y_left[y_pos] > tol) ||
        (x_right[x_pos] - y_right[y_pos] > tol);
      ++x_pos;
      ++y_pos;
    }
    if (violated) return FALSE;
  }
  
  return TRUE;
}
