

// [[Rcpp::depends("RcppArmadillo")]]
#include <RcppArmadillo.h>
using namespace Rcpp;
using namespace arma;



// [[Rcpp::export]]
mat solvecpp(const mat& A){
  return(pinv(A, 1.0e-25));}




// [[Rcpp::export]]
double maxeigencpp( const mat& X) {
  return(max(eig_sym( X )));
}


// [[Rcpp::export]]
double normcpp(const vec& x, const mat& A, bool euc=true) {
  if (euc == true)  {return(norm(x,2));}
  else {return(as_scalar(sqrt(trans(x) *A* x) ));}
}