#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
double accuracy(NumericVector pre_probs, IntegerVector labels, double threshold=0.5) {
    int n = pre_probs.size();
    if (n != labels.size()) {
        stop("The size of pre_probs and labels must be the same.");
    }
    int true_pre=0;
    for (int i=0; i<n;i++){
        int label = pre_probs[i]> threshold ? 1 : 0;
        if (label == labels[i]) {
            true_pre++;
        }
    }
    return static_cast<double>(true_pre)/n;
}
