// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif

// accuracy
double accuracy(NumericVector pre_probs, IntegerVector labels, double threshold);
RcppExport SEXP _GLMhw_accuracy(SEXP pre_probsSEXP, SEXP labelsSEXP, SEXP thresholdSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type pre_probs(pre_probsSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type labels(labelsSEXP);
    Rcpp::traits::input_parameter< double >::type threshold(thresholdSEXP);
    rcpp_result_gen = Rcpp::wrap(accuracy(pre_probs, labels, threshold));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_GLMhw_accuracy", (DL_FUNC) &_GLMhw_accuracy, 3},
    {NULL, NULL, 0}
};

RcppExport void R_init_GLMhw(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
