// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include "fastcpd_types.h"
#include <RcppArmadillo.h>
#include <Rcpp.h>

using namespace Rcpp;

#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif

// fastcpd_impl
List fastcpd_impl(mat data, double beta, const int segment_count, const double trim, const double momentum_coef, Function k, const string family, const double epsilon, const double min_prob, const double winsorise_minval, const double winsorise_maxval, const int p, const colvec order, Nullable<Function> cost, Nullable<Function> cost_gradient, Nullable<Function> cost_hessian, const bool cp_only, const double vanilla_percentage, const bool warm_start, colvec lower, colvec upper, colvec line_search, const mat mean_data_cov, const unsigned int p_response);
RcppExport SEXP _fastcpd_fastcpd_impl(SEXP dataSEXP, SEXP betaSEXP, SEXP segment_countSEXP, SEXP trimSEXP, SEXP momentum_coefSEXP, SEXP kSEXP, SEXP familySEXP, SEXP epsilonSEXP, SEXP min_probSEXP, SEXP winsorise_minvalSEXP, SEXP winsorise_maxvalSEXP, SEXP pSEXP, SEXP orderSEXP, SEXP costSEXP, SEXP cost_gradientSEXP, SEXP cost_hessianSEXP, SEXP cp_onlySEXP, SEXP vanilla_percentageSEXP, SEXP warm_startSEXP, SEXP lowerSEXP, SEXP upperSEXP, SEXP line_searchSEXP, SEXP mean_data_covSEXP, SEXP p_responseSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< mat >::type data(dataSEXP);
    Rcpp::traits::input_parameter< double >::type beta(betaSEXP);
    Rcpp::traits::input_parameter< const int >::type segment_count(segment_countSEXP);
    Rcpp::traits::input_parameter< const double >::type trim(trimSEXP);
    Rcpp::traits::input_parameter< const double >::type momentum_coef(momentum_coefSEXP);
    Rcpp::traits::input_parameter< Function >::type k(kSEXP);
    Rcpp::traits::input_parameter< const string >::type family(familySEXP);
    Rcpp::traits::input_parameter< const double >::type epsilon(epsilonSEXP);
    Rcpp::traits::input_parameter< const double >::type min_prob(min_probSEXP);
    Rcpp::traits::input_parameter< const double >::type winsorise_minval(winsorise_minvalSEXP);
    Rcpp::traits::input_parameter< const double >::type winsorise_maxval(winsorise_maxvalSEXP);
    Rcpp::traits::input_parameter< const int >::type p(pSEXP);
    Rcpp::traits::input_parameter< const colvec >::type order(orderSEXP);
    Rcpp::traits::input_parameter< Nullable<Function> >::type cost(costSEXP);
    Rcpp::traits::input_parameter< Nullable<Function> >::type cost_gradient(cost_gradientSEXP);
    Rcpp::traits::input_parameter< Nullable<Function> >::type cost_hessian(cost_hessianSEXP);
    Rcpp::traits::input_parameter< const bool >::type cp_only(cp_onlySEXP);
    Rcpp::traits::input_parameter< const double >::type vanilla_percentage(vanilla_percentageSEXP);
    Rcpp::traits::input_parameter< const bool >::type warm_start(warm_startSEXP);
    Rcpp::traits::input_parameter< colvec >::type lower(lowerSEXP);
    Rcpp::traits::input_parameter< colvec >::type upper(upperSEXP);
    Rcpp::traits::input_parameter< colvec >::type line_search(line_searchSEXP);
    Rcpp::traits::input_parameter< const mat >::type mean_data_cov(mean_data_covSEXP);
    Rcpp::traits::input_parameter< const unsigned int >::type p_response(p_responseSEXP);
    rcpp_result_gen = Rcpp::wrap(fastcpd_impl(data, beta, segment_count, trim, momentum_coef, k, family, epsilon, min_prob, winsorise_minval, winsorise_maxval, p, order, cost, cost_gradient, cost_hessian, cp_only, vanilla_percentage, warm_start, lower, upper, line_search, mean_data_cov, p_response));
    return rcpp_result_gen;
END_RCPP
}

RcppExport SEXP run_testthat_tests(void *);

static const R_CallMethodDef CallEntries[] = {
    {"_fastcpd_fastcpd_impl", (DL_FUNC) &_fastcpd_fastcpd_impl, 24},
    {"run_testthat_tests", (DL_FUNC) &run_testthat_tests, 1},
    {NULL, NULL, 0}
};

RcppExport void R_init_fastcpd(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
