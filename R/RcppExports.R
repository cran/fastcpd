# Generated by using Rcpp::compileAttributes() -> do not edit by hand
# Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

fastcpd_impl <- function(data, beta, segment_count, trim, momentum_coef, k, family, epsilon, min_prob, winsorise_minval, winsorise_maxval, p, order, cost, cost_gradient, cost_hessian, cp_only, vanilla_percentage, warm_start, lower, upper, line_search, mean_data_cov, p_response, r_progress) {
    .Call(`_fastcpd_fastcpd_impl`, data, beta, segment_count, trim, momentum_coef, k, family, epsilon, min_prob, winsorise_minval, winsorise_maxval, p, order, cost, cost_gradient, cost_hessian, cp_only, vanilla_percentage, warm_start, lower, upper, line_search, mean_data_cov, p_response, r_progress)
}

