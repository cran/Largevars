#' Input checker for largevar function
#' @description
#' This is an internal function that checks the validity of the inputs of the largevar function.
#'
#' @param data a numeric matrix where columns contain the individual time series that will be examined for presence of cointegrating relationships
#' @param k The number of lags we wish to employ in the VECM form (default: k=1)
#' @param r The number of cointegrating relationships we impose on the H1 hypothesis (default: r=1)
#' @param fin_sample_corr A boolean variable indicating whether we wish to employ finite sample correction on our test statistic. Default is false
#' @param plot_output A boolean variable indicating whether we wish to generate the distribution of the eigenvalues (default: TRUE)
#' @param significance_level Specify the significance level at which the decision about the H0 should be made. For r=1 this can be any level of significance. For r=2 and r=3, the significance level input will be rounded up to the nearest of the following: 0.1, 0.05, 0.025, 0.01. If the significance level is larger than 0.1, the decision will be made at the 10\% level. For r>3 only the test statistic is returned. For an empirical p-value for r>3 use the sim_function fun. in the package.
#' @returns Nothing (or warning message) if all inputs are correct, and an error message otherwise.
#' @keywords internal
check_input_largevar <- function(data, k, r, fin_sample_corr, plot_output, significance_level){

  # more obvious input errors
  if (is.null(data) || !is.matrix(data) || !is.numeric(data) || ncol(data) < 2) {
    stop("`data` must be a numeric matrix with at least two columns (time series).")
  }

  if (is.null(k) || !is.numeric(k) || length(k) != 1 || k %% 1 != 0 || k <= 0) {
    stop("`k` must be a single positive integer.")
  }

  if (is.null(r) || !is.numeric(r) || length(r) != 1 || r %% 1 != 0 || r <= 0) {
    stop("`r` must be a single positive integer.")
  }

  if (is.null(significance_level) || !is.numeric(significance_level) ||
      length(significance_level) != 1 || significance_level <= 0 ||
      significance_level >= 1) {
    stop("`significance_level` must be a single numeric value strictly between 0 and 1.")
  }

  if (is.null(plot_output) || !is.logical(plot_output) || length(plot_output) != 1) {
    stop("`plot_output` must be a single boolean value (TRUE or FALSE).")
  }

  if (is.null(fin_sample_corr) || !is.logical(fin_sample_corr) || length(fin_sample_corr) != 1) {
    stop("`fin_sample_corr` must be a single boolean value (TRUE or FALSE).")
  }


  # less obvious errors
  if (k >= ((nrow(data) - 1) / ncol(data)) - 1) {
    stop("`k` too large, check dim requirements")
  }

  if (r > ncol(data)) {
    stop("`r` must be less than or equal to the number of variables in your dataset.")
  }

  if (significance_level <= 0 || significance_level >= 1) {
    stop("`significance_level` must be a real number strictly between 0 and 1.")
  }

  if (r > 10) {
    warning("Test statistic percentiles are only available for `r` <= 10.")
  }
}
