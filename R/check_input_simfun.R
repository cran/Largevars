#' @title Input checker for simfun function
#' @description
#' This is an internal function that checks the validity of the inputs of the sim function function.
#'
#' @param N a number representing the number of time series we want to simulate in the system
#' @param tau a number representing the length of the time series we want to simulate in the system
#' @param stat_value the test statistic value we want to calculate p-value based on
#' @param k The number of lags we wish to employ in the VECM form (default: k=1)
#' @param r The number of cointegrating relationships we impose on the H1 hypothesis (default: r=1)
#' @param sim_num The number of simulation we wish to run.
#' @returns Nothing (or warning message) if all inputs are correct, and an error message otherwise.
#' @keywords internal
check_input_simfun <- function(N, tau, stat_value, k, r, fin_sample_corr, sim_num, seed){

  # more obvious input errors
  if (is.null(N) || !is.numeric(N) || length(N) != 1 || N %% 1 != 0 || N <= 0) {
    stop("`N` must be a single positive integer.")
  }

  if (is.null(tau) || !is.numeric(tau) || length(tau) != 1 ||
      tau %% 1 != 0 || tau <= 0) {
    stop("`tau` must be a single positive integer.")
  }

  if (is.null(stat_value) || !is.numeric(stat_value) || length(stat_value) != 1) {
    stop("`stat_value` must be a single numeric value.")
  }

  if (!is.numeric(k) || length(k) != 1 || k %% 1 != 0 || k <= 0) {
    stop("`k` must be a single positive integer.")
  }

  if (!is.numeric(r) || length(r) != 1 || r %% 1 != 0 || r <= 0) {
    stop("`r` must be a single positive integer.")
  }

  if (!is.numeric(sim_num) || length(sim_num) != 1 ||
      sim_num %% 1 != 0 || sim_num <= 0) {
    stop("`sim_num` must be a single positive integer.")
  }

  if (!is.logical(fin_sample_corr) || length(fin_sample_corr) != 1) {
    stop("`fin_sample_corr` must be a single boolean value (TRUE or FALSE).")
  }


  if (!is.null(seed) && (!is.numeric(seed) || length(seed) != 1 ||
                         seed %% 1 != 0 || seed < 0)) {
    stop("`seed` must be a single non-negative integer if given.")
  }


  # less obvious errors
  if  ( k >= ((tau - 1)/N) - 1){
    stop("`k` too large, check dim requirements")
  }

  if  ( r > N){
    stop("`r` must be less than or equal to the number of variables in your dataset.")
  }

  if (sim_num > 500){
    warning("Simulation may run for several minutes")
  }
}
