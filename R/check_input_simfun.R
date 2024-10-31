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
check_input_simfun <- function(N,tau,stat_value,k,r,fin_sample_corr,sim_num){

  if(is.null(N)){ # (default is NULL if nothing is input)
    stop("`N` is a mandatory input")

  }else if((is.numeric(N)==FALSE)|(length(N) == 1)==FALSE){
    stop("`N` must be a number.")

  }else if(((N%%1==0)==FALSE)|((N>0)==FALSE)){
    stop("`N` must be a positive integer.")

  }else if(is.null(tau)){ # (default is NULL if nothing is input)
    stop("`tau` is a mandatory input")

  }else if((is.numeric(tau)==FALSE)|(length(tau) == 1)==FALSE){
    stop("`tau` must be a number.")

  }else if(((tau%%1==0)==FALSE)|((tau>0)==FALSE)){
    stop("`tau` must be a positive integer.")

  }else if(is.null(stat_value)){ # (default is NULL if nothing is input)
    stop("`stat_value` is a mandatory input")

  }else if((is.numeric(stat_value)==FALSE)|(length(stat_value) == 1)==FALSE){
    stop("`stat_value` must be a number.")

  }else if((is.numeric(k)==FALSE)|(length(k) == 1)==FALSE){
    stop("`k` must be a number.")

  }else if(((k%%1==0)==FALSE)|((k>0)==FALSE)){
    stop("`k` must be a positive integer.")

  }else if  ( k >= ((tau-1)/N)-1){
    stop("`k` too large, check dim requirements")


  }else if((is.numeric(r)==FALSE)|(length(r) == 1)==FALSE){
    stop("`r` must be a number.")

  }else if(((r%%1==0)==FALSE)|((r>0)==FALSE)){
    stop("`r` must be a positive integer.")

  }else if  ( r>N){
    stop("`r` must be less than or equal to the number of variables in your dataset.")

  }else if((is.numeric(sim_num)==FALSE)|(length(sim_num) == 1)==FALSE){
    stop("`sim_num` must be a number.")

  }else if(((sim_num%%1==0)==FALSE)|((sim_num>0)==FALSE)){
    stop("`sim_num` must be a positive integer.")

  }else if  ( (isTRUE(fin_sample_corr)==FALSE) & (isFALSE(fin_sample_corr)==FALSE) ){
    stop("`fin_sample_corr` must be a boolean.")

  } else if (sim_num>500){
    warning("Simulation may run for several minutes")
  }
}
