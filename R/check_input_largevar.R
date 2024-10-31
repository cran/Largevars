#' Input checker for largevar function
#'
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
check_input_largevar <- function(data,k,r, fin_sample_corr, plot_output, significance_level){

  if(is.null(data)){ # data must be existing input (default is NULL if nothing is input)
    stop("`data` is a mandatory input")

  }else if((is.matrix(data)==FALSE)|(is.numeric(data)==FALSE)){
    stop("`data` must be a numeric matrix.")

  }else if(dim(data)[2] < 2){
    stop("`data` must have at least two time series.")

  }else if((is.numeric(k)==FALSE)|(length(k) == 1)==FALSE){
    stop("`k` must be a number.")

  }else if(((k%%1==0)==FALSE)|((k>0)==FALSE)){
    stop("`k` must be a positive integer.")

  }else if  (k >= ((dim(data)[1]-1)/dim(data)[2])-1){
    stop("`k` too large, check dim requirements")

  }else if((is.numeric(r)==FALSE)|(length(r) == 1)==FALSE){
    stop("`r` must be a number.")

  }else if(((r%%1==0)==FALSE)|((r>0)==FALSE)){
    stop("`r` must be a positive integer.")

  }else if  ( r>dim(data)[2] ){
    stop("`r` must be less than or equal to the number of variables in your dataset.")

  }else if((is.numeric(significance_level)==FALSE)|(length(significance_level) == 1)==FALSE){
    stop("`significance_level` must be a number.")

  }else if( (significance_level<0)|(significance_level>1) ){
    stop("`significance_level` must be a real number strictly between 0 and 1.")

  }else if  ( (isTRUE(plot_output)==FALSE) & (isFALSE(plot_output)==FALSE) ){
    stop("`plot_output` must be a boolean.")

  }else if  ( (isTRUE(fin_sample_corr)==FALSE) & (isFALSE(fin_sample_corr)==FALSE) ){
    stop("`fin_sample_corr` must be a boolean.")

  } else if (r>10){
    warning("Test statistic percentiles are only available for `r` <= 10.")
  }
}
