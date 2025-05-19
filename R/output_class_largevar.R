#' @import methods
#' @noRd
# Define the class for the output list of largevar function
## Source: https://stackoverflow.com/a/15046810


methods::setClass("stat_test", representation("list"))

# Define the custom show method
methods::setMethod("show", "stat_test", function(object) {
  cat("Output for the largevar function", "\n")
  cat("=================================", "\n")
  cat("Cointegration test for high-dimensional VAR(k)  ",
      "T=", object$function_inputs$t,
      "N=", object$function_inputs$N,"\n")

  cat("\n")  # Print double dashed line
  if (object$function_inputs$r <= 10) {
    print(round(object$significance_test$significance_row, digits = 2))
    cat("\n")
    cat("If the test statistic is larger than the quantile, reject H0. \n")
    cat("===============================================================", "\n")
  }
  cat("Test statistic:", object$statistic ,"\n")
  cat("The p-value is ", object$significance_test$p_value,"\n")

  if (object$function_inputs$r <= 10) {
    cat("Decision about H0: ", object$significance_test$boolean_decision,"\n")
  }
})
