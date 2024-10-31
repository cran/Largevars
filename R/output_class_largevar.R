#' @import methods
#' @noRd
# Define the class for the output list of largevar function
## Source: https://stackoverflow.com/a/15046810


methods::setClass("stat_test", representation("list"))

# Define the custom show method
methods::setMethod("show", "stat_test", function(object) {
  cat("Output for the largevars function", "\n")
  cat("===================================", "\n")
  cat("Cointegration test for high-dimensional VAR(k)                 ","T=", object$t,", N=", object$N,"\n")
  cat("\n")  # Print double dashed line
  if (object$r<=10){
  print(round(object$significance_test$significance_table[object$r,],digits=2))
  cat("\n")
  cat(object$significance_test$`Statistical decision`,"\n")
  cat("============================================================================", "\n")
  }
  cat("Test statistic:", object$statistic ,"\n")
  cat(object$significance_test$text,"\n")
  if (object$r<=10){
  cat("Decision about H0: ", object$significance_test$boolean_decision,"\n")
  }
})
