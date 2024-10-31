#' @import methods
#' @noRd
# Define the class for the output list of sim_function function
## Source: https://stackoverflow.com/a/15046810


methods::setClass("simfun_output", representation("list"))

# Define the custom show method
methods::setMethod("show", "simfun_output", function(object) {
  cat("Output for the sim_function function", "\n")
  cat("===================================", "\n")
  cat("The empirical p-value is ", object$empirical_percentage , "\n")
  cat("\n")  # Print double dashed line
})
