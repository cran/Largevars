## We have example data in the package. Since largevar is a complex function,
## we might want to change the internal working of it through developments,
## so the best test we can do right now is to see that the function gives
## certain values for the same data (our example).

library(testthat)
library(Largevars)

# Load the expected results
expected_result <- readRDS(test_path("testdata", "expected_largevar_result.rds"))

library(readr)
s_p100_price <- read_csv(test_path("testdata", "s_p100_price_adj.csv"),show_col_types = FALSE)
dataSP <- log(s_p100_price[, seq(2, dim(s_p100_price)[2])])
dataSP <- as.matrix(dataSP)

test_that("largevar produces expected results", {
  result <- largevar(data = dataSP, k = 1, r = 1, fin_sample_corr = FALSE,
                     plot_output = FALSE, significance_level = 0.05)

  expect_equal(result$statistic, expected_result$statistic, tolerance = 1e-4)
  expect_equal(result$significance_test$p_value, expected_result$p_value, tolerance = 1e-4)
  expect_equal(result$significance_test$boolean_decision, expected_result$boolean_decision)
  expect_equal(result$measure_upper_bound, expected_result$measure_upper_bound, tolerance = 1e-4)
  expect_equal(result$measure_lower_bound, expected_result$measure_lower_bound, tolerance = 1e-4)
  expect_equal(result$eigenvalues, expected_result$eigenvalues, tolerance = 1e-4)
})
