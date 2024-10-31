## Tests whether the input checker function works as intended for every input
## (tries multiple types of inputs for each)

### Idea: run check_input_largevar function in each test
### where for each type of input, all other inputs are set
### such that they surely pass the test (the first test evaluates
### this combination of inputs)

test_that("checks running as intended when all inputs correct", {
    # function inputs: function(data,k,r, fin_sample_corr, plot_output, significance_level)
    expect_silent(check_input_largevar(matrix(1:20, nrow=10, ncol=2), 1, 1, FALSE, TRUE, 0.05))
})

### Checks for input data
test_that("data input checker works", {
  # Test for NULL data
  expect_error(
    check_input_largevar(NULL, 1, 1, FALSE, TRUE, 0.05),
    "`data` is a mandatory input"
  )

  # Test for non-numeric matrix
  expect_error(
    check_input_largevar(matrix(letters[1:20], nrow=10, ncol=2), 1, 1, FALSE, TRUE, 0.05),
    "`data` must be a numeric matrix."
  )

  # Test for dimensions being too small
  expect_error(
    check_input_largevar(matrix(1:20, nrow=20, ncol=1), 1, 1, FALSE, TRUE, 0.05),
    "`data` must have at least two time series."
  )

  # Tests for non-matrix table-like input

  ## Dataframe
  expect_error(check_input_largevar(data.frame(Stock1 = c(1, 1.25, 2.3), Stock2 = c(2.23, 3, 3.5)),1, 1, FALSE, TRUE, 0.05),"`data` must be a numeric matrix.")

  ## Data table
  if (requireNamespace("data.table", quietly = TRUE)) {
    library(data.table)
    expect_error(check_input_largevar(data.frame(Stock1 = c(1,1.25,2.3), Stock2 = c(2.23, 3, 3.5)),1, 1, FALSE, TRUE, 0.05),"`data` must be a numeric matrix.")
  }
  ## List
  expect_error(check_input_largevar(c(c(1,2,3),c(4,5,6),c(7,8,9)), 1, 1, FALSE, TRUE, 0.05), "`data` must be a numeric matrix.")

  ## Tibble
  if (requireNamespace("tibble", quietly = TRUE)) {
    library(tibble)
    expect_error(check_input_largevar(tibble(Stock1 = c(1,1.25,2.3), Stock2 = c(2.23, 3, 3.5)),1, 1, FALSE, TRUE, 0.05),"`data` must be a numeric matrix.")
  }

  # Test for some misc other inputs
  expect_error(
    check_input_largevar(3, 1, 1, FALSE, TRUE, 0.05),
    "`data` must be a numeric matrix."
  )

  expect_error(
    check_input_largevar(FALSE, 1, 1, FALSE, TRUE, 0.05),
    "`data` must be a numeric matrix."
  )

  expect_error(
    check_input_largevar('dog', 1, 1, FALSE, TRUE, 0.05),
    "`data` must be a numeric matrix."
  )
})


### Checks for input k
test_that("k input checker works", {
  # non-number errors
  expect_error(
    check_input_largevar(matrix(1:20, nrow=10, ncol=2), "a", 1, FALSE, TRUE, 0.05),
    "`k` must be a number."
  )

  expect_error(
    check_input_largevar(matrix(1:20, nrow=10, ncol=2), FALSE, 1, FALSE, TRUE, 0.05),
    "`k` must be a number."
  )
  expect_error(
    check_input_largevar(matrix(1:20, nrow=10, ncol=2), c(1,2), 1, FALSE, TRUE, 0.05),
    "`k` must be a number."
  )

  #not positive integer errors
  expect_error(
    check_input_largevar(matrix(1:20, nrow=10, ncol=2), 1.5, 1, FALSE, TRUE, 0.05),
    "`k` must be a positive integer."
  )
  expect_error(
    check_input_largevar(matrix(1:20, nrow=10, ncol=2), -3, 1, FALSE, TRUE, 0.05),
    "`k` must be a positive integer."
  )

  # data dimension-related error
  expect_error(
    check_input_largevar(matrix(1:20, nrow=10, ncol=2), 13, 1, FALSE, TRUE, 0.05),
    "`k` too large, check dim requirements"
  )
})



### Checks for input r
test_that("r input checker works", {
  # Test with non-numeric `r` values
  expect_error(
    check_input_largevar(matrix(1:20, nrow=10, ncol=2), 1, "a", FALSE, TRUE, 0.05),
    "`r` must be a number."
  )

  expect_error(
    check_input_largevar(matrix(1:20, nrow=10, ncol=2), 1, FALSE, FALSE, TRUE, 0.05),
    "`r` must be a number."
  )

  expect_error(
    check_input_largevar(matrix(1:20, nrow=10, ncol=2), 1, c(1, 2), FALSE, TRUE, 0.05),
    "`r` must be a number."
  )

  # Test with non-positive integer `r` values
  expect_error(
    check_input_largevar(matrix(1:20, nrow=10, ncol=2), 1, 1.5, FALSE, TRUE, 0.05),
    "`r` must be a positive integer."
  )

  expect_error(
    check_input_largevar(matrix(1:20, nrow=10, ncol=2), 1, -3, FALSE, TRUE, 0.05),
    "`r` must be a positive integer."
  )

  # Test with `r` too large for the given data
  expect_error(
    check_input_largevar(matrix(1:20, nrow=10, ncol=2), 1, 3, FALSE, TRUE, 0.05),
    "`r` must be less than or equal to the number of variables in your dataset."
  )

  # Test with valid `r` within acceptable range
  expect_silent(
    check_input_largevar(matrix(1:20, nrow=10, ncol=2), 1, 1, FALSE, TRUE, 0.05)
  )

  # Test with `r` > 10 to trigger a warning
  expect_warning(
    check_input_largevar(matrix(1:2000, nrow=100, ncol=20), 1, 11, FALSE, TRUE, 0.05),
    "Test statistic percentiles are only available for `r` <= 10."
  )
})


### Checks for input fin_sample_corr
test_that("fin_sample_corr input checker works", {
  # Test with non-boolean `fin_sample_corr` values
  expect_error(
    check_input_largevar(matrix(1:20, nrow=10, ncol=2), 1, 1, "yes", TRUE, 0.05),
    "`fin_sample_corr` must be a boolean."
  )

  expect_error(
    check_input_largevar(matrix(1:20, nrow=10, ncol=2), 1, 1, 1, TRUE, 0.05),
    "`fin_sample_corr` must be a boolean."
  )

  expect_error(
    check_input_largevar(matrix(1:20, nrow=10, ncol=2), 1, 1, c(TRUE, FALSE), TRUE, 0.05),
    "`fin_sample_corr` must be a boolean."
  )

  # Test with valid boolean values
  expect_silent(
    check_input_largevar(matrix(1:20, nrow=10, ncol=2), 1, 1, FALSE, TRUE, 0.05)
  )

  expect_silent(
    check_input_largevar(matrix(1:20, nrow=10, ncol=2), 1, 1, TRUE, TRUE, 0.05)
  )
})


### Checks for input plot_output
test_that("plot_output input checker works", {
  # Test with non-boolean `plot_output` values
  expect_error(
    check_input_largevar(matrix(1:20, nrow=10, ncol=2), 1, 1, FALSE, "yes", 0.05),
    "`plot_output` must be a boolean."
  )

  expect_error(
    check_input_largevar(matrix(1:20, nrow=10, ncol=2), 1, 1, FALSE, 1, 0.05),
    "`plot_output` must be a boolean."
  )

  expect_error(
    check_input_largevar(matrix(1:20, nrow=10, ncol=2), 1, 1, FALSE, c(TRUE, FALSE), 0.05),
    "`plot_output` must be a boolean."
  )

  # Test with valid boolean values
  expect_silent(
    check_input_largevar(matrix(1:20, nrow=10, ncol=2), 1, 1, FALSE, FALSE, 0.05)
  )

  expect_silent(
    check_input_largevar(matrix(1:20, nrow=10, ncol=2), 1, 1, FALSE, TRUE, 0.05)
  )
})



### Checks for input significance_level
test_that("significance_level input checker works", {
  # Test with non-numeric `significance_level` values
  expect_error(
    check_input_largevar(matrix(1:20, nrow=10, ncol=2), 1, 1, FALSE, TRUE, "high"),
    "`significance_level` must be a number."
  )

  expect_error(
    check_input_largevar(matrix(1:20, nrow=10, ncol=2), 1, 1, FALSE, TRUE, c(0.05, 0.1)),
    "`significance_level` must be a number."
  )

  # Test with numeric but out-of-bounds `significance_level` values
  expect_error(
    check_input_largevar(matrix(1:20, nrow=10, ncol=2), 1, 1, FALSE, TRUE, -0.1),
    "`significance_level` must be a real number strictly between 0 and 1."
  )

  expect_error(
    check_input_largevar(matrix(1:20, nrow=10, ncol=2), 1, 1, FALSE, TRUE, 1.1),
    "`significance_level` must be a real number strictly between 0 and 1."
  )

  # Test with valid `significance_level` values
  expect_silent(
    check_input_largevar(matrix(1:20, nrow=10, ncol=2), 1, 1, FALSE, TRUE, 0.05)
  )

  expect_silent(
    check_input_largevar(matrix(1:20, nrow=10, ncol=2), 1, 1, FALSE, TRUE, 0.1)
  )
})
