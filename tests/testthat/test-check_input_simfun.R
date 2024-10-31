## Tests whether the input checker function works as intended for every input
## (tries multiple types of inputs for each)

### Idea: run check_input_simfun function in each test
### where for each type of input, all other inputs are set
### such that they surely pass the test (the first test evaluates
### this combination of inputs)


test_that("checks running as intended when all inputs are correct", {
  expect_silent(check_input_simfun(10, 100, 1.5, 1, 1, FALSE, 100))
})


### Checks for input N
test_that("N input checker works", {
  # Test for NULL N
  expect_error(
    check_input_simfun(NULL, 100, 1.5, 1, 1, FALSE, 100),
    "`N` is a mandatory input"
  )

  # Test for non-numeric N
  expect_error(
    check_input_simfun("a", 100, 1.5, 1, 1, FALSE, 100),
    "`N` must be a number."
  )

  expect_error(
    check_input_simfun(FALSE, 100, 1.5, 1, 1, FALSE, 100),
    "`N` must be a number."
  )

  # Test for non-integer N
  expect_error(
    check_input_simfun(1.5, 100, 1.5, 1, 1, FALSE, 100),
    "`N` must be a positive integer."
  )

  expect_error(
    check_input_simfun(-5, 100, 1.5, 1, 1, FALSE, 100),
    "`N` must be a positive integer."
  )
})

### Checks for input tau
test_that("tau input checker works", {
  # Test for NULL tau
  expect_error(
    check_input_simfun(10, NULL, 1.5, 1, 1, FALSE, 100),
    "`tau` is a mandatory input"
  )

  # Test for non-numeric tau
  expect_error(
    check_input_simfun(10, "a", 1.5, 1, 1, FALSE, 100),
    "`tau` must be a number."
  )

  expect_error(
    check_input_simfun(10, FALSE, 1.5, 1, 1, FALSE, 100),
    "`tau` must be a number."
  )

  # Test for non-integer tau
  expect_error(
    check_input_simfun(10, 1.5, 1.5, 1, 1, FALSE, 100),
    "`tau` must be a positive integer."
  )

  expect_error(
    check_input_simfun(10, -10, 1.5, 1, 1, FALSE, 100),
    "`tau` must be a positive integer."
  )
})


### Checks for input stat_value
test_that("stat_value input checker works", {
  # Test for NULL stat_value
  expect_error(
    check_input_simfun(10, 100, NULL, 1, 1, FALSE, 100),
    "`stat_value` is a mandatory input"
  )

  # Test for non-numeric stat_value
  expect_error(
    check_input_simfun(10, 100, "a", 1, 1, FALSE, 100),
    "`stat_value` must be a number."
  )

  expect_error(
    check_input_simfun(10, 100, FALSE, 1, 1, FALSE, 100),
    "`stat_value` must be a number."
  )
})

### Checks for input k
test_that("k input checker works", {
  # Test for non-numeric k
  expect_error(
    check_input_simfun(10, 100, 1.5, "a", 1, FALSE, 100),
    "`k` must be a number."
  )

  expect_error(
    check_input_simfun(10, 100, 1.5, FALSE, 1, FALSE, 100),
    "`k` must be a number."
  )

  # Test for non-positive integer k
  expect_error(
    check_input_simfun(10, 100, 1.5, 1.5, 1, FALSE, 100),
    "`k` must be a positive integer."
  )

  expect_error(
    check_input_simfun(10, 100, 1.5, -1, 1, FALSE, 100),
    "`k` must be a positive integer."
  )

  # Test for k too large
  expect_error(
    check_input_simfun(10, 100, 1.5, 11, 1, FALSE, 100),
    "`k` too large, check dim requirements"
  )
})

### Checks for input r
test_that("r input checker works", {
  # Test for non-numeric r
  expect_error(
    check_input_simfun(10, 100, 1.5, 1, "a", FALSE, 100),
    "`r` must be a number."
  )

  expect_error(
    check_input_simfun(10, 100, 1.5, 1, FALSE, FALSE, 100),
    "`r` must be a number."
  )

  # Test for non-positive integer r
  expect_error(
    check_input_simfun(10, 100, 1.5, 1, 1.5, FALSE, 100),
    "`r` must be a positive integer."
  )

  expect_error(
    check_input_simfun(10, 100, 1.5, 1, -1, FALSE, 100),
    "`r` must be a positive integer."
  )

  # Test for r too large
  expect_error(
    check_input_simfun(10, 100, 1.5, 1, 11, FALSE, 100),
    "`r` must be less than or equal to the number of variables in your dataset."
  )

  # Test for valid r
  expect_silent(
    check_input_simfun(10, 100, 1.5, 1, 1, FALSE, 100)
  )
})

### Checks for input fin_sample_corr
test_that("fin_sample_corr input checker works", {
  # Test for non-boolean fin_sample_corr
  expect_error(
    check_input_simfun(10, 100, 1.5, 1, 1, "yes", 100),
    "`fin_sample_corr` must be a boolean."
  )

  expect_error(
    check_input_simfun(10, 100, 1.5, 1, 1, 1, 100),
    "`fin_sample_corr` must be a boolean."
  )

  expect_error(
    check_input_simfun(10, 100, 1.5, 1, 1, c(TRUE, FALSE), 100),
    "`fin_sample_corr` must be a boolean."
  )

  # Test for valid boolean values
  expect_silent(
    check_input_simfun(10, 100, 1.5, 1, 1, FALSE, 100)
  )

  expect_silent(
    check_input_simfun(10, 100, 1.5, 1, 1, TRUE, 100)
  )
})

### Checks for input sim_num
test_that("sim_num input checker works", {
  # Test for non-numeric sim_num
  expect_error(
    check_input_simfun(10, 100, 1.5, 1, 1, FALSE, "many"),
    "`sim_num` must be a number."
  )

  expect_error(
    check_input_simfun(10, 100, 1.5, 1, 1, FALSE, FALSE),
    "`sim_num` must be a number."
  )

  # Test for non-positive integer sim_num
  expect_error(
    check_input_simfun(10, 100, 1.5, 1, 1, FALSE, 1.5),
    "`sim_num` must be a positive integer."
  )

  expect_error(
    check_input_simfun(10, 100, 1.5, 1, 1, FALSE, -10),
    "`sim_num` must be a positive integer."
  )

  # Test for valid sim_num
  expect_silent(
    check_input_simfun(10, 100, 1.5, 1, 1, FALSE, 100)
  )

  # Test for sim_num > 500 to trigger a warning
  expect_warning(
    check_input_simfun(10, 100, 1.5, 1, 1, FALSE, 600),
    "Simulation may run for several minutes"
  )
})
