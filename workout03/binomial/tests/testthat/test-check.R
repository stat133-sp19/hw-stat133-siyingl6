# Context for checkers
context("Check checker arguments")

# check_prob()
test_that("check_prob with valid number", {
  expect_true(check_prob(0))
  expect_true(check_prob(0.25))
  expect_true(check_prob(0.755))
  expect_true(check_prob(c(1)))
})

test_that("check_prob with invalid number", {
  expect_error(check_prob(1.2))
  expect_error(check_prob(-0.1))

})

test_that("check_prob with invalid length", {
  expect_error(check_prob(c(0,1)))
  expect_error(check_prob(c(0.25, 0.755)))
})

test_that("check_prob getting error with invalid prob", {
  expect_error(check_prob(1:5), "invalid prob value")
  expect_error(check_prob(-0.1), "p has to be a number between 0 and 1")
})


# check_trials()
test_that("check_trials with valid number", {
  expect_true(check_trials(1))
  expect_true(check_trials(10))
})

test_that("check_trials with invalid number", {
  expect_error(check_trials(3.2))
  expect_error(check_trials(-2))

})

test_that("check_trials with invalid length", {
  expect_error(check_trials(c(0, 1)))
  expect_error(check_trials(c(0.25, 0.755)))
})

test_that("check_trials getting error with invalid trials number", {
  expect_error(check_trials(-0.1), "invalid trials value")
  expect_error(check_trials(1:5), "invalid trials value")
})


# check_success()
test_that("check_success with valid success", {
  expect_true(check_success(2, 2))
  expect_true(check_success(2, 5))
  expect_true(check_success(c(2,3,4), 5))
})

test_that("check_success with invalid success number", {
  expect_error(check_success(3.2, 4))
  expect_error(check_success(3, 2))
  expect_error(check_success(c(2,3,4),3))
})

test_that("check_success with invalid success vector", {
  expect_error(check_success(c(2,3,4),3))
  expect_error(check_success(c(2,3,4.5),5))
})

test_that("check_success getting error with invalid success number or vector", {
  expect_error(check_success(3.2, 4),"invalid success value")
  expect_error(check_success(3, 2), "success cannot be greater than trials")
  expect_error(check_success(c(2,3,4),3), "success cannot be greater than trials")
  expect_error(check_success(c(2,3,4),3), "success cannot be greater than trials")
  expect_error(check_success(c(2,3,4.5),5), "invalid success value")
})
