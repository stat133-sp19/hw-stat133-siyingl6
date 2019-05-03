# Context for binomial
context("Check for binomial")

# bin_choose()
test_that("bin_choose with right type", {
  expect_type(bin_choose(n = 5, k = 2), "double")
  expect_type(bin_choose(5, 0), "double")
  expect_type(bin_choose(5, 1:3), "double")
})

test_that("bin_choose with right class", {
  expect_is(bin_choose(n = 5, k = 2), "numeric")
  expect_is(bin_choose(5, 0), "numeric")
  expect_is(bin_choose(5, 1:3), "numeric")
})

test_that("bin_choose with right length", {
  expect_length(bin_choose(n = 5, k = 2), 1)
  expect_length(bin_choose(5, 0), 1)
  expect_length(bin_choose(5, 1:3), 3)
})

test_that("bin_choose with right answer", {
  expect_equal(bin_choose(n = 5, k = 2), 10)
  expect_equal(bin_choose(5, 0), 1)
  expect_equal(bin_choose(5, 1:3), c(5, 10, 10))
})


# bin_probability()
test_that("bin_probability with right type", {
  expect_type(bin_probability(success = 2, trials = 5, prob = 0.5), "double")
  expect_type(bin_probability(success = 0:2, trials = 5, prob = 0.5), "double")
  expect_type(bin_probability(success = 55, trials = 100, prob = 0.5), "double")
})

test_that("bin_probability with right class", {
  expect_is(bin_probability(success = 2, trials = 5, prob = 0.5), "numeric")
  expect_is(bin_probability(success = 0:2, trials = 5, prob = 0.5), "numeric")
  expect_is(bin_probability(success = 55, trials = 100, prob = 0.5), "numeric")
})

test_that("bin_probability with right length", {
  expect_length(bin_probability(success = 2, trials = 5, prob = 0.5), 1)
  expect_length(bin_probability(success = 0:2, trials = 5, prob = 0.5), 3)
  expect_length(bin_probability(success = 55, trials = 100, prob = 0.5), 1)
})

test_that("bin_probability with right answer", {
  expect_equal(bin_probability(success = 2, trials = 5, prob = 0.5), 0.3125)
  expect_equal(bin_probability(success = 0:2, trials = 5, prob = 0.5), c(0.03125, 0.15625, 0.31250))
  expect_equal(bin_probability(success = c(2, 3, 4), trials = 5, prob = 0.5), c(0.3125, 0.3125, 0.15625))
})

test_that("bin_probability getting error if doesn't pass checker", {
  expect_error(bin_probability(success = 2, trials = 5.2, prob = 0.5), "invalid trials value")
  expect_error(bin_probability(success = 2, trials = 5, prob = -0.5), "p has to be a number between 0 and 1")
  expect_error(bin_probability(success = 6, trials = 5, prob = 0.5), "success cannot be greater than trials")
})


# bin_distribution()
test_that("bin_distribution with right type", {
  expect_type(bin_distribution(trials = 5, prob = 0.5), "list")
  expect_type(bin_distribution(trials = 15, prob = 0.5), "list")
})

test_that("bin_distribution with right class", {
  expect_is(bin_distribution(trials = 5, prob = 0.5), c("bindis", "data.frame"))
  expect_is(bin_distribution(trials = 15, prob = 0.5), c("bindis", "data.frame"))
})

test_that("bin_distribution with right length", {
  expect_length(bin_distribution(trials = 5, prob = 0.5), 2)
  expect_length(bin_distribution(trials = 15, prob = 0.5), 2)
})

test_that("bin_distribution getting error if doesn't pass checker", {
  expect_error(bin_distribution(trials = 5.2, prob = 0.5), "invalid trials value")
  expect_error(bin_distribution(trials = 5, prob = -0.5), "p has to be a number between 0 and 1")
})


# bin_cumulative()
test_that("bin_cumulative with right type", {
  expect_type(bin_cumulative(trials = 5, prob = 0.5), "list")
  expect_type(bin_cumulative(trials = 15, prob = 0.5), "list")
})

test_that("bin_cumulative with right class", {
  expect_is(bin_cumulative(trials = 5, prob = 0.5), c("bincum", "data.frame"))
  expect_is(bin_cumulative(trials = 15, prob = 0.5), c("bincum", "data.frame"))
})

test_that("bin_cumulative with right length", {
  expect_length(bin_cumulative(trials = 5, prob = 0.5), 3)
  expect_length(bin_cumulative(trials = 15, prob = 0.5), 3)
})

test_that("bin_cumulative getting error if doesn't pass checker", {
  expect_error(bin_cumulative(trials = 5.2, prob = 0.5), "invalid trials value")
  expect_error(bin_cumulative(trials = 5, prob = 1.2), "p has to be a number between 0 and 1")
})
