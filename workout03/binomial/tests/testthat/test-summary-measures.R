# Context for summary measures
context("Check summary measures")

# aux_mean()
test_that("aux_mean with right type", {
  expect_type(aux_mean(1, 0.5), "double")
  expect_type(aux_mean(3, 0.95), "double")
})

test_that("aux_mean with right class", {
  expect_is(aux_mean(1, 0.5), "numeric")
  expect_is(aux_mean(3, 0.95), "numeric")
})

test_that("aux_mean with right length", {
  expect_length(aux_mean(1, 0.5), 1)
  expect_length(aux_mean(3, 0.95), 1)
})

test_that("aux_mean with right answer", {
  expect_equal(aux_mean(1, 0.5), 0.5)
  expect_equal(aux_mean(3, 0.95), 2.85)
})


# aux_variance()
test_that("aux_variance with right type", {
  expect_type(aux_variance(1, 0.5), "double")
  expect_type(aux_variance(3, 0.95), "double")
})

test_that("aux_variance with right class", {
  expect_is(aux_variance(1, 0.5), "numeric")
  expect_is(aux_variance(3, 0.95), "numeric")
})

test_that("aux_variance with right length", {
  expect_length(aux_variance(1, 0.5), 1)
  expect_length(aux_variance(3, 0.95), 1)
})

test_that("aux_variance with right answer", {
  expect_equal(aux_variance(1, 0.5), 0.25)
  expect_equal(aux_variance(3, 0.95), 0.1425)
})


# aux_mode()
test_that("aux_mode with right type", {
  expect_type(aux_mode(1, 0.5), "double")
  expect_type(aux_mode(3, 0.95), "integer")
})

test_that("aux_mode with right class", {
  expect_is(aux_mode(1, 0.5), "numeric")
  expect_is(aux_mode(3, 0.95), "integer")
})

test_that("aux_mode with right length", {
  expect_length(aux_mode(1, 0.5), 2)
  expect_length(aux_mode(3, 0.95), 1)
})

test_that("aux_mode with right answer", {
  expect_equal(aux_mode(1, 0.5), c(1, 0))
  expect_equal(aux_mode(3, 0.95), 3)
})


# aux_skewness()
test_that("aux_skewness with right type", {
  expect_type(aux_skewness(1, 0.5), "double")
  expect_type(aux_skewness(3, 0.95), "double")
})

test_that("aux_skewness with right class", {
  expect_is(aux_skewness(1, 0.5), "numeric")
  expect_is(aux_skewness(3, 0.95), "numeric")
})

test_that("aux_skewness with right length", {
  expect_length(aux_skewness(1, 0.5), 1)
  expect_length(aux_skewness(3, 0.95), 1)
})

test_that("aux_skewness with right answer", {
  expect_equal(aux_skewness(1, 0.5), 0)
  expect_equal(aux_skewness(3, 1), -Inf)
})


# aux_kurtosis()
test_that("aux_kurtosis with right type", {
  expect_type(aux_kurtosis(1, 0.5), "double")
  expect_type(aux_kurtosis(3, 0.95), "double")
})

test_that("aux_kurtosis with right class", {
  expect_is(aux_kurtosis(1, 0.5), "numeric")
  expect_is(aux_kurtosis(3, 0.95), "numeric")
})

test_that("aux_kurtosis with right length", {
  expect_length(aux_kurtosis(1, 0.5), 1)
  expect_length(aux_kurtosis(3, 0.95), 1)
})

test_that("aux_kurtosis with right answer", {
  expect_equal(aux_kurtosis(1, 0.5), -2)
  expect_equal(aux_kurtosis(3, 1), Inf)
})

