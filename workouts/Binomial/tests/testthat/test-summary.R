#Daniel del Carpio
#Testhat for Summaries
#stat133 Workout03

context("Tests for Summary Functions work correctly")


#Aux_mean
test_that("Aux_mean computes correctly",{
  expect_equal(aux_mean(10,0.3),3)
  expect_equal(aux_mean(5,.5),2.5)
})

test_that("Aux_mean returns error correctly",{
  expect_error(aux_mean(4))
  expect_error(aux_mean(12,'the'))
})

#Aux_variance
test_that("aux_variance computes correctly",{
  expect_equal(aux_variance(10,0.3), 2.1)
  expect_equal(length(aux_variance(10, 0.3)),1)
})

test_that("aux_variance returns error correctly",{
  expect_error(aux_variance(10,4))
})

#Aux_mode
test_that("aux_mode computes correctly",{
  expect_equal(aux_mode(10,0.3), 3)
  expect_equal(length(aux_mode(10, 0.3)),1)
})

test_that("aux_mode returns error correctly",{
  expect_error(aux_mode(10,4))
})

#Aux_skewness
test_that("aux_sknewness computes correctly",{
  expect_equal(round(aux_skewness(10,0.3), 3), 0.276)
  expect_equal(length(aux_skewness(10, 0.3)), 1)
})

test_that("aux_skewness returns error correctly",{
  expect_error(aux_skewness(10,4))
})

#Aux_Kurtosis
test_that("aux_kurtosis computes correctly",{
  expect_equal(round(aux_kurtosis(10,0.3), 3), -0.124)
  expect_equal(length(aux_kurtosis(10, 0.3)), 1)
})

test_that("aux_kurtosis returns error correctly",{
  expect_error(aux_kurtosis(10,4))
})


