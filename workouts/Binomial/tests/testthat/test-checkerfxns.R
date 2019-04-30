#Daniel del Carpio
#Testhat for Checker Functions
#stat133 Workout03

context("Tests for checker functions")


#check_prob
test_that("test check_prob returns true with valid inputs",{
  expect_true(check_prob(1) >= 0 & check_prob(1) <= 1)
  expect_true(check_prob(0.6))

})

test_that("test check_prob returns error with neg/not 0-1",{
  expect_error(check_prob(2))
  expect_error(check_prob(-2))

})

#check_trials
test_that("test check trials returns error with invalid inputs",{
  expect_error(check_trials(-2))
  expect_error(check_trails(1:3))

})

test_that("test check_trials returns true with valid inputs",{
  expect_true(check_trials(2))
  expect_true(length(check_trials(2))==1)
})

#check_success
test_that("test check_success returns error with invalid inputs"){
  expect_error(check_success(c(1, -2, 3), 5))
  expect_error(check_success(c(1, 2, 3), 3))
}

test_that("test check_success returns true with valid inputs"){
  expect_true(check_success(c(1,2,3), 4))
  expect_true(length(check_success(c(1,2,3), 4))==1)
}
