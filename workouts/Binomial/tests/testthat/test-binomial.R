#Daniel del Carpio
#Testhat for Binomial Functions
#stat133 Workout03

context("Tests for Binomial Functions")


#bin_choose()
test_that("bin_choose computes correctly",{
  expect_equal(bin_choose(5,2),10)
  expect_equal(bin_choose(12,1),12)
})

test_that("bin_choose returns error correctly",{
  expect_error(bin_choose(1,2))
  expect_error(bin_choose(12,'1'))
})

#bin_probability
test_that("bin_probabilty computes correctly",{
  expect_equal(bin_probability(2,5,0.5),0.3125)
  expect_equal(bin_probability(55,100,.45),.01075277)
})

test_that("bin_probability returns error correctly",{
  expect_error(bin_probability(1,2))
  expect_error(bin_probability(2,4,2))
})

#bin_distribution
test_that("bin_distribution creates correct dataframe",{
  dis1 <- bin_distribution(5, 0.5)
  expect_equal(nrow(dis1), 6)
  expect_equal(length(dis1), 2)
  expect_equal(dis1$success, 0:5)
  expect_equal(dis1$probability,
               c(0.03125, 0.15625, 0.31250, 0.31250, 0.15625, 0.03125))
})


#bin_cumulative
test_that("bin_cumulative creates correct dataframe",{
  dis2 <- bin_distribution(5, 0.5)
  expect_equal(nrow(dis2), 6)
  expect_equal(length(dis2), 2)
  expect_equal(dis2$success, 0:5)
  expect_equal(dis2$probability,
               c(0.03125, 0.15625, 0.31250, 0.31250, 0.15625, 0.03125))
  expect_equal(dis2$cumulative,
               c(0.03125,0.18750,0.50000,0.81250,0.96875,1.00000))
})
