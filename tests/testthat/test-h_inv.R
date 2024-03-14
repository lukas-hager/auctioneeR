test_that("gives the right constant", {
  expect_equal(h_inv(2,2,2,10,2),2)
})

test_that("inverse works with more than two bidders", {
  expect_equal((h_inv(5,4,3,10,2)*(10-1) + 3 + 2*4)/(2+10), 5)
})

test_that("inverse works with two bidders", {
  expect_equal((h_inv(5,4,3,2,2) + 3 + 2*4)/(2+2), 5)
})
