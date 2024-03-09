test_that("correct total area with upper bound", {
  expect_equal(
    stats::integrate(
      function(x){
        tnorm_pdf(x, 0, 1, .5, -1)
      },
      lower = -Inf,
      upper = -1
    )$value,
  1
  )
})

test_that("correct total area overall", {
  expect_equal(
    stats::integrate(
      function(x){
        tnorm_pdf(x, 0, 1, .5, -1)
      },
      lower = -Inf,
      upper = Inf
    )$value,
    1
  )
})
