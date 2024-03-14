test_that("bid linearity trick works, no default, bajari spec", {
  my_k <- .3
  my_mu <- 1
  my_sigma <- .6

  my_mats <- bid_mats(seq(-3,3,length.out=50), seq(99,101,length.out=2), c(2:30), my_k, 0, 1)
  bids_mat <- bid_vec(
    my_mu,
    my_sigma,
    seq(-3,3,length.out=50),
    seq(99,101,length.out=2),
    100,
    3,
    30,
    my_k,
    my_mats
  )

  bids_flat <- sapply(my_sigma*seq(-3,3,length.out=50)+my_mu, function(x){
    all_bids <- sapply(c(2:30), function(n){
      bid(x,n,100,my_k,my_mu,my_sigma)
    })
    p_n <- dpois(c(2:30), 3)
    f_y <- sapply(c(2:30), function(n){f_y_y(x, my_mu, my_sigma,n, my_k)})
    sum(all_bids * p_n * f_y) / sum(p_n*f_y)
  })

  expect_equal(bids_mat, bids_flat, tolerance = .001)
})


test_that("bid linearity trick works, no default, big spec", {
  my_k <- .8
  my_mu <- 1000
  my_sigma <- 10

  my_mats <- bid_mats(seq(-3,3,length.out=50), seq(99,101,length.out=2), c(2:30), my_k, 0, 1)
  bids_mat <- bid_vec(
    my_mu,
    my_sigma,
    seq(-3,3,length.out=50),
    seq(99,101,length.out=2),
    100,
    3,
    30,
    my_k,
    my_mats
  )

  bids_flat <- sapply(my_sigma*seq(-3,3,length.out=50)+my_mu, function(x){
    all_bids <- sapply(c(2:30), function(n){
      bid(x,n,100,my_k,my_mu,my_sigma)
    })
    p_n <- dpois(c(2:30), 3)
    f_y <- sapply(c(2:30), function(n){f_y_y(x, my_mu, my_sigma,n, my_k)})
    sum(all_bids * p_n * f_y) / sum(p_n*f_y)
  })

  expect_equal(bids_mat, bids_flat, tolerance = .001)
})

test_that("bid linearity trick works, with default, bajari spec", {
  my_k <- .3
  my_mu <- 1.1
  my_sigma <- .6
  my_c <- .2

  my_mats <- bid_mats(seq(-3,3,length.out=50), seq(.01, 3, length.out=50), c(2:30), my_k, 0, 1)
  bids_mat <- bid_vec(
    my_mu,
    my_sigma,
    seq(-3,3,length.out=50),
    seq(.01, 3, length.out=50),
    my_c,
    3,
    30,
    my_k,
    my_mats
  )

  bids_flat <- sapply(my_sigma*seq(-3,3,length.out=50)+my_mu, function(x){
    all_bids <- sapply(c(2:30), function(n){
      bid(x,n,my_c,my_k,my_mu,my_sigma)
    })
    p_n <- dpois(c(2:30), 3)
    f_y <- sapply(c(2:30), function(n){f_y_y(x, my_mu, my_sigma,n, my_k)})
    sum(all_bids * p_n * f_y) / sum(p_n*f_y)
  })

  expect_equal(bids_mat, bids_flat, tolerance = .001)
})


test_that("bid linearity trick works, with default, big spec", {
  my_k <- .8
  my_mu <- 1000
  my_sigma <- 10
  my_c <- 17

  my_mats <- bid_mats(seq(-3,3,length.out=50), seq(.01, 3, length.out=50), c(2:30), my_k, 0, 1)
  bids_mat <- bid_vec(
    my_mu,
    my_sigma,
    seq(-3,3,length.out=50),
    seq(.01, 3, length.out=50),
    my_c,
    3,
    30,
    my_k,
    my_mats
  )

  bids_flat <- sapply(my_sigma*seq(-3,3,length.out=50)+my_mu, function(x){
    all_bids <- sapply(c(2:30), function(n){
      bid(x,n,my_c,my_k,my_mu,my_sigma)
    })
    p_n <- dpois(c(2:30), 3)
    f_y <- sapply(c(2:30), function(n){f_y_y(x, my_mu, my_sigma,n, my_k)})
    sum(all_bids * p_n * f_y) / sum(p_n*f_y)
  })

  expect_equal(bids_mat, bids_flat, tolerance = .001)
})
