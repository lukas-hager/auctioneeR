# **auctioneeR**

[![R-CMD-check](https://github.com/lukas-hager/auctioneeR/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/lukas-hager/auctioneeR/actions/workflows/R-CMD-check.yaml) [![test-coverage](https://github.com/lukas-hager/auctioneeR/actions/workflows/test-coverage.yaml/badge.svg)](https://github.com/lukas-hager/auctioneeR/actions/workflows/test-coverage.yaml)

This is a (silly-named) package to implement the estimation in my paper on auctions with default.

To run the estimation with some simulated data, use the following vignette:

```r
library(devtools)
install_github('lukas-hager/auctioneeR', force=TRUE)

true_params <- c(
  50,
  2,
  -.001,
  10,
  .5,
  -.0005,
  .1,
  .1,
  .0001,
  .5,
  10,
  .02
)

auction_data <- create_test_data(100)

start_params <- true_params * .9

out <- estimate_model(auction_data, 10, start_params, 30, TRUE)
```
