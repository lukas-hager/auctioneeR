riemann_sums <- function(f, lower, upper, n_bins, ...){
  seq_vals = seq(lower, upper, length.out=n_bins)
  base_size = seq_vals[2]-seq_vals[1]
  sum(f(seq_vals, ...) * base_size)
}
