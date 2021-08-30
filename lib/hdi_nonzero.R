hdi_nonzero <- function(x) {
  # x needs to be tidyobject
  nonzero_idx <- pmap_lgl(x, function(...) {
    x <- tibble(...)
    (x$conf.low > 0 & x$conf.high > 0) | (x$conf.low < 0 & x$conf.high < 0)})
  return(x[nonzero_idx,])
}