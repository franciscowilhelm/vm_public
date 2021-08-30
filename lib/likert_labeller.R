library(stringr)
# takes a percentage value and turns it into a label such that that values below 5 % are omitted
likert_labeller <- function(x, supress_small_values = TRUE) {
  out <- sprintf("%.*f%%", 1, 100 * x)
  if(supress_small_values == TRUE) {
    for (i in seq_along(x)) {
      if (x[i] <= 0.05)
        out[i] <- ""
    }
  }
  out <- str_replace(out, "[.]", ",")
  return(out)
}

proc_convert <- function(x) {
  out <- out <- sprintf("%.*f%%", 1, x)
  out <- str_replace(out, "[.]", ",")
}
