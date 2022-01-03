frenchphrase_join <- function(sentence) {
  idx <- vector(mode = "logical", length = max(sentence$token_id)-2)
  for(t in  seq_along(idx)) {
    idx[t] <- sentence$pos[t] == "NOUN" & sentence$pos[t+1] == "ADP" & sentence$pos[t+2] == "NOUN"
  }
  # extract
  # stop when none found
  if(any(idx) == FALSE) { return("") }
  idx_which <- which(idx)
  out <- vector(mode = "character", length = length(idx_which))
  
  for(i in seq_along(idx_which)) {
    out[i] <- str_c(sentence$token[idx_which[i]], sentence$token[idx_which[i]+1], sentence$token[idx_which[i]+2], sep = " ", collapse = " ")
  }
  return(out)
}

