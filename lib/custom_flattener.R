custom_flattener <- function(doc, hyphen_end) {
  out <- doc[1] 
  for(l in head(seq_along(doc), -1)) {
    # when hyphenated
    if(hyphen_end[l] == TRUE ) {
      out <- paste(out, doc[l+1], sep = "")
    } else {
      out <- paste(out, doc[l+1], sep = " ")
    }
  }
  return(out)
}

