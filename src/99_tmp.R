tmp <- map(berichte_cleaned_de[1:2], function(doc) {
  # does the line end with a hyphen?
  hyphen_end <- map_lgl(doc, function(line) {
    lastchar <- str_sub(line, start = -1, end = -1) # whats the last character of the line
    if(lastchar == "-") {
      return(TRUE)
    }
    else return(FALSE)
  })
  out <- if(length(doc) == 1) {
    return(doc)
  } else {
    return(custom_flattener(doc, hyphen_end))
  }
  return(out)
})
