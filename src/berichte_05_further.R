x <- berichte_cleaned_de %>% map_dbl(., function(x) { sum(str_count(x, pattern = "\\W+")) } )
hist(x)
