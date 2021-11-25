load("data/berichte_cleaned_0421.RData")
load("data/berichte_cleaned_0721.RData")
load("data/berichte_cleaned_1121.RData")

identifier <- c(identifier, identifier_0721, identifier_1121)
kanton <- c(kanton, kanton_0721, kanton_1121)
berichte_cleaned_de_integrated <- c(berichte_cleaned_de, berichte_cleaned_de_0721, berichte_cleaned_de_1121)
berichte_cleaned_fr_integrated <- c(berichte_cleaned_fr, berichte_cleaned_fr_0721, berichte_cleaned_fr_1121)

save(identifier, kanton, berichte_cleaned_de_integrated, berichte_cleaned_fr_integrated,
     file = "data/berichte_cleaned_integrated.RData")
