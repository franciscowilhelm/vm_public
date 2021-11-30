load("data/berichte_cleaned_0421.RData")
load("data/berichte_cleaned_0721.RData")
load("data/berichte_cleaned_1121.RData")


identifier_de <- c(identifier_de, identifier_0721_de, identifier_1121_de)
identifier_fr <- c(identifier_fr, identifier_0721_fr, identifier_1121_fr)

kanton_de <- c(kanton_de, kanton_0721_de, kanton_1121_de)
kanton_fr <- c(kanton_fr, kanton_0721_fr, kanton_1121_fr)

berichte_cleaned_de_integrated <- c(berichte_cleaned_de, berichte_cleaned_de_0721, berichte_cleaned_de_1121)
berichte_cleaned_fr_integrated <- c(berichte_cleaned_fr, berichte_cleaned_fr_0721, berichte_cleaned_fr_1121)

# find duplicates, first flatten then run stri_duplicated
tmp <- map_chr(berichte_cleaned_de_integrated, function(doc) {
  str_flatten(doc)
}
)

duplicate_idx <- stringi::stri_duplicated(tmp)
berichte_cleaned_de_integrated <- berichte_cleaned_de_integrated[!duplicate_idx]
identifier_de <- identifier_de[!duplicate_idx]
kanton_de <- kanton_de[!duplicate_idx]

# for french
tmp <- map_chr(berichte_cleaned_fr_integrated, function(doc) {
  str_flatten(doc)
}
)

duplicate_idx <- stringi::stri_duplicated(tmp)
berichte_cleaned_fr_integrated <- berichte_cleaned_fr_integrated[!duplicate_idx]
identifier_fr <- identifier_fr[!duplicate_idx]
kanton_fr <- kanton_fr[!duplicate_idx]



save(identifier_de, identifier_fr, kanton_de, kanton_fr,
     berichte_cleaned_de_integrated, berichte_cleaned_fr_integrated,
     file = "data/berichte_cleaned_integrated.RData")
