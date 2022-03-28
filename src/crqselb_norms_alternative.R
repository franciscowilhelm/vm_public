load("data/df_crqs.RData")

# norms with rounding to nearest possible scale score.


crq_meansd <- df_crqs %>%
  summarize(across(oexp:act, list(mean = ~mean(.x, na.rm = TRUE), sd = ~sd(.x, na.rm = TRUE)), .names = "{.col}.{.fn}"))


# Compute Stanines: 1) Where they begin in terms of CRQ raw scores, and transformed scores.

# we need to take mean = 5, sd/2 = 1, and compute values for each.
compute_stanine <- function(mean, sd) {
  x <- as.double(mean)
  y <- as.double(sd)
  # S1 begins at -2.25z, ends at -1.75z and so on (0.5z width) and so on.
  out <- c(x-2.25*y, x-1.75*y, x-1.25*y, x-0.75*y, x-0.25*y, x+0.25*y, x+0.75*y, x+1.25*y, x+1.75*y)
  return(out)
}

# set values out of bounds to upper and lower minimum (1, 5)
set_to_bounds <- function(x) {
  if(x < 1) x <- 1
  else if(x > 5) x <- 5
  x
}
# vectorize it
vset_to_bounds <- Vectorize(set_to_bounds)

# stanine rescaled values. Not really needed because in diagnostics stanines are always binned.
stanine_rescale <- function(x) {
  psych::rescale(x, mean = 5, sd = 2, df = TRUE)
}

names_for_stanines <- c("oexp", "jmk", "ssk", "inv", "conf", "ccl", "jcha", "scs", "net", "cexpl", "lear","knsk","mot","env","act")
stanines_df <- map_dfc(names_for_stanines, function(scale){
  mean <- crq_meansd[1, str_c(scale, ".mean")]
  sd <- crq_meansd[1, str_c(scale, ".sd")]
  stanines <- compute_stanine(mean = mean,
                              sd = sd)
  out <- enframe(stanines) %>% select(value) 
  names(out) <- scale
  return(out)
}) 

# Applying vset_to_bounds Function to each element of the dataframe, setting values
# below 1 to 1 and above 5 to 5
stanines_df <- modify(stanines_df, vset_to_bounds)

# Creating new Column, numbering the Stanine values
stanines_df <- stanines_df %>% mutate(Stanines = c(1:9), .before = "oexp")

# stanines_df


# faktoren haben items mit 3, items mit 4. Items mit 4: nur conf.
possibleval_3 <- seq(1,5, length.out = 13)
possibleval_4 <- tibble(i1 = 1:5, i2 = 1:5, i3 = 1:5, i4 = 1:5) %>% cross_df() %>% rowMeans() %>% unique()

# überdimensionen deutlich komplexer. macht hier keinen sinn?

#knsk besteht aus 3 3ern, mot aus 2 3ern und 1 x4er, env aus 2 x 3ern, act aus 3 x 3ern
possibleval_knsk <- cross_df(tibble(a = possibleval_3, b = possibleval_3, c=  possibleval_3)) %>% rowMeans() %>% unique()
possibleval_mot <- cross_df(list(a = possibleval_3, b = possibleval_3, c=  possibleval_4)) %>% rowMeans() %>% unique()
possibleval_env <- cross_df(list(a = possibleval_3, b = possibleval_3)) %>% rowMeans() %>% unique()
possibleval_act <- cross_df(tibble(a = possibleval_3, b = possibleval_3, c=  possibleval_3)) %>% rowMeans() %>% unique()

# now we have: the stanines and the nearest possible raw value (score).

# # 3er runter runden - version
# map_dfc(c("oexp", "jmk", "ssk", "inv", "ccl", "jcha", "scs", "net", "cexpl", "lear"), function(s) {
#   stan_col <- stanines_df[,s, drop = TRUE]
#   print(s)
#   newvals <- sapply(stan_col, function(x) {
#     idx <- abs(x - possibleval_3) %>% which.min()
#     return <- possibleval_3[idx-1] # idx-1 to roudn down
#     return <- ifelse(is_empty(return), 1, return) #fix if idx is already at 1 and 1-1 = 0 produces error.
#     return(return)
#   })
#   out <- stanines_df[,s]
#   out[] <- newvals
#   return(out)
# })

stanine_rounding <- function(s, possibleval) {
  stan_col <- stanines_df[,s, drop = TRUE]
  newvals <- sapply(stan_col, function(x) {
    idx <- abs(x - get(possibleval)) %>% which.min()
    return <- get(possibleval)[idx] 
    return(return)
  })
  out <- stanines_df[,s]
  out[] <- newvals
  return(out)
}

# 3er normal runde 
stanines_df_rounded <- map_dfc(c("oexp", "jmk", "ssk", "inv", "ccl", "jcha", "scs", "net", "cexpl", "lear"), function(s) {
  stanine_rounding(s, "possibleval_3")
})

# 4er normal runden 
stanines_df_rounded_conf <- map_dfc("conf", function(s) {
  stanine_rounding(s, "possibleval_4")
})

# dimensionen runden

# 4er normal runden 
stanines_df_rounded_dims <- map2_dfc(c("knsk", "mot", "env", "act"),
                                     c("possibleval_knsk", "possibleval_mot", "possibleval_env", "possibleval_act"),
                                     function(s, pval) {
  stanine_rounding(s, pval)
})


stanines_df_rounded_all <- bind_cols(stanines_df_rounded, stanines_df_rounded_conf, stanines_df_rounded_dims) %>% rownames_to_column(var = "stanine") 

# prozentrang braucht es auch für abbildung
# wir nehmen nicht empirische sondern erwartete prozentränge da längst nicht alle werte vorhanden sind.

# pnorm(5, mean = 5, sd =2) # stanine based normal distributon using the distribution function. e.g. pnorm(5, mean = 5, sd =2) = 0.5 (50%)
stanine_percentrang_calc <- function(x, mean, sd) {
  round(pnorm(x, mean = mean, sd = sd)*100,0) # returns prozentrang without decimal point
}

# crq_meansd <- df_crqs %>%
#   summarize(across(oexp:act, list(mean = ~mean(.x, na.rm = TRUE), sd = ~sd(.x, na.rm = TRUE)), .names = "{.col}.{.fn}"))
tmp1 <- df_crqs %>% summarise(across((oexp:act), list(mean = ~mean(.x, na.rm = TRUE)))) %>% t()
tmp2 <- df_crqs %>% summarise(across((oexp:act), list(sd = ~sd(.x, na.rm = TRUE)))) %>% t()

crq_mean_sd_long <- tibble(name = names(df_crqs %>% select(oexp:act))) %>% mutate(mean = tmp1[,1], sd = tmp2[,1])

prang_list <- map2(names_for_stanines[1:11], c(rep("possibleval_3",4),
                              "possibleval_4",
                              rep("possibleval_3",6)),
     function(s, pval) {
  prang_dbl <- map_dfr(get(pval), function(val) {
    prang <- stanine_percentrang_calc(val,
                             mean = crq_mean_sd_long[crq_mean_sd_long$name == s, "mean", drop = TRUE],
                             sd = crq_mean_sd_long[crq_mean_sd_long$name == s, "sd", drop = TRUE])
    stanval <- (val >= stanines_df_rounded_all[,s,drop = TRUE]) %>% sum() %>% ifelse(. < 1, 1, .)
    return(tibble(Rohwert = val, `Prozentrang` = prang, `Stanine-Wert` = stanval))
  })
})
names(prang_list) <- names_for_stanines[1:11]

stanine_percentrang_calc(possibleval_3[10], crq_mean_sd_long$mean[1], crq_mean_sd_long$sd[1])

# do it different for the 4 dimensions. 0.25 schritte
prang_list_dim <- map2(names_for_stanines[12:15], list(seq(1,5, 0.25), seq(1,5, 0.25), seq(1,5, 0.25), seq(1,5, 0.25)),
                   function(s, pval) {
                     prang_dbl <- map_dfr(pval, function(val) {
                       prang <- stanine_percentrang_calc(val,
                                                         mean = crq_mean_sd_long[crq_mean_sd_long$name == s, "mean", drop = TRUE],
                                                         sd = crq_mean_sd_long[crq_mean_sd_long$name == s, "sd", drop = TRUE])
                       stanval <- (val >= stanines_df_rounded_all[,s,drop = TRUE]) %>% sum() %>% ifelse(. < 1, 1, .)
                       return(tibble(Rohwert = val, `Prozentrang` = prang, `Stanine-Wert` = stanval))
                     })
                   })
names(prang_list_dim) <- names_for_stanines[12:15]

writexl::write_xlsx(c(Stanines_Tabelle = list(stanines_df_rounded_all), prang_list, prang_list_dim),
                    path = "outputs/CRQ S Normen 20220324.xlsx")



# test - abweichungen zwischen stanine und prozentränge entstehen durch rundungen. 
# c(1,2,3,4,5)
# stanine_percentrang_calc(3.33, crq_mean_sd_long[crq_mean_sd_long$name == "inv", "mean", drop = TRUE],
#                          crq_mean_sd_long[crq_mean_sd_long$name == "inv", "sd", drop = TRUE])
# stanine_percentrang_calc(2,5,2) #stanine von 2
# # 3 wäre ein stanine von 2 (2.99-3.34)
# stanine_percentrang_calc(-1.75,0,1) # stanine 2 beginnt bei -1.75 SD und endet bei -1.25 SD
# stanine_percentrang_calc(-1.25,0,1) # stanine 3 beginnt
# stanine_percentrang_calc(-0.75,0,1) # stanine 4 beginnt
# stanine_percentrang_calc(-0.25,0,1) #stanine 5 beginnt


# # Cleaning up the Dataframe with the means and standard deviations of the CRQ scales
# crq_mean <- crq_meansd %>% select(contains(".mean"))
# crq_sd <- crq_meansd %>% select(contains(".sd"))
# 
# # Set Column names to be the same, for merging
# names(crq_mean) <- names_for_stanines
# names(crq_sd) <- names_for_stanines
# 
# # Merging the Dataframes
# crq_meansd <- rbind(crq_mean,crq_sd) %>% mutate("measure" = c("mean","sd"), .before = "oexp")
