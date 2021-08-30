fact_to_num <- function(var) {
  label <- levels(var)
  out <- as.numeric(var) %>% set_labels(labels = label)
}

# num to fact
num_to_fact <- function(var) {
  level <- get_labels(var) 
  out <- factor(var, labels = level)
}

# tmp <- num_to_fact(df_brm$B1_amf)

# plot_df <-  df %>% select(`A1_FB2[SQ001]`:`A1_FB2[SQ005]`)
# tmp <- df %>% select(`A1_FB2[SQ001]`:`A1_FB2[SQ005]`) %>% mutate(across(.fns = fact_to_num))