# Modellierung der Berichte (Topic modelling etc)

library(quanteda.textmodels)
# create weighting
dfm_anliegen_idf <- as.tokens(anliegen_spacyd, use_lemma = TRUE) %>% dfm() %>% dfm_tfidf()

# show doc frequencies
docfreq(dfm_anliegen_idf)[order(docfreq(dfm_anliegen_idf), decreasing = TRUE)] %>% .[1:80]

# Latent Semantic Analysis
mylsa <- textmodel_lsa(dfm_anliegen_idf, nd = 10) #question is, how many dimensions? (nd = argument)
tmp <- as_tibble(mylsa[["features"]], .name_repair = c("universal")) %>% mutate(names = dfm_anliegen_idf@Dimnames[["features"]])
head(tmp %>% arrange(desc(...1)), 10)
head(tmp %>% arrange(desc(...2)), 10)

# what to do wit hthis?

# 5b LDA ------
library(topicmodels)
# Assign an arbitrary number of topics
topic.count <- 15
# Convert the trimmed DFM to a topicmodels object
dfm_anliegen <- as.tokens(anliegen_spacyd, use_lemma = TRUE) %>% dfm() %>% 
  dfm_trim(min_termfreq = 10, max_termfreq = 500)
dfm2topicmodels <- convert(dfm_anliegen, to = "topicmodels")
lda.model <- LDA(dfm2topicmodels, topic.count)

terms(lda.model, 10)
# topics(lda.model)

# 5c CLUSTER

# Linkage method
library(cluster)
library(factoextra)

m <- c( "average", "single", "complete", "ward")
names(m) <- c( "average", "single", "complete", "ward")
#function to compute agglomerative coefficient
ac <- function(x) {
  agnes(mylsa$docs, method = x)$ac
}
#calculate agglomerative coefficient for each clustering linkage method
sapply(m, ac) #indistinguishable fit, ward has highest tho.
#perform hierarchical clustering using Ward's minimum variance
clust <- agnes(mylsa$docs, method = "ward")
#produce dendrogram
pltree(clust, cex = 0.6, hang = -1, main = "Dendrogram") 

#calculate gap statistic for each number of clusters (up to 10 clusters)
gap_stat <- clusGap(mylsa$docs, FUN = hcut, nstart = 25, K.max = 10, B = 50)
#produce plot of clusters vs. gap statistic
fviz_gap_stat(gap_stat) #higher gap, better. but k = 9 is a bit much. 4 has a local maximum.

# cut into k clusters.
#compute distance matrix
d <- dist(mylsa$docs, method = "euclidean")

#perform hierarchical clustering using Ward's method
final_clust <- hclust(d, method = "ward.D2" )

#cut the dendrogram into 4 clusters
groups <- cutree(final_clust, k=4)

#find number of observations in each cluster
table(groups)

# finds n = 1 groups, that is not good...
#append cluster labels to original data
tmp <- parsed_anliegen %>% add_count(doc_id) %>% arrange(doc_id)

tmp2 <- tmp %>% group_by(doc_id) %>% summarize(mean(n))

newgroups <- map2(groups, tmp2$`mean(n)`,
                  ~rep(.x, .y)) %>% flatten_dbl()

tmp$cluster <- newgroups
# count by cluster, 
# TODO: should be percent rather than count. wider = n per group as k = group columns.
tmp2 <- tmp %>%
  filter(cluster != 4) %>%
  group_by(cluster) %>% 
  count(lemma, sort = TRUE) %>% 
  pivot_wider(names_from = cluster, values_from = n, names_prefix = "c") %>% 
  mutate(c1 = c1/table(groups)[1], c2 = c2/table(groups)[2],
         c3 = c3/table(groups)[3])

head(tmp2 %>% arrange(desc(c1)), n = 20)
head(tmp2 %>% arrange(desc(c3)), n = 20)