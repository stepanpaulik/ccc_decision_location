# Load necessary libraries
library(tidytext)
library(tidyverse)
library(widyr)
library(doc2vec)
library(targets)
library(kableExtra)

load("data/report/vizualization.RData")
  

find_similar_popular_name = function(data, full_data, number_similar = 10){
  large_corpus = read_rds("../data/ccc_database/rds/ccc_metadata.rds") |>
    filter(!is.na(popular_name))
  
  # DOC2VEC -----------------------------------------------------------------
  # Train the doc2vec model
  model = large_corpus |>
    select(doc_id, popular_name) |>
    rename(text = popular_name) |>
    doc2vec::paragraph2vec(type = "PV-DBOW", dim = 100L,  iter = 20, threads = 8L)
  
  # Infer vectors for the documents in the combined dataset
  doc_vectors = as.matrix(model, which = "docs") |>
    as_tibble(rownames = "doc_id")
  
  # Compute cosine similarity between pre-selected docs and large corpus docs
  output = predict(model, newdata = data$doc_id, type = "nearest", which = "doc2doc", top_n = number_similar) |>
    reduce(bind_rows) |>
    left_join(read_rds("../data/ccc_database/rds/ccc_metadata.rds") |> select(doc_id, case_id), by = join_by(term1 == doc_id)) |>
    rename(term1_case_id = case_id) |>
    left_join(read_rds("../data/ccc_database/rds/ccc_metadata.rds") |> select(doc_id, case_id), by = join_by(term2 == doc_id)) |>
    rename(term2_case_id = case_id)
  
  return(output)
}

cosine_sim_ozv = find_similar_popular_name(data = model_ozv, full_data = result_ozv)
cosine_sim_consumer_delays = find_similar_popular_name(data = model_consumer_onlydelays, full_data = model_consumer)
cosine_sim_consumer_gm = find_similar_popular_name(data = model_consumer_gm, full_data = model_consumer, number_similar = 3)

cosine_sim_ozv |>
  select(term2_case_id, similarity, rank) |>
  kable(col.names = c("Case", "Similarity",  "Rank"),
        booktabs = T,
        digits = 3,
        caption = "A table showing the 10 most cosine similar cases to the statistically significant whose position was estimated to be negative. The pink highlighted rows are those, in which the most similar decision was one of the negative decisions, the grey highlighted rows are those who were in the whole dataset.") |>
  pack_rows(index = table(fct_inorder(cosine_sim_ozv$term1_case_id))) |>
  row_spec(which(cosine_sim_ozv$term2_case_id %in% model_ozv$case_id), bold = T, color = "white", background = "pink") |> 
  row_spec(which(cosine_sim_ozv$term2_case_id %in% cosine_sim_ozv$term1_case_id), bold = T, color = "white", background = "gray")


cosine_sim_consumer_delays |>
  select(term2_case_id, similarity, rank) |>
  kable(col.names = c("Case", "Similarity",  "Rank"),
        booktabs = T,
        digits = 3,
        caption = "A table showing the 10 most cosine similar cases to the statistically significant whose position was estimated to be negative. The pink highlighted rows are those, in which the most similar decision was one of the negative decisions, the grey highlighted rows are those who were in the whole dataset.") |>
  pack_rows(index = table(fct_inorder(cosine_sim_consumer_delays$term1_case_id))) |>
  row_spec(which(cosine_sim_consumer_delays$term2_case_id %in% model_consumer$case_id), bold = T, color = "white", background = "pink") |> 
  row_spec(which(cosine_sim_consumer_delays$term2_case_id %in% cosine_sim_consumer_delays$term1_case_id), bold = T, color = "white", background = "gray")

cosine_sim_consumer_gm |>
  select(term2_case_id, similarity, rank) |>
  kable(col.names = c("Case", "Similarity",  "Rank"),
        booktabs = T,
        digits = 3,
        caption = "A table showing the 10 most cosine similar cases to the statistically significant whose position was estimated to be negative. The pink highlighted rows are those, in which the most similar decision was one of the negative decisions, the grey highlighted rows are those who were in the whole dataset.") |>
  pack_rows(index = table(fct_inorder(cosine_sim_consumer_gm$term1_case_id))) |>
  row_spec(which(cosine_sim_consumer_gm$term2_case_id %in% model_consumer$case_id), bold = T, color = "white", background = "pink") |> 
  row_spec(which(cosine_sim_consumer_gm$term2_case_id %in% cosine_sim_consumer_gm$term1_case_id), bold = T, color = "white", background = "gray")



# SAVE --------------------------------------------------------------------
pattern_to_keep = "^cosine_sim"

# Get a list of all objects in the environment
all_objects = ls()

# Identify objects that do not match the pattern
objects_to_remove = all_objects[!grepl(pattern_to_keep, all_objects)]

# Remove the objects that do not match the pattern
rm(list = objects_to_remove)

save.image("data/report/cosine_sims.RData")

# # POPULAR NAME ------------------------------------------------------------
# # Example data: large population of documents
# large_corpus = read_rds("../data/ccc_database/rds/ccc_metadata.rds") |>
#   filter(!is.na(popular_name))
# 
# # TF-IDF ------------------------------------------------------------------
# # Tokenize the text data and calculate TF-IDF
# if(embeddings != TRUE){
#   tfidf = large_corpus |>
#     unnest_tokens(word, popular_name) |>            # Tokenize text
#     count(doc_id, word, sort = TRUE) |>     # Count word frequencies
#     bind_tf_idf(word, doc_id, n)          # Calculate TF-IDF
# 
#   # Separate the pre-selected and large corpus after TF-IDF calculation
#   tfidf_pre_selected = tfidf |> filter(doc_id %in% pre_selected$doc_id)
#   tfidf_large_corpus = tfidf |> filter(!doc_id %in% pre_selected$doc_id)
# 
#   # Compute pairwise cosine similarity between pre-selected docs and large corpus
#   cosine_sim = tfidf |>
#     pairwise_similarity(doc_id, word, tf_idf)
# 
#   cosine_sim_filtered = cosine_sim |>
#     filter(item1 %in% pre_selected$doc_id & !item2 %in% pre_selected$doc_id)
# 
#   # Display the cosine similarity results
#   cosine_matrix <- cosine_sim_filtered |>
#     spread(item2, similarity, fill = 0) |>
#     rename(pre_selected_doc = item1)
# 
#   print(cosine_matrix)
# 
#   # Find the most similar document in the large corpus for each pre-selected document
#   most_similar <- cosine_sim_filtered |>
#     group_by(item1) |>
#     filter(similarity == max(similarity)) |>
#     ungroup() |>
#     arrange(desc(similarity))
# 
#   cat("Most similar documents in the large corpus for each pre-selected document:\n")
#   print(most_similar)
# }
# 
# 
# 
# 
# e# TITLE -------------------------------------------------------------------
# large_corpus = readxl::read_xlsx("data/decision_titles.xlsx") |>
#   rename(case_id = "Citace, Sp.zn.",
#          text = "Věta",
#          date_decision = "Ze dne (Schváleno)") |>
#   select(case_id, text, date_decision) |>
#   mutate(date_decision = as_date(date_decision, format = "%d. %m. %Y")) |>
#   mutate(case_id = str_replace(string = case_id, pattern = " ÚS", replacement = "ÚS")) |>
#   left_join(read_rds("../data/ccc_database/rds/ccc_metadata.rds") |>
#               select(case_id, case_nr, doc_id, date_decision, popular_name) |>
#               mutate(case_id = case_when(case_nr > 1 ~ paste0(case_id, "-", case_nr),
#                                          .default = case_id))) |>
#   distinct(case_id, text, .keep_all = TRUE) |>
#   select(doc_id, text)
# 
# model = large_corpus |>
#   doc2vec::paragraph2vec(type = "PV-DBOW", dim = 100L,  iter = 20, threads = 8L)
# 
# # Infer vectors for the documents in the combined dataset
# doc_vectors = as.matrix(model, which = "docs") |>
#   as_tibble(rownames = "doc_id")
# 
# # Compute cosine similarity between pre-selected docs and large corpus docs
# predict(model, newdata = pre_selected$doc_id, type = "nearest", which = "doc2doc", top_n = 10)
# 
# # Convert the cosine similarity matrix into a tidy format
# cosine_sim_df <- as.data.frame(cosine_sim) %>%
#   mutate(pre_selected_doc = 11:12) %>%
#   gather(key = "large_corpus_doc", value = "similarity", -pre_selected_doc) %>%
#   mutate(large_corpus_doc = as.numeric(gsub("V", "", large_corpus_doc)))
# 
# # Find the most similar document in the large corpus for each pre-selected document
# most_similar <- cosine_sim_df %>%
#   group_by(pre_selected_doc) %>%
#   filter(similarity == max(similarity)) %>%
#   ungroup() %>%
#   arrange(desc(similarity))
# 
# cat("Most similar documents in the large corpus for each pre-selected document:\n")
# print(most_similar)



# load("data/report/cosine_sims.RData")

