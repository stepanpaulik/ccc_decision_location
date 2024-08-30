# Load necessary libraries
library(tidytext)
library(tidyverse)
library(widyr)
library(doc2vec)
library(targets)
library(kableExtra)

data = targets::tar_read(fitted_model_ozv)

# Pre-selected documents
pre_selected = tar_read(fitted_model_spotr) |>
  rename(doc_id = case) |>
  filter(sign(higher) == sign(lower)) |>
  filter(higher < 0)

pre_selected = tar_read(fitted_model_ozv) |>
  rename(doc_id = case) |>
  filter(sign(higher) == sign(lower)) |>
  filter(higher < 0)

# POPULAR NAME ------------------------------------------------------------
# Example data: large population of documents
large_corpus = read_rds("../data/ccc_database/rds/ccc_metadata.rds") |>
  filter(!is.na(popular_name))

# TF-IDF ------------------------------------------------------------------
# Tokenize the text data and calculate TF-IDF
if(embeddings != TRUE){
  tfidf = large_corpus |>
    unnest_tokens(word, popular_name) |>            # Tokenize text
    count(doc_id, word, sort = TRUE) |>     # Count word frequencies
    bind_tf_idf(word, doc_id, n)          # Calculate TF-IDF
  
  # Separate the pre-selected and large corpus after TF-IDF calculation
  tfidf_pre_selected = tfidf |> filter(doc_id %in% pre_selected$doc_id)
  tfidf_large_corpus = tfidf |> filter(!doc_id %in% pre_selected$doc_id)
  
  # Compute pairwise cosine similarity between pre-selected docs and large corpus
  cosine_sim = tfidf |>
    pairwise_similarity(doc_id, word, tf_idf)
  
  cosine_sim_filtered = cosine_sim |>
    filter(item1 %in% pre_selected$doc_id & !item2 %in% pre_selected$doc_id)
  
  # Display the cosine similarity results
  cosine_matrix <- cosine_sim_filtered |>
    spread(item2, similarity, fill = 0) |>
    rename(pre_selected_doc = item1)
  
  print(cosine_matrix)
  
  # Find the most similar document in the large corpus for each pre-selected document
  most_similar <- cosine_sim_filtered |>
    group_by(item1) |>
    filter(similarity == max(similarity)) |>
    ungroup() |>
    arrange(desc(similarity))
  
  cat("Most similar documents in the large corpus for each pre-selected document:\n")
  print(most_similar)
}

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
cosine_sim = predict(model, newdata = pre_selected$doc_id, type = "nearest", which = "doc2doc", top_n = 10) |>
  reduce(bind_rows)


cosine_sim |>
  select(-term1) |>
  kable(col.names = c("Case", "Similarity",  "Rank"),
        booktabs = T,
        digits = 3,
        caption = "A table showing the 10 most cosine similar cases to the four statistically significant customer law cases whose position was estimated to be negative. The highlighted rows are those, in which the most similar decision was one of the other three from the four.") |>
  pack_rows(index = table(fct_inorder(cosine_sim$term1))) |>
  row_spec(which(cosine_sim_spotr$term2 %in% data$case), bold = T, color = "white", background = "pink") |>
  row_spec(which(cosine_sim_spotr$term2 %in% cosine_sim$term1), bold = T, color = "white", background = "gray")


e# TITLE -------------------------------------------------------------------
large_corpus = readxl::read_xlsx("data/decision_titles.xlsx") |>
  rename(case_id = "Citace, Sp.zn.",
         text = "Věta",
         date_decision = "Ze dne (Schváleno)") |>
  select(case_id, text, date_decision) |>
  mutate(date_decision = as_date(date_decision, format = "%d. %m. %Y")) |>
  mutate(case_id = str_replace(string = case_id, pattern = " ÚS", replacement = "ÚS")) |>
  left_join(read_rds("../data/ccc_database/rds/ccc_metadata.rds") |> 
              select(case_id, case_nr, doc_id, date_decision, popular_name) |> 
              mutate(case_id = case_when(case_nr > 1 ~ paste0(case_id, "-", case_nr),
                                         .default = case_id))) |>
  distinct(case_id, text, .keep_all = TRUE) |>
  select(doc_id, text)

model = large_corpus |>
  doc2vec::paragraph2vec(type = "PV-DBOW", dim = 100L,  iter = 20, threads = 8L)

# Infer vectors for the documents in the combined dataset
doc_vectors = as.matrix(model, which = "docs") |>
  as_tibble(rownames = "doc_id")

# Compute cosine similarity between pre-selected docs and large corpus docs
predict(model, newdata = pre_selected$doc_id, type = "nearest", which = "doc2doc", top_n = 10)

# Convert the cosine similarity matrix into a tidy format
cosine_sim_df <- as.data.frame(cosine_sim) %>%
  mutate(pre_selected_doc = 11:12) %>%
  gather(key = "large_corpus_doc", value = "similarity", -pre_selected_doc) %>%
  mutate(large_corpus_doc = as.numeric(gsub("V", "", large_corpus_doc)))

# Find the most similar document in the large corpus for each pre-selected document
most_similar <- cosine_sim_df %>%
  group_by(pre_selected_doc) %>%
  filter(similarity == max(similarity)) %>%
  ungroup() %>%
  arrange(desc(similarity))

cat("Most similar documents in the large corpus for each pre-selected document:\n")
print(most_similar)

