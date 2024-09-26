library(tidyverse)
library(readxl)


# cases = tar_read(cases_consumer)
# metadata = "../data/ccc_database/rds/ccc_metadata.rds"
# texts = "../data/ccc_database/rds/ccc_texts.rds"
# file_subject_matter = "../data/ccc_database/rds/ccc_subject_matter.rds"
# subject = "spotřebitel"
# subject2 = "církev"
# file = "data/US_Cituje.xlsx"
# acts = "../data/ccc_database/rds/ccc_references.rds"
# model = "bernoulli"

subset_data = function(file, file_subject_matter = subject_matter, year = NULL, doc_ids = NULL, to_filter = NULL, subject = NULL, subject2 = NULL, only_merits = TRUE){
  # Creates a DF with unique doc_ids as well as filtered restitution cases
  subject_matter_filtered = read_rds(file_subject_matter)

  
  if(!is.null(doc_ids)){
    subject_matter_filtered = subject_matter_filtered |>
      filter(doc_id %in% doc_ids)
  }
  
  if(!is.null(subject)) {
    subject_matter_filtered = subject_matter_filtered |>
      filter(str_detect(subject_matter, subject))
  }
  
  if(!is.null(subject2)) {
    subject_matter_filtered = read_rds(file_subject_matter) |> 
      filter(doc_id %in% subject_matter_filtered$doc_id) |>
      filter(str_detect(subject_matter, subject2))
  }
  
  output = read_rds(file) |> 
    filter(doc_id %in% subject_matter_filtered$doc_id)
  
  if(only_merits == TRUE){
    output = output |>
      filter(grounds == "merits")
  }
  
  if(!is.null(to_filter)) {
    filtered = output |>
      filter(str_detect(popular_name, to_filter))
    
    output = output |>
      filter(!doc_id %in% filtered$doc_id)
  }
  
  if(!is.null(year)){
   output = output |>
      filter(year_decision < year)
  }
  
  output = output |>
    select(doc_id, case_id, date_decision)
    # mutate(case_id = case_when(case_nr > 1 ~ paste0(case_id, "-", case_nr),
    #                            .default = case_id)) |>
    # select(-case_nr)
  

  
  return(output)
}

transform_data = function(cases, file, metadata, acts = NULL, texts, model = "bernoulli") {
  data_long = readxl::read_xlsx(file) |>
    dplyr::rename(citing_doc_id = "Sp. zn.",
                  citing_date_decision = "Ze dne",
                  citing_type_decision = "Druh",
                  citing_court = "Soud",
                  cited_doc_id = "Cituje",
                  cited_date_decision = "Ze dne citováno",
                  cited_type_decision = "Druh citováno",
                  cited_court = "Soud citováno",
                  quality = "Kvalita",
                  database = "Máme citované J v db") |>
    filter(!cited_date_decision %in% "NULL") |>
    mutate(citing_doc_id = str_replace(string = citing_doc_id, pattern = " ÚS", replacement = "ÚS"),
           citing_date_decision = as_date(citing_date_decision),
           cited_doc_id = str_replace(string = cited_doc_id, pattern = " ÚS", replacement = "ÚS"),
           cited_doc_id = str_remove(string = cited_doc_id, pattern = "\\s\\d+\\/\\d+\\sSb\\."),
           cited_doc_id = str_remove(string = cited_doc_id, pattern = "\\s?-\\s?\\d+"),
           cited_date_decision = as_date(cited_date_decision)) |>
    filter(!quality %in% c("Citováno odlišným stanoviskem", "Nesouhlasí, neaplikuje", "Překonán")) |>
    left_join(cases, by = join_by(citing_doc_id == case_id, citing_date_decision == date_decision)) |>
    mutate(citing_doc_id = doc_id) |>
    select(-doc_id) |>
    drop_na(c(citing_doc_id, cited_doc_id))
  
  if(model == "bernoulli"){
    data_long = data_long |>
      select(citing_doc_id, cited_doc_id)
    
    if(!is.null(acts)) {
      data_long = data_long |> 
        rbind(read_rds(acts) |>
                filter(act_type %in% c("ordinary_act", "constitutional_act")) |>
                mutate(act = str_extract(string = concerned_act, pattern = "\\d+/\\d+\\sSb\\.(/Sb\\.m\\.s\\.)?"),
                       paragraph = str_remove(concerned_act, pattern = "\\d+/\\d+\\sSb\\.(/Sb\\.m\\.s\\.)?(,\\s)?") |>
                         str_split(pattern = ",\\s")) |>
                select(-c(concerned_act, act_type)) |>
                unnest(paragraph) |>
                mutate(cited_doc_id = paste(act, paragraph, sep = " ")) |>
                select(doc_id, cited_doc_id) |>
                rename(citing_doc_id = "doc_id") |>
                filter(citing_doc_id %in% cases$doc_id))
    }
    data_long = data_long |>
      mutate(cited_count = 1) |>
      distinct()
  } else {
    data_long = data_long |>
      left_join(read_rds(texts), by = join_by(citing_doc_id == doc_id)) |>
      mutate(cited_count = str_count(string = str_replace((str_replace(string = text, pattern = "\\n", replacement = " ")), pattern = "  ", " "), pattern = str_replace(string = cited_doc_id, pattern = "ÚS", replacement = " ÚS"))) |>
      select(citing_doc_id, cited_doc_id, cited_count) |>
      distinct() |>
      mutate(cited_count = replace(cited_count, cited_count == 0, 1))
  }
  
  data = data_long |>
    pivot_wider(names_from = "cited_doc_id", values_from = cited_count, values_fill = 0) |>
    column_to_rownames(var = "citing_doc_id")
  return(data)
}

# transform_data = function(cases, file, metadata, texts) {
#   cases = read_rds(metadata) |>
#     select(doc_id, case_id, date_decision) |>
#     filter(doc_id %in% cases$doc_id) |>
#     left_join(read_rds(texts))
#   
#   data_long = readxl::read_xlsx(file) |>
#     dplyr::rename(citing_doc_id = "Sp. zn.",
#                   citing_date_decision = "Ze dne",
#                   citing_type_decision = "Druh",
#                   citing_court = "Soud",
#                   cited_doc_id = "Cituje",
#                   cited_date_decision = "Ze dne citováno",
#                   cited_type_decision = "Druh citováno",
#                   cited_court = "Soud citováno",
#                   quality = "Kvalita",
#                   database = "Máme citované J v db") |>
#     filter(!cited_date_decision %in% "NULL") |>
#     mutate(citing_doc_id = str_replace(string = citing_doc_id, pattern = " ÚS", replacement = "ÚS"),
#            citing_date_decision = as_date(citing_date_decision),
#            cited_doc_id = str_replace(string = cited_doc_id, pattern = " ÚS", replacement = "ÚS"),
#            cited_doc_id = str_remove(string = cited_doc_id, pattern = "\\s\\d+\\/\\d+\\sSb\\."),
#            cited_doc_id = str_remove(string = cited_doc_id, pattern = "\\s?-\\s?\\d+"),
#            cited_date_decision = as_date(cited_date_decision)) |>
#     filter(!quality %in% c("Citováno odlišným stanoviskem", "Nesouhlasí, neaplikuje", "Překonán")) |>
#     left_join(cases, by = join_by(citing_doc_id == case_id, citing_date_decision == date_decision)) |>
#     mutate(citing_doc_id = doc_id) |>
#     select(-doc_id) |>
#     drop_na(c(citing_doc_id, cited_doc_id)) |>
#     mutate(cited_count = str_count(string = str_replace((str_replace(string = text, pattern = "\\n", replacement = " ")), pattern = "  ", " "), pattern = str_replace(string = cited_doc_id, pattern = "ÚS", replacement = " ÚS"))) |>
#     select(citing_doc_id, cited_doc_id, cited_count) |>
#     distinct() |>
#     mutate(cited_count = replace(cited_count, cited_count == 0, 1)) 
#   
#   data = data_long |>
#     pivot_wider(names_from = "cited_doc_id", values_from = cited_count, values_fill = 0) |>
#     column_to_rownames(var = "citing_doc_id")
#   return(data)
# }

reshape_data = function(data, filtered = TRUE) {
  # --- Reshape ------------------------------------------------------------------
  # Goal: Return Three Data Matrices:
  #   1 real, 1 with count, 1 with citation matrix
  if(nrow(data) == 0) return(NULL)
  
  # J number of legal documents
  J <- length(rownames(data))
  # K number of different sources overall
  K <- length(colnames(data))
  #  N
  N <- J*K
  
  # y[N] dummy if user i follows elite j
  set_of_all_links <- colnames(data)
  
  # y connection matrix as a vector 
  # y is binary vector 
  # y_count is count 
  y_count <- vector()
  for(i in seq(1,J)){
    y_count <- append(y_count, data[i,])
  }
  y_count <- unlist(y_count)
  y <- y_count
  y[y_count > 1] <- 1
  
  table(y_count)
  table(y)
  
  #  jj[N] verdict/decision for observation y_n
  jj <- vector()
  for(i in seq(1,J)){ # accounting for python starting at 0
    jj <- append(jj, rep(i,K))
  }
  
  #  kk[N]
  # legal document/source for observation y_n
  kk <- rep(seq(1,K), J) # accounting for python starting at 0
  
  # This filters those legal sources out, that appear only once --------------
  # Step 1: Get a mask
  # y into matrix: Rows nr. of cases, cols nr of sources
  # y_matrix = np.asarray(y).reshape(J,K)
  y_matrix <- data
  # colsums
  # citations = y_matrix.sum(axis=0)
  citations <- colSums(y_matrix)
  # colsums > 1 == True
  mask_sources_to_keep_one_decision <- citations > 1
  # TF vector * nr of decisions (gets mask to right shape)
  mask_sources_to_keep <- rep(mask_sources_to_keep_one_decision,J)
  # Step 2: Select with the mask
  y_cit_filter <- y[mask_sources_to_keep]
  y_count_cit_filter <- y_count[mask_sources_to_keep]
  set_of_all_links_cit_filter <- set_of_all_links[mask_sources_to_keep_one_decision]
  K_cit_filter <- sum(mask_sources_to_keep_one_decision)
  N_cit_filter <- length(y_cit_filter)
  J_cit_filter <- N_cit_filter/K_cit_filter
  
  jj_cit_filter <- vector()
  # re-create counters
  for(i in seq(1,J_cit_filter)){
    jj_cit_filter <- append(jj_cit_filter, rep(i,K_cit_filter))
  }
  
  #  kk[N]
  # legal document/source for observation y_n
  kk_cit_filter <- rep(seq(1,K_cit_filter), J_cit_filter)
  
  if(filtered == TRUE) {
    data = list(case_ids = rownames(y_matrix),
                J = J,
                K = K_cit_filter,
                N = N_cit_filter,
                jj = jj_cit_filter,
                kk = kk_cit_filter,
                y = y_cit_filter)
  } else {
    data = list(case_ids = rownames(y_matrix),
                J = J,
                K = K,
                N = N,
                jj = jj,
                kk = kk,
                y = y)
  }
  return(data)
}


# DATA PREP
# finalise_data = function(file, cases) {
#   positive_citation = c("Souhlasí a Následuje", "Vysvětlení", "Aplikuje a rozvíjí", "Vysvětlení", "Neaplikuje, ale souhlasí")
#   negative_citation = c("Citováno odlišným stanoviskem", "Nesouhlasí, neaplikuje", "Překonán")
#   
#   data = readxl::read_xlsx(file) |>
#     rename(citing_doc_id = "Sp. zn.",
#            citing_date_decision = "Ze dne",
#            citing_type_decision = "Druh",
#            citing_court = "Soud",
#            cited_doc_id = "Cituje",
#            cited_date_decision = "Ze dne citováno",
#            cited_type_decision = "Druh citováno",
#            cited_court = "Soud citováno",
#            quality = "Kvalita",
#            database = "Máme citované J v db") |>
#     mutate(citing_doc_id = str_replace(string = citing_doc_id, pattern = " ÚS", replacement = "ÚS"),
#            citing_date_decision = as_date(citing_date_decision),
#            cited_doc_id = str_replace(string = cited_doc_id, pattern = " ÚS", replacement = "ÚS"),
#            cited_date_decision = as_date(cited_date_decision)) |>
#     left_join(., cases, by = join_by(citing_doc_id == case_id, citing_date_decision == date_decision)) |>
#     mutate(citing_doc_id = doc_id) |>
#     select(-doc_id) |>
#     left_join(., cases, by = join_by(cited_doc_id == case_id, cited_date_decision == date_decision)) |>
#     mutate(cited_doc_id = doc_id) |>
#     select(-doc_id) |>
#     drop_na(c(cited_doc_id, citing_doc_id)) |>
#     filter(!is.na(quality) & cited_court == "Ústavní soud" & quality != "Neuvedeno") |>
#     select(citing_doc_id, cited_doc_id, quality) |>
#     mutate(quality = case_when(quality %in% positive_citation ~ 1,
#                                quality %in% negative_citation ~ 0)) |>
#     rename(cited = cited_doc_id,
#            citing = citing_doc_id) |> 
#     mutate(citing = factor(citing, levels = union(citing, cited)), 
#            cited = factor(cited, levels = union(citing, cited)))
# }





