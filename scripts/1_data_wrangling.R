library(tidyverse)
library(readxl)

file = "../data/US_metadata.rds"

subset_data = function(file, subject){
  # Creates a DF with unique doc_ids as well as filtered restitution cases
  if(subject == "restitution"){
    output = read_rds(file) %>%
      filter(grepl("restitu", field_register)) %>%
      select(doc_id, case_id, date_decision)
  }
  
  if(subject == "discrimination"){
    output = read_rds(file) %>%
      filter(grepl("diskriminace", field_register)) %>%
      select(doc_id, case_id, date_decision)
  }
  
  if(subject == "ozv"){
    output = read_rds(file) %>%
      filter(grepl("becně závazná vyhláška", field_register)) %>%
      select(doc_id, case_id, date_decision)
  }
  if(subject == "oop"){
    output = read_rds(file) %>%
      filter(grepl("opatření obecné povahy", field_register)) %>%
      select(doc_id, case_id, date_decision)
  }
  output = output %>%
    mutate(case_id = case_when(str_detect(case_id, " #1") ~ str_remove(case_id, " #1"),
                               str_detect(case_id, " #\\d") ~ str_replace(case_id, pattern = " #", replacement = "-"),
                               .default = case_id))
  return(output)
}

# DATA PREP
finalise_data = function(file, cases) {
  positive_citation = c("Souhlasí a Následuje", "Vysvětlení", "Aplikuje a rozvíjí", "Vysvětlení", "Neaplikuje, ale souhlasí")
  negative_citation = c("Citováno odlišným stanoviskem", "Nesouhlasí, neaplikuje", "Překonán")
  
  readxl::read_xlsx(file) %>%
    rename(citing_doc_id = "Sp. zn.",
           citing_date_decision = "Ze dne",
           citing_type_decision = "Druh",
           citing_court = "Soud",
           cited_doc_id = "Cituje",
           cited_date_decision = "Ze dne citováno",
           cited_type_decision = "Druh citováno",
           cited_court = "Soud citováno",
           quality = "Kvalita",
           database = "Máme citované J v db") %>%
    mutate(citing_doc_id = str_replace(string = citing_doc_id, pattern = " ÚS", replacement = "ÚS"),
           citing_date_decision = ymd(citing_date_decision),
           cited_doc_id = str_replace(string = cited_doc_id, pattern = " ÚS", replacement = "ÚS"),
           cited_date_decision = ymd(cited_date_decision)) %>%
    left_join(., cases, by = join_by(citing_doc_id == case_id, citing_date_decision == date_decision)) %>%
    mutate(citing_doc_id = doc_id) %>%
    select(-doc_id) %>%
    left_join(., cases, by = join_by(cited_doc_id == case_id, cited_date_decision == date_decision)) %>%
    mutate(cited_doc_id = doc_id) %>%
    select(-doc_id) %>%
    drop_na(c(cited_doc_id, citing_doc_id)) %>%
    filter(!is.na(quality) & cited_court == "Ústavní soud" & quality != "Neuvedeno") %>%
    select(citing_doc_id, cited_doc_id, quality) %>%
    mutate(quality = case_when(quality %in% positive_citation ~ 1,
                               quality %in% negative_citation ~ 0)) %>%
    rename(cited = cited_doc_id,
           citing = citing_doc_id) %>% 
    mutate(citing = factor(citing, levels = union(citing, cited)), 
           cited = factor(cited, levels = union(citing, cited)))
}

# EXPLORATORY ANALYSIS
# without_citation = round(nrow(data %>%
#   filter(is.na(cited_doc_id)))/length(unique(data$citing_doc_id)), digits = 2)
# 
# exploratory_citations = data %>%
#   filter(cited_court == "Ústavní soud" & citing_type_decision == "Nález") %>%
#   group_by(quality) %>%
#   count() %>%
#   ungroup() %>%
#   mutate(freq = round(n/sum(n), 3))

# write_rds(tar_read(CiteData), file = "data/CiteData.rds")

# CiteData %>%
#   group_by(quality) %>%
#   count() %>%
#   ungroup() %>%
#   mutate(freq = round(n/sum(n), digits = 2))
# 












