library(tidyverse)
library(readxl)

metadata = read_rds("../data/US_metadata.rds") %>%
  select(doc_id, case_id, date_decision, field_register) %>%
  unnest_longer(field_register)

field_subjects = unique(metadata$field_register)

field_subjects_data = map(.x = field_subjects, ~filter(.data = metadata, field_register == .x))

positive_citation = c("Souhlasí a Následuje", "Vysvětlení", "Aplikuje a rozvíjí", "Vysvětlení", "Neaplikuje, ale souhlasí")
negative_citation = c("Citováno odlišným stanoviskem", "Nesouhlasí, neaplikuje", "Překonán")

data = readxl::read_xlsx("data/US_Cituje.xlsx") %>%
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
  mutate(citing_doc_id = str_remove(string = citing_doc_id, pattern = "-[0-9]+"),
         cited_doc_id = str_remove(string = cited_doc_id, pattern = "-[0-9]+")) %>%
  drop_na(quality) %>%
  filter(cited_court == "Ústavní soud" & quality != "Neuvedeno") %>%
  mutate(quality = case_when(quality %in% positive_citation ~ 1,
                             quality %in% negative_citation ~ 0))
field_subjects_ratios = list()
field_subjects_ratios$names = field_subjects
field_subjects_ratios$data = map(.x = field_subjects_data, ~data %>%
                          filter(citing_doc_id %in% .x$case_id) %>%
                          filter(cited_doc_id %in% .x$case_id) %>%
                          left_join(., .x, by = join_by(citing_doc_id == case_id, citing_date_decision == date_decision)) %>%
                          mutate(citing_doc_id = doc_id) %>%
                          select(-doc_id) %>%
                          left_join(., .x, by = join_by(cited_doc_id == case_id, cited_date_decision == date_decision)) %>%
                          mutate(cited_doc_id = doc_id) %>%
                          select(-doc_id) %>%
                          drop_na(cited_doc_id, citing_doc_id) %>%
                          group_by(quality) %>%
                          count() %>%
                          ungroup() %>%
                          mutate(freq = round(n/sum(n), 3),
                                 n = sum(n)) %>% 
                          filter(quality == 0) %>%
                          as_tibble())

field_subjects_ratios = field_subjects_ratios %>% 
  as_tibble() %>%
  unnest_longer(data)

# write_csv(field_subjects_ratios, "data/field_subjects_ratios.csv")

