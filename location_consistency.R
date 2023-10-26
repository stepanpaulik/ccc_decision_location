library(tidyverse)

data = data = readxl::read_xlsx(path = "data/US_Cituje.xlsx") %>%
  rename(citing_doc_id = "Sp. zn.",
         citing_date_decision = "Ze dne",
         citing_type_decision = "Druh",
         citing_court = "Soud",
         cited_doc_id = "Cituje",
         cited_date_decision = "Ze dne citováno",
         cited_type_decision = "Druh citováno",
         cited_court = "Soud citováno",
         quality = "Kvalita",
         database = "Máme citované J v db")

unique(data$citing_court)

without_citation = round(nrow(data %>% 
  filter(is.na(cited_doc_id)))/length(unique(data$citing_doc_id)), digits = 2)

exploratory_citations = data %>%
  filter(cited_court == "Ústavní soud" & citing_type_decision == "Nález") %>%
  group_by(quality) %>%
  count() %>%
  ungroup() %>%
  mutate(freq = round(n/sum(n), 3))





