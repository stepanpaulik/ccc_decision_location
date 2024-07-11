library(tidyverse)
library(readxl)
library(targets)


# Number of decisions and (negative) citations by subject matter ----------
metadata = read_rds("../data/ccc_dataset/rds/ccc_metadata.rds") %>%
  select(doc_id, case_id, date_decision, subject_register) %>%
  unnest_longer(subject_register)

field_subjects = unique(metadata$subject_register)

field_subjects_data = map(.x = field_subjects, ~filter(.data = metadata, subject_register == .x))

positive_citation = c("Souhlasí a Následuje", "Vysvětlení", "Aplikuje a rozvíjí", "Vysvětlení", "Neaplikuje, ale souhlasí")
negative_citation = c("Citováno odlišným stanoviskem", "Nesouhlasí, neaplikuje", "Překonán")

data = readxl::read_xlsx("data/US_Cituje_old.xlsx") %>%
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
         citing_date_decision = as_date(citing_date_decision),
         cited_doc_id = str_replace(string = cited_doc_id, pattern = " ÚS", replacement = "ÚS"),
         cited_date_decision = as_date(cited_date_decision)) %>%
  mutate(citing_doc_id = str_remove(string = citing_doc_id, pattern = "-[0-9]+"),
         cited_doc_id = str_remove(string = cited_doc_id, pattern = "-[0-9]+")) %>%
  drop_na(quality) %>%
  filter(cited_court == "Ústavní soud" & quality != "Neuvedeno") %>%
  mutate(quality = case_when(quality %in% positive_citation ~ 1,
                             quality %in% negative_citation ~ 0))

filtered_data_subject_matter = list()
filtered_data_subject_matter$names = field_subjects
filtered_data_subject_matter$data = map(.x = field_subjects_data, ~data %>% # OPRAVIT
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
                          summarise(n = n(),
                                    n_decisions = length(unique(citing_doc_id))) %>%
                          ungroup() %>%
                          mutate(freq = round(n/sum(n), 3),
                                 n = sum(n),
                                 n_decisions = sum(n_decisions)) %>% 
                          filter(quality == 0) %>%
                          as_tibble())

filtered_data_subject_matter = filtered_data_subject_matter %>% 
  as_tibble() %>%
  unnest_longer(data)

full_data_subject_matter = list()
full_data_subject_matter$names = field_subjects
full_data_subject_matter$data = map(.x = field_subjects_data, ~data %>% # OPRAVIT
                                          filter(citing_doc_id %in% .x$case_id) %>%
                                          left_join(., .x, by = join_by(citing_doc_id == case_id, citing_date_decision == date_decision)) %>%
                                          mutate(citing_doc_id = doc_id) %>%
                                          select(-doc_id) %>%
                                          drop_na(citing_doc_id) %>%
                                          group_by(quality) %>%
                                          summarise(n = n(),
                                                    n_decisions = length(unique(citing_doc_id))) %>%
                                          ungroup() %>%
                                          mutate(freq = round(n/sum(n), 3),
                                                 n = sum(n),
                                                 n_decisions = sum(n_decisions)) %>% 
                                          filter(quality == 0) %>%
                                          as_tibble())

full_data_subject_matter = full_data_subject_matter %>% 
  as_tibble() %>%
  unnest_longer(data)

descriptive_table_filtered = tibble(
  subject_matter = filtered_data_subject_matter$names,
  filtered_n_decisions = filtered_data_subject_matter$data$n_decisions,
  filtered_n_references = filtered_data_subject_matter$data$n,
  filtered_negative_proportion = filtered_data_subject_matter$data$freq
)

descriptive_table_full = tibble(
  subject_matter = full_data_subject_matter$names,
  full_n_decisions = full_data_subject_matter$data$n_decisions,
  full_n_references = full_data_subject_matter$data$n,
  full_negative_proportion = full_data_subject_matter$data$freq
)

descriptive_table = left_join(descriptive_table_full, descriptive_table_filtered)
remove(descriptive_table_full, descriptive_table_filtered)

descriptive_table_summarized = descriptive_table |>
  summarise(
    mean_full_n_decisions = round(mean(full_n_decisions), 1),
    median_full_n_decisions = round(median(full_n_decisions), 1),
    mean_full_n_references = round(mean(full_n_references), 1),
    median_full_n_references = round(median(full_n_references), 1),
    mean_full_references_per_decision = round(mean_full_n_references/mean_full_n_decisions, 1),
    mean_filtered_n_decisions = round(mean(filtered_n_decisions, na.rm = TRUE), 1),
    median_filtered_n_decisions = round(median(filtered_n_decisions, na.rm = TRUE), 1),
    mean_filtered_n_references = round(mean(filtered_n_references, na.rm = TRUE), 1),
    median_filtered_n_references = round(median(filtered_n_references, na.rm = TRUE), 1),
    mean_filtered_references_per_decision = round(mean_filtered_n_references/mean_filtered_n_decisions, 1)
  )



# Number of citations within subject matter -------------------------------
cases = tar_read(cases_restitution)
file = "data/US_Cituje_old.xlsx"
data_location = "../data/2b_model/"

cases = read_rds("../data/ccc_dataset/rds/ccc_metadata.rds") |>
  select(doc_id, case_id, date_decision) |>
  filter(doc_id %in% cases$doc_id) |>
  left_join(read_rds("../data/ccc_dataset/rds/ccc_texts.rds"))

data = readxl::read_xlsx(file) |>
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
  drop_na(c(citing_doc_id, cited_doc_id)) |>
  mutate(cited_count = str_count(string = str_replace((str_replace(string = text, pattern = "\\n", replacement = " ")), pattern = "  ", " "), pattern = str_replace(string = cited_doc_id, pattern = "ÚS", replacement = " ÚS"))) |>
  select(citing_doc_id, cited_doc_id, cited_count) |>
  distinct() |>
  mutate(cited_count = replace(cited_count, cited_count == 0, 1))

data %>%
  mutate(cited_count = as.integer(cited_count)) |>
  ggplot(aes(x = cited_count)) +
  geom_bar() +
  scale_x_continuous(breaks = scales::breaks_pretty(20))



# write_csv(filtered_data_subject_matter, "data/filtered_data_subject_matter.csv")

# filtered_data_subject_matter = read_rds(file = "data/filtered_data_subject_matter.rsd")

save.image("report/descriptive_data.RData")

