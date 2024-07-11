library(targets)
library(tarchetypes)
library(furrr)
source("scripts/data_wrangling.R", chdir = T)
source("scripts/model.R", chdir = T)

metadata = readr::read_rds("../data/ccc_database/rds/ccc_metadata.rds") %>%
  select(doc_id, case_id, date_decision, field_register, formation) %>%
  unnest_longer(field_register)

field_subjects = unique(metadata$field_register)

field_subjects_data = map(.x = field_subjects, ~filter(.data = metadata, field_register == .x))Q

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
         citing_date_decision = ymd(citing_date_decision),
         cited_doc_id = str_replace(string = cited_doc_id, pattern = " ÚS", replacement = "ÚS"),
         cited_date_decision = ymd(cited_date_decision)) %>%
  mutate(citing_doc_id = str_remove(string = citing_doc_id, pattern = "-[0-9]+"),
         cited_doc_id = str_remove(string = cited_doc_id, pattern = "-[0-9]+")) %>%
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

field_subjects_ratios = field_subjects_ratios %>%
  filter(field_subjects_ratios$data$n > 100)

subset_data = function(metadata, subject){
  # Creates a DF with unique doc_ids as well as filtered restitution cases
    output = metadata %>%
      filter(grepl(subject, field_register)) %>%
      select(doc_id, case_id, date_decision)
  return(output)
}

finalise_data = function(file, cases) {
    file %>%
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
    left_join(., cases, by = join_by(citing_doc_id == case_id, citing_date_decision == date_decision)) %>%
    mutate(citing_doc_id = doc_id) %>%
    select(-doc_id) %>%
    left_join(., cases, by = join_by(cited_doc_id == case_id, cited_date_decision == date_decision)) %>%
    mutate(cited_doc_id = doc_id) %>%
    select(-doc_id) %>%
    drop_na(c(cited_doc_id, citing_doc_id))
}

fit_model = function(data, model_specification, metadata) {
  MCMCAdapt = 1000
  MCMCBurn = 1000
  MCMCSample = 100000
  MCMCThin = 10
  
  UniqueCaseIDs = factor(union(data$citing, data$cited))
  
  IntExtIDMap = UniqueCaseIDs
  ExtIntIDMap = c()
  for (i in 1:length(UniqueCaseIDs)) {
    ExtIntIDMap[UniqueCaseIDs[i]] = i
  }
  
  NewCaseID = ExtIntIDMap[data$citing]
  PrecedentID = ExtIntIDMap[data$cited]
  Z = data$quality
  OpinionCount = length(UniqueCaseIDs)
  CitationCount = length(Z)
  
  model.jags = jags.model(file = model_specification,
                          n.adapt = MCMCAdapt,
                          data = environment())
  update(model.jags, MCMCBurn)
  model = jags.samples(model.jags,
                       c("x.opinion", "lambda", "kappa"),
                       n.iter = MCMCSample,
                       thin = MCMCThin)
  x.opinion.chain = model$x.opinion[1:dim(model$x.opinion)[1], , 1]
  lambda.chain = model$lambda[1, , 1]
  kappa.chain = model$kappa[1, , 1]
  
  final_data = tibble(
    x.opinion.est = rowMeans(x.opinion.chain),
    x.opinion.se = sd(t(x.opinion.chain)),
    x.opinion.weights = 1 / (x.opinion.se) ^ 2,
    lambda.est = mean(lambda.chain),
    kappa.est = mean(kappa.chain),
    lambda.se = sd(lambda.chain),
    kappa.se = sd(kappa.chain)
  ) %>%
    bind_cols(
      .,
      apply(x.opinion.chain, 1, quantile, c(0.025, 0.5, 0.975)) %>%
        t() %>%
        as_tibble() %>%
        rename(
          "x.opinion.025" = "2.5%",
          "x.opinion.5" = "50%",
          "x.opinion.975" = "97.5%"
        )
    ) %>%
    bind_cols(tibble(IntExtIDMap), .) %>%
    rename(doc_id = IntExtIDMap) %>%
    left_join(
      .,
      metadata %>%
        mutate(year_decision = factor(year(date_decision))) %>%
        select(doc_id, case_id, year_decision, formation),
      join_by(doc_id)
    )
  return(final_data)
}

citations = readxl::read_xlsx("data/US_Cituje.xlsx")
model_specification = "scripts/ModelUS.txt"
plan(multisession, workers = parallel::detectCores() - 2)
new_data = future_map(field_subjects_ratios$names, ~subset_data(metadata = metadata, subject = .x) %>%
                 finalise_data(file = citations, cases = .) %>%
                 prepare_cite_data(data = .) %>% 
                   fit_model(data = ., model_specification = model_specification, metadata = read_rds("../data/US_metadata.rds")), .progress = TRUE)

final = map(.x = new_data, ~sd(.x$x.opinion.est)) %>%
   unlist() %>%
  as_tibble() %>%
  bind_cols(field_subjects_ratios %>% select(names), .) %>%
  mutate(value = round(value, digits = 4)) %>%
  rename(field_subject = names,
         inconsistency = value) %>%
  arrange(desc(inconsistency))

final_top = final %>%
  slice_head(n = 10)

final_bottom = final %>%
  slice_tail(n = 10)

save.image(file = "data/all_subjects.RData")
