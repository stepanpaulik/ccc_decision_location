library(targets)
library(tarchetypes)
source("scripts/1_data_wrangling.R", chdir = T)
source("scripts/2b_model_engst.R", chdir = T)
tar_option_set(packages = c("tidyverse"))


list(
  tar_target(metadata, "../data/ccc_database/rds/ccc_metadata.rds", format = "file"),
  tar_target(texts, "../data/ccc_database/rds/ccc_texts.rds", format = "file"),
  tar_target(acts, "../data/ccc_database/rds/ccc_references.rds", format = "file"),
  tar_target(subject_matter, "../data/ccc_database/rds/ccc_subject_matter.rds", format = "file"),
  tar_target(input, "data/US_Cituje.xlsx", format = "file"),
  tar_target(data_location, "../data/2b_model/"),
  # tar_target(model_file, "scripts/code_helpers/stan_poisson_1D_decenter_anchors.stan", format = "file"),
  # tar_target(model_file_pre_run, 'scripts/code_helpers/stan_poisson_pre_run.stan', format = "file"),
  tar_target(cases_custody, subset_data(file = metadata, file_subject_matter = subject_matter, subject = "právo dítěte na rodičovskou výchovu a péči")),
  tar_target(cases_discrimination, subset_data(file = metadata, file_subject_matter = subject_matter, subject = "diskriminace")),
  tar_target(cases_restitution, subset_data(file = metadata, file_subject_matter = subject_matter, subject = "restitu", subject2 = "církev")),
  tar_target(cases_restitution_2nd, subset_data(file = metadata, file_subject_matter = subject_matter, year = 2013, subject = "restitu", subject2 = "církev", only_merits = TRUE)),
  tar_target(cases_ozv, {
    
    read_rds(metadata) |>
      mutate(subject_register = as.character(subject_register),
             subject_proceedings = as.character(subject_proceedings),
             subject_matter = paste0(subject_register, subject_proceedings)) |>
      filter(str_detect(subject_matter, "becně závazná vyhláška") | str_detect(popular_name, "(Vyhláška města)|(Vyhláška obce)|(becně závazná vyhláška)")) |>
      filter(grounds == "merits") |>
      select(doc_id, case_id, date_decision)
  }
  ),
  # tar_target(cases_ozv, subset_data(file = metadata, file_subject_matter = subject_matter, subject = "becně závazná vyhláška", only_merits = TRUE)),
  tar_target(cases_oop, subset_data(file = metadata, file_subject_matter = subject_matter, subject = "opatření obecné povahy")),
  tar_target(cases_EU, subset_data(file = metadata, file_subject_matter = subject_matter, subject = "EU")),
  tar_target(cases_consumer, subset_data(file = metadata, file_subject_matter = subject_matter, subject = "spotřebitel", to_filter = "dovolání", only_merits = TRUE)),
  tar_target(cases_investigation, subset_data(file = metadata, file_subject_matter = subject_matter, subject = "účinné vyšetřování", only_merits = F)),
  tar_target(data_custody, transform_data(cases = cases_custody, file = input, metadata = metadata, texts = texts, acts = acts)),
  tar_target(data_discrimination, transform_data(cases = cases_discrimination, file = input, metadata = metadata, texts = texts, acts = acts)),
  tar_target(data_restitution, transform_data(cases = cases_restitution, file = input, metadata = metadata, texts = texts, acts = NULL)),
  tar_target(data_restitution_2nd, transform_data(cases = cases_restitution_2nd, file = input, metadata = metadata, texts = texts, acts = NULL)),
  tar_target(data_ozv, transform_data(cases = cases_ozv, file = input, metadata = metadata, texts = texts, acts = NULL)),
  tar_target(data_oop, transform_data(cases = cases_oop, file = input, metadata = metadata, texts = texts, acts = acts)),
  tar_target(data_consumer, transform_data(cases = cases_consumer, file = input, metadata = metadata, texts = texts, acts = acts)),
  tar_target(data_consumer_wodelays, {
    model_consumer = fitted_model_consumer
    filtered_ids = model_consumer |>
      filter(sign(higher) == sign(lower)) |>
      filter(sign(higher) < 0) |>
      pull(doc_id)
    
    cases = cases_consumer |>
      filter(!doc_id %in% filtered_ids) |>
      transform_data(file = input, metadata = metadata, texts = texts, acts = acts)
  }
  ),
  tar_target(data_consumer_onlydelays, {
    model_consumer = fitted_model_consumer
    filtered_ids = model_consumer |>
      filter(sign(higher) == sign(lower)) |>
      filter(sign(higher) < 0) |>
      pull(doc_id)
    
    cases_consumer |>
      filter(doc_id %in% filtered_ids) |>
      transform_data(file = input, metadata = metadata, texts = texts, acts = acts)
  }
  ),
  tar_target(data_consumer_ldgm, {
    model_consumer = fitted_model_consumer_wodelays
    filtered_ids = model_consumer |>
      filter(sign(higher) == sign(lower)) |>
      pull(doc_id)
    
    cases_consumer |>
      filter(doc_id %in% filtered_ids) |>
      left_join(read_rds(metadata) |> select(doc_id, popular_name)) |>
      # filter(!str_detect(popular_name, "rozhodčí")) |>
      select(doc_id, case_id, date_decision) |>
      transform_data(file = input, metadata = metadata, texts = texts, acts = acts)
  }
  ),
  tar_target(data_consumer_gm, {
    cases_consumer |>
      left_join(read_rds(metadata) |> select(doc_id, subject_register)) |>
      mutate(subject_register = as.character(subject_register)) |>
      filter(str_detect(subject_register, "dobré mravy")) |>
      select(doc_id, case_id, date_decision) |>
      transform_data(file = input, metadata = metadata, texts = texts, acts = acts)
  }
  ),
  tar_target(data_consumer_gm_cosine, {
    rbind(cases_consumer |>
            left_join(read_rds(metadata) |> select(doc_id, subject_register)) |>
            mutate(subject_register = as.character(subject_register)) |>
            filter(str_detect(subject_register, "dobré mravy")) |>
            select(doc_id, case_id, date_decision),
          cases_consumer |> filter(doc_id == "ECLI:CZ:US:2018:4.US.3009.17.2") |>
            select(doc_id, case_id, date_decision)
    ) |> 
      transform_data(file = input, metadata = metadata, texts = texts, acts = acts)
  }
  ),
  tar_target(data_consumer_ld, {
    cases_consumer |>
      left_join(read_rds(metadata) |> select(doc_id, popular_name, subject_register)) |>
      mutate(subject_register = as.character(subject_register)) |>
      filter(str_detect(popular_name, "dálku")) |>
      select(doc_id, case_id, date_decision) |>
      transform_data(file = input, metadata = metadata, texts = texts, acts = acts)
  }
  ),
  tar_target(data_investigation_article, {
    doc_ids_article_conservative = c("ECLI:CZ:US:2003:3.US.8.03", "ECLI:CZ:US:1997:2.US.361.96", "ECLI:CZ:US:1999:1.US.84.99", "ECLI:CZ:US:2004:3.US.587.04", "ECLI:CZ:US:2000:1.US.249.2000", "ECLI:CZ:US:2007:4.US.264.06", "ECLI:CZ:US:2015:3.US.3835.14.1", "ECLI:CZ:US:2007:2.US.303.05.1", "ECLI:CZ:US:2016:4.US.3113.15.1")
    doc_ids_article_progressive = c("ECLI:CZ:US:2013:1.US.2886.13.1", "ECLI:CZ:US:2014:1.US.3196.12.1", "ECLI:CZ:US:2015:2.US.3626.13.1", "ECLI:CZ:US:2016:2.US.3436.14.1", "ECLI:CZ:US:2015:1.US.1565.14.1")
    read_rds(metadata) |> 
      filter(doc_id %in% c(doc_ids_article_conservative, doc_ids_article_progressive)) |>
      select(doc_id, case_id, date_decision) |>
      transform_data(file = input, metadata = metadata, texts = texts, acts = acts)
  }
  ),
  tar_target(data_investigation_article_3rdterm, {
    doc_ids_article_conservative = c("ECLI:CZ:US:2003:3.US.8.03", "ECLI:CZ:US:1997:2.US.361.96", "ECLI:CZ:US:1999:1.US.84.99", "ECLI:CZ:US:2004:3.US.587.04", "ECLI:CZ:US:2000:1.US.249.2000", "ECLI:CZ:US:2007:4.US.264.06", "ECLI:CZ:US:2015:3.US.3835.14.1", "ECLI:CZ:US:2007:2.US.303.05.1", "ECLI:CZ:US:2016:4.US.3113.15.1")
    doc_ids_article_progressive = c("ECLI:CZ:US:2013:1.US.2886.13.1", "ECLI:CZ:US:2014:1.US.3196.12.1", "ECLI:CZ:US:2015:2.US.3626.13.1", "ECLI:CZ:US:2016:2.US.3436.14.1", "ECLI:CZ:US:2015:1.US.1565.14.1")
    read_rds(metadata) |> 
      filter(doc_id %in% c(doc_ids_article_conservative, doc_ids_article_progressive)) |>
      select(doc_id, case_id, date_decision) |>
      filter(year(date_decision) > 2012) |>
      transform_data(file = input, metadata = metadata, texts = texts, acts = NULL)
  }
  ),
  tar_target(data_EU, transform_data(cases = cases_EU, file = input, metadata = metadata, texts = texts, acts = acts)),
  tar_target(data_investigation, transform_data(cases = cases_investigation, file = input, metadata = metadata, texts = texts, acts = acts)),
  tar_target(data_input_custody, reshape_data(data_custody, filtered = TRUE)),
  tar_target(data_input_discrimination, reshape_data(data_discrimination, filtered = TRUE)),
  tar_target(data_input_restitution, reshape_data(data_restitution, filtered = TRUE)),
  tar_target(data_input_restitution_2nd, reshape_data(data_restitution_2nd, filtered = TRUE)),
  tar_target(data_input_ozv, reshape_data(data_ozv, filtered = TRUE)),
  tar_target(data_input_oop, reshape_data(data_oop, filtered = TRUE)),
  tar_target(data_input_consumer, reshape_data(data_consumer, filtered = TRUE)),
  tar_target(data_input_consumer_wodelays, reshape_data(data_consumer_wodelays, filtered = TRUE)),
  tar_target(data_input_consumer_onlydelays, reshape_data(data_consumer_onlydelays, filtered = TRUE)),
  tar_target(data_input_consumer_ldgm, reshape_data(data_consumer_ldgm, filtered = TRUE)),
  tar_target(data_input_consumer_gm, reshape_data(data_consumer_gm, filtered = TRUE)),
  tar_target(data_input_consumer_gm_cosine, reshape_data(data_consumer_gm_cosine, filtered = TRUE)),
  tar_target(data_input_consumer_ld, reshape_data(data_consumer_ld, filtered = TRUE)),
  tar_target(data_input_investigation, reshape_data(data_investigation, filtered = TRUE)),
  tar_target(data_input_investigation_article, reshape_data(data_investigation_article, filtered = TRUE)),
  tar_target(data_input_investigation_article_3rdterm, reshape_data(data_investigation_article_3rdterm, filtered = TRUE)),
  tar_target(data_input_EU, reshape_data(data_EU, filtered = TRUE)),
  tar_target(fitted_model_custody, fit_model(data = data_input_custody, metadata = metadata)),
  # tar_target(fitted_model_discrimination, fit_model(data = data_input_discrimination)),
  # tar_target(fitted_model_restitution, fit_model(data = data_input_restitution, metadata = metadata)),
  # tar_target(fitted_model_restitution_2nd, fit_model(data = data_input_restitution_2nd, metadata = metadata)),
  tar_target(fitted_model_ozv, fit_model(data = data_input_ozv, metadata = metadata)),
  # tar_target(fitted_model_oop, fit_model(data = data_input_oop, metadata = metadata)),
  tar_target(fitted_model_consumer, fit_model(data = data_input_consumer, metadata = metadata)),
  tar_target(fitted_model_consumer_onlydelays, fit_model(data = data_input_consumer_onlydelays, metadata = metadata)),
  tar_target(fitted_model_consumer_wodelays, fit_model(data = data_input_consumer_wodelays, metadata = metadata)),
  tar_target(fitted_model_consumer_ldgm, fit_model(data = data_input_consumer_ldgm, metadata = metadata)),
  tar_target(fitted_model_consumer_gm, fit_model(data = data_input_consumer_gm, metadata = metadata)),
  tar_target(fitted_model_consumer_gm_cosine, fit_model(data = data_input_consumer_gm_cosine, metadata = metadata)),
  tar_target(fitted_model_consumer_ld, fit_model(data = data_input_consumer_ld, metadata = metadata)),
  # tar_target(fitted_model_investigation, fit_model(data = data_input_investigation, metadata = metadata)),
  tar_target(fitted_model_investigation_article, fit_model(data = data_input_investigation_article, metadata = metadata)),
  tar_target(fitted_model_investigation_article_3rdterm, fit_model(data = data_input_investigation_article_3rdterm, metadata = metadata))
  # tar_target(fitted_model_EU, fit_model(data = data_input_EU))
)
