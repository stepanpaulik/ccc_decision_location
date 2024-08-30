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
  tar_target(cases_restitution_2nd, subset_data(file = metadata, file_subject_matter = subject_matter, year = 2013, subject = "restitu", subject2 = "církev", only_merits = FALSE)),
  tar_target(cases_ozv, subset_data(file = metadata, file_subject_matter = subject_matter, subject = "becně závazná vyhláška")),
  tar_target(cases_oop, subset_data(file = metadata, file_subject_matter = subject_matter, subject = "opatření obecné povahy")),
  tar_target(cases_EU, subset_data(file = metadata, file_subject_matter = subject_matter, subject = "EU")),
  tar_target(cases_spotr, subset_data(file = metadata, file_subject_matter = subject_matter, subject = "spotřebitel", to_filter = "dovolání", only_merits = TRUE)),
  tar_target(cases_investigation, subset_data(file = metadata, file_subject_matter = subject_matter, subject = "účinné vyšetřování")),
  tar_target(data_custody, transform_data(cases = cases_custody, file = input, metadata = metadata, texts = texts, acts = acts)),
  tar_target(data_discrimination, transform_data(cases = cases_discrimination, file = input, metadata = metadata, texts = texts, acts = acts)),
  tar_target(data_restitution, transform_data(cases = cases_restitution, file = input, metadata = metadata, texts = texts, acts = acts)),
  tar_target(data_restitution_2nd, transform_data(cases = cases_restitution_2nd, file = input, metadata = metadata, texts = texts, acts = acts)),
  tar_target(data_ozv, transform_data(cases = cases_ozv, file = input, metadata = metadata, texts = texts, acts = NULL)),
  tar_target(data_oop, transform_data(cases = cases_oop, file = input, metadata = metadata, texts = texts, acts = acts)),
  tar_target(data_spotr, transform_data(cases = cases_spotr, file = input, metadata = metadata, texts = texts, acts = acts)),
  tar_target(data_spotr_filtered, {
    model_spotr = fitted_model_spotr
    filtered_ids = model_spotr |>
      filter(sign(higher) == sign(lower)) |>
      filter(higher > 0) |>
      pull(doc_id)
    
    cases_spotr |>
      filter(doc_id %in% filtered_ids) |>
      transform_data(file = input, metadata = metadata, texts = texts, acts = acts)
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
  tar_target(data_input_spotr, reshape_data(data_spotr, filtered = TRUE)),
  tar_target(data_input_spotr_filtered, reshape_data(data_spotr_filtered, filtered = TRUE)),
  tar_target(data_input_investigation, reshape_data(data_investigation, filtered = TRUE)),
  tar_target(data_input_EU, reshape_data(data_EU, filtered = TRUE)),
  tar_target(fitted_model_custody, fit_model(data = data_input_custody)),
  # tar_target(fitted_model_discrimination, fit_model(data = data_input_discrimination)),
  tar_target(fitted_model_restitution, fit_model(data = data_input_restitution)),
  tar_target(fitted_model_restitution_2nd, fit_model(data = data_input_restitution_2nd)),
  tar_target(fitted_model_ozv, fit_model(data = data_input_ozv)),
  # tar_target(fitted_model_oop, fit_model(data = data_input_oop)),
  tar_target(fitted_model_spotr, fit_model(data = data_input_spotr)),
  tar_target(fitted_model_spotr_filtered, fit_model(data = data_input_spotr_filtered))
  # tar_target(fitted_model_investigation, fit_model(data = data_input_investigation)),
  # tar_target(fitted_model_EU, fit_model(data = data_input_EU))
  # tar_target(write_fitted_discrimination, readr::write_rds(fitted_model_discrimination, file = "data/fitted_model_discrimination.rds")),
  # tar_target(write_fitted_restitution, readr::write_rds(fitted_model_restitution, file = "data/fitted_model_restitution.rds")),
  # tar_target(write_fitted_ozv, readr::write_rds(fitted_model_ozv, file = "data/fitted_model_ozv.rds")),
  # tar_target(write_fitted_oop, readr::write_rds(fitted_model_oop, file = "data/fitted_model_oop.rds"))
)
