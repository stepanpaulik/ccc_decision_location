library(targets)
library(tarchetypes)
source("scripts/1_data_wrangling.R", chdir = T)
source("scripts/2_model.R", chdir = T)
tar_option_set(packages = c("tidyverse"))

list(
  tar_target(metadata, "../data/ccc_database/rds/ccc_metadata.rds", format = "file"),
  tar_target(input, "data/US_Cituje.xlsx", format = "file"),
  tar_target(model_specification, "scripts/ModelUS.txt", format = "file"),
  tar_target(cases_discrimination, subset_data(file = metadata, subject = "discrimination")),
  tar_target(cases_restitution, subset_data(file = metadata, subject = "restitution")),
  tar_target(cases_ozv, subset_data(file = metadata, subject = "ozv")),
  tar_target(cases_oop, subset_data(file = metadata, subject = "oop")),
  tar_target(data_discriminiation, finalise_data(file = input, cases = cases_discrimination)),
  tar_target(data_restitution, finalise_data(file = input, cases = cases_restitution)),
  tar_target(data_ozv, finalise_data(file = input, cases = cases_ozv)),
  tar_target(data_oop, finalise_data(file = input, cases = cases_oop)),
  tar_target(fitted_model_discrimination, fit_model(data = data_discriminiation, model_specification = model_specification, metadata = metadata)),
  tar_target(fitted_model_restitution, fit_model(data = data_restitution, model_specification = model_specification, metadata = metadata)),
  tar_target(fitted_model_ozv, fit_model(data = data_ozv, model_specification = model_specification, metadata = metadata)),
  tar_target(fitted_model_oop, fit_model(data = data_oop, model_specification = model_specification, metadata = metadata)),
  tar_target(write_fitted_discrimination, readr::write_rds(fitted_model_discrimination, file = "data/fitted_model_discrimination.rds")),
  tar_target(write_fitted_restitution, readr::write_rds(fitted_model_restitution, file = "data/fitted_model_restitution.rds")),
  tar_target(write_fitted_ozv, readr::write_rds(fitted_model_ozv, file = "data/fitted_model_ozv.rds")),
  tar_target(write_fitted_oop, readr::write_rds(fitted_model_oop, file = "data/fitted_model_oop.rds"))
)