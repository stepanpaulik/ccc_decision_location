library(targets)
library(tarchetypes)
source("scripts/data_wrangling.R", chdir = T)
source("scripts/model.R", chdir = T)
tar_option_set(packages = c("tidyverse"))

list(
  tar_target(metadata, "../data/US_metadata.rds", format = "file"),
  tar_target(input, "data/US_Cituje.xlsx", format = "file"),
  tar_target(model_specification, "scripts/ModelUS.txt", format = "file"),
  tar_target(cases_discrimination, subset_data(file = metadata, subject = "discrimination")),
  tar_target(cases_restitution, subset_data(file = metadata, subject = "restitution")),
  tar_target(data_discriminiation, finalise_data(file = input, cases = cases_discrimination)),
  tar_target(data_restitution, finalise_data(file = input, cases = cases_restitution)),
  tar_target(cite_data_discrimination, prepare_cite_data(data_discriminiation)),
  tar_target(cite_data_restitution, prepare_cite_data(data_restitution)),
  tar_target(fitted_model_discrimination, fit_model(data = cite_data_discrimination, model_specification = model_specification, metadata = metadata)),
  tar_target(fitted_model_restitution, fit_model(data = cite_data_restitution, model_specification = model_specification, metadata = metadata)),
  tar_target(write_fitted_discrimination, readr::write_rds(fitted_model_discrimination, file = "data/fitted_model_discrimination.rds")),
  tar_target(write_fitted_restitution, readr::write_rds(fitted_model_restitution, file = "data/fitted_model_restitution.rds"))
)




