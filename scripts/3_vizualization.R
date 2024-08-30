library(targets)
library(tidyverse)

# RESTITUTIONS ------------------------------------------------------------
cases_restitution_2nd = tar_read(cases_restitution_2nd)
cases_restitution = tar_read(cases_restitution)

data_restition = tar_read(data_restitution)
data_restition_2nd = tar_read(data_restitution_2nd)

tar_read(fitted_model_restitution_2nd) |>
  filter(sign(higher) == sign(lower))

tar_read(fitted_model_restitution) |>
  filter(sign(higher) == sign(lower))


tar_read(fitted_model_restitution) |>
  ggplot(mapping = aes(x = doc_id, y = theta)) +
  geom_pointrange(aes(ymin = lower, ymax = higher)) +
  coord_flip()  +
  labs(y = "Estimated location of a decision",
       x = NULL,
       title = "Overview of all decisions")

tar_read(fitted_model_restitution_2nd) |>
  ggplot(mapping = aes(x = doc_id, y = theta)) +
  geom_pointrange(aes(ymin = lower, ymax = higher)) +
  coord_flip()  +
  labs(y = "Estimated location of a decision",
       x = NULL,
       title = "Overview of all decisions")

# OZV ---------------------------------------------------------------------
cases_ozv = tar_read(cases_ozv)
data_ozv = tar_read(data_ozv)

fitted_model_ozv = tar_read(fitted_model_ozv)

fitted_model_ozv |>
  ggplot(mapping = aes(x = doc_id, y = theta)) +
  geom_pointrange(aes(ymin = lower, ymax = higher)) +
  coord_flip()  +
  labs(y = "Estimated location of a decision",
       x = NULL,
       title = "Overview of all decisions")

fitted_model_ozv |>
  filter(sign(higher) == sign(lower))

# SPOTREBITEl -------------------------------------------------------------
cases_spotr = tar_read(cases_spotr)

data_spotr = tar_read(data_spotr)
data_spotr_filtered = tar_read(data_spotr_filtered)

fitted_model_spotr = tar_read(fitted_model_spotr)
fitted_model_spotr_filtered = tar_read(fitted_model_spotr_filtered)

fitted_model_spotr |>
  filter(sign(higher) == sign(lower))

fitted_model_spotr_filtered |>
  filter(sign(higher) == sign(lower))

# output_spotr = fitted_model_spotr |>
#   left_join(read_rds("../data/ccc_database/rds/ccc_compositions.rds"), by = join_by(doc_id == doc_id))
# 
# output_spotr = fitted_model_spotr |>
#   left_join(model_poisson |>
#   left_join(read_rds("../data/ccc_database/rds/ccc_compositions.rds"), by = join_by(doc_id == doc_id)) |>
#   group_by(doc_id) |>
#   summarise(simackova = if_else(any(judge_name %in% "Kateřina Šimáčková"), "With Šimáčková", "Without Šimnáčková")))
#   

fitted_model_spotr |>
  ggplot(mapping = aes(x = doc_id, y = theta)) +
  geom_pointrange(aes(ymin = lower, ymax = higher)) +
  coord_flip()  +
  labs(y = "Estimated location of a decision",
       x = NULL,
       title = "Overview of all decisions")

fitted_model_spotr_filtered |>
  ggplot(mapping = aes(x = doc_id, y = theta)) +
  geom_pointrange(aes(ymin = lower, ymax = higher)) +
  coord_flip()  +
  labs(y = "Estimated location of a decision",
       x = NULL,
       title = "Overview of all decisions")

# output_spotr |> 
#   ggplot(mapping = aes(x = doc_id, y = theta, color = simackova)) +
#   geom_pointrange(aes(ymin = lower, ymax = higher)) +
#   coord_flip()  +
#   labs(y = "Estimated location of a decision",
#        x = NULL,
#        title = "Overview of all decisions")



# Effective Investigation -------------------------------------------------
data = tar_read(fitted_mode_expression)

model_investigation = 
# model_bernoulli = tar_read(fitted_model_ozv_bernoulli)

tar_read(fitted_model_EU) |>
  ggplot(mapping = aes(x = doc_id, y = theta)) +
  geom_pointrange(aes(ymin = lower, ymax = higher)) +
  coord_flip()  +
  labs(y = "Estimated location of a decision",
       x = NULL,
       title = "Overview of all decisions")



# model_bernoulli |>
#   ggplot(mapping = aes(x = doc_id, y = theta)) +
#   geom_pointrange(aes(ymin = lower, ymax = higher)) +
#   coord_flip()  +
#   labs(y = "Estimated location of a decision",
#        x = NULL,
#        title = "Overview of all decisions")


data = tar_read(fitted_mode_custody) |>
  filter(sign(higher) == sign(lower)) |>
  left_join(read_rds("../data/ccc_database/rds/ccc_metadata.rds"), by = join_by(doc_id == doc_id))


tar_read(fitted_model_investigation) |>
  filter(sign(higher) == sign(lower))

tar_read(fitted_model_EU) |>
  filter(sign(higher) == sign(lower))


save.image("data/report/data_ozv.RData")
data_export_full
