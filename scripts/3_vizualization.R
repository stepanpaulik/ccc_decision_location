library(targets)
library(tidyverse); theme_set(theme_minimal())
library(gghighlight)
library(fixest)
library(patchwork)

# concerned_acts_tofilter = c("II. ÚS 78/19", "IV. ÚS 3092/16")
# acts = read_rds("../data/ccc_database/rds/ccc_references.rds") |>
#   distinct(doc_id, concerned_act) |>
#   filter(concerned_act %in% concerned_acts_tofilter) |>
#   left_join(read_rds("../data/ccc_database/rds/ccc_metadata.rds") |> select(doc_id, popular_name, subject_register, subject_proceedings) |> mutate(across(everything(), as.character)), by = join_by(doc_id == doc_id)) |>
#   mutate(consumer = if_else(str_detect(subject_register, "spotřebitel"), 1, 0))

# CONSUMER -------------------------------------------------------------
cases_consumer = tar_read(cases_consumer)

data_consumer = tar_read(data_consumer)
data_consumer_onlydelays = tar_read(data_consumer_onlydelays)
data_consumer_wodelays = tar_read(data_consumer_wodelays)
data_consumer_ldgm = tar_read(data_consumer_ldgm)
data_consumer_gm = tar_read(data_consumer_gm)
data_consumer_ld = tar_read(data_consumer_ld)

model_consumer = tar_read(fitted_model_consumer)
model_consumer_onlydelays = tar_read(fitted_model_consumer_onlydelays) 
model_consumer_wodelays = tar_read(fitted_model_consumer_wodelays)
model_consumer_ldgm = tar_read(fitted_model_consumer_ldgm)
model_consumer_gm = tar_read(fitted_model_consumer_gm)
model_consumer_ld = tar_read(fitted_model_consumer_ld)

result_consumer = model_consumer |>
  filter(sign(higher) == sign(lower))
result_consumer

result_consumer_onlydelays = model_consumer_onlydelays |>
  filter(sign(higher) == sign(lower))
result_consumer_onlydelays

result_consumer_wodelays = model_consumer_wodelays |>
  filter(sign(higher) == sign(lower))
result_consumer_wodelays

result_consumer_ldgm = model_consumer_ldgm |>
  filter(sign(higher) == sign(lower))
result_consumer_ldgm

result_consumer_gm = model_consumer_gm |>
  filter(sign(higher) == sign(lower))
result_consumer_gm

result_consumer_ld = model_consumer_ld |>
  filter(sign(higher) == sign(lower))
result_consumer_ld
# output_consumer = model_consumer |>
#   left_join(read_rds("../data/ccc_database/rds/ccc_compositions.rds"), by = join_by(doc_id == doc_id))
# 
# output_consumer = model_consumer |>
#   left_join(model_poisson |>
#   left_join(read_rds("../data/ccc_database/rds/ccc_compositions.rds"), by = join_by(doc_id == doc_id)) |>
#   group_by(doc_id) |>
#   summarise(simackova = if_else(any(judge_name %in% "Kateřina Šimáčková"), "With Šimáčková", "Without Šimnáčková")))
#   

plot_consumer_all = model_consumer |>
  ggplot(mapping = aes(x = reorder(case_id, date_decision), y = theta)) +
  geom_pointrange(aes(ymin = lower, ymax = higher)) +
  gghighlight(sign(higher) == sign(lower) & sign(higher) == -1) +
  geom_hline(yintercept = 0, linetype="dashed", color = "purple") +
  coord_flip()  +
  labs(y = "Estimated location of a decision",
       x = NULL)

plot_consumer_onlydelays = model_consumer_onlydelays |>
  ggplot(mapping = aes(x = reorder(case_id, date_decision), y = theta)) +
  geom_pointrange(aes(ymin = lower, ymax = higher)) +  
  geom_hline(yintercept = 0, linetype="dashed", color = "purple") +
  coord_flip()  +
  labs(y = "Estimated location of a decision",
       x = NULL)

plot_consumer_all_onlydelays = plot_consumer_all + plot_consumer_onlydelays + plot_annotation(tag_levels = 'A')
plot_consumer_all_onlydelays

plot_consumer_wodelays = model_consumer_wodelays |>
  ggplot(mapping = aes(x = reorder(case_id, date_decision), y = theta)) +
  geom_pointrange(aes(ymin = lower, ymax = higher)) +  
  gghighlight(sign(higher) == sign(lower)) +
  geom_hline(yintercept = 0, linetype="dashed", color = "purple") +
  coord_flip()  +
  labs(y = "Estimated location of a decision",
       x = NULL)
plot_consumer_wodelays

plot_consumer_ldgm = model_consumer_ldgm |>
  ggplot(mapping = aes(x = reorder(case_id, date_decision), y = theta)) +
  geom_pointrange(aes(ymin = lower, ymax = higher)) +
  gghighlight(sign(higher) == sign(lower)) +
  geom_hline(yintercept = 0, linetype="dashed", color = "purple") +
  coord_flip()  +
  labs(y = "Estimated location of a decision",
       x = NULL)

plot_consumer_gm = model_consumer_gm |>
  ggplot(mapping = aes(x = reorder(case_id, date_decision), y = theta)) +
  geom_pointrange(aes(ymin = lower, ymax = higher)) +  
  geom_hline(yintercept = 0, linetype="dashed", color = "purple") +
  coord_flip()  +
  labs(y = "Estimated location of a decision",
       x = NULL)

tar_read(fitted_model_consumer_gm_cosine) |>
  ggplot(mapping = aes(x = reorder(case_id, date_decision), y = theta)) +
  geom_pointrange(aes(ymin = lower, ymax = higher)) +  
  geom_hline(yintercept = 0, linetype="dashed", color = "purple") +
  coord_flip()  +
  labs(y = "Estimated location of a decision",
       x = NULL)

plot_consumer_ld = model_consumer_ld |>
  ggplot(mapping = aes(x = reorder(case_id, date_decision), y = theta)) +
  geom_pointrange(aes(ymin = lower, ymax = higher)) +  
  geom_hline(yintercept = 0, linetype="dashed", color = "purple") +
  coord_flip()  +
  labs(y = "Estimated location of a decision",
       x = NULL)

plot_consumer_ldgm_combined = plot_consumer_wodelays + plot_consumer_gm + plot_consumer_ld + plot_annotation(tag_levels = 'A')
plot_consumer_ldgm_combined

# output_consumer |> 
#   ggplot(mapping = aes(x = reorder(case_id, date_decision), y = theta, color = simackova)) +
#   geom_pointrange(aes(ymin = lower, ymax = higher)) +
#   coord_flip()  +
#   labs(y = "Estimated location of a decision",
#        x = NULL,
#        title = "Overview of all decisions")

data_winrate_consumer = read_rds("../data/ccc_database/rds/ccc_metadata.rds") |>
  mutate(subject_register = as.character(subject_register)) |>
  filter(str_detect(subject_register, "spotřebitel")) |>
  mutate(outcome = if_else(outcome %in% "granted", 1, 0),
         judge_rapporteur_name = as_factor(judge_rapporteur_name))


table_winrate_consumer = read_rds("../data/ccc_database/rds/ccc_metadata.rds") |>
  mutate(subject_register = as.character(subject_register)) |>
  filter(str_detect(subject_register, "spotřebitel")) |> 
  group_by(judge_rapporteur_name, outcome) |>
  count() |>
  group_by(judge_rapporteur_name) |>
  summarise(outcome = outcome,
            n = n,
            decisions = sum(n),
            freq = n/sum(n)) |>
  filter(outcome %in% "granted") |>
  arrange(desc(freq)) |>
  select(judge_rapporteur_name, freq, decisions) |>
  left_join(read_rds("../data/ccc_database/rds/ccc_metadata.rds") |>
              group_by(judge_rapporteur_name, outcome) |>
              count() |>
              group_by(judge_rapporteur_name) |>
              summarise(outcome = outcome,
                        decisions_total = sum(n),
                        freq_total = n/sum(n)) |>
              filter(outcome %in% "granted") |>
              select(judge_rapporteur_name, freq_total, decisions_total))
table_winrate_consumer



plot_winrate_consumer = table_winrate_consumer |>
  ggplot(aes(x = reorder(judge_rapporteur_name, freq), y = freq)) +
  geom_point(colour = "black") +
  geom_point(aes(x = judge_rapporteur_name, y = freq_total), colour = "grey") +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Judge Rapporteur", y = "Winrate") +
  coord_flip()
plot_winrate_consumer



model_consumer_winrate_fe = feglm(
  fml = outcome ~ 1 | judge_rapporteur_name,
  data = data_winrate_consumer,
  # cluster = "formation",
  family = "binomial"
)

model_consumer_winrate = feglm(
  fml = outcome ~ 1 + judge_rapporteur_name,
  data = data_winrate_consumer,
  # cluster = "formation",
  family = "binomial"
)

coef_df  = as.data.frame(summary(model_consumer_winrate)$coeftable) |>
  mutate(exp_coef = exp(Estimate)) |>
  rownames_to_column() |>
  mutate(rowname = str_remove(rowname, pattern = "judge_rapporteur_name"))

coef_df |>
  ggplot(aes(x = rowname, y = exp_coef)) +
  geom_point() +
  geom_errorbar(aes(ymin = exp(Estimate - 1.96 * `Std. Error`),
                    ymax = exp(Estimate + 1.96 * `Std. Error`),
                    width = 0.2)) +
  labs(x = "Variable", y = "Exponentiated Coefficient (Odds Ratio)") +
  theme_minimal() +
  coord_flip()


modelsummary::modelsummary(
  model_consumer_winrate,
  estimate = "{estimate}{stars}",
  statistic = "{p.value} [{conf.low}, {conf.high}]",
  exponentiate = TRUE,
  stars = TRUE)



plot(fixef(model_consumer_winrate_fe))

# OZV ---------------------------------------------------------------------
cases_ozv = tar_read(cases_ozv)
data_ozv = tar_read(data_ozv)

model_ozv = tar_read(fitted_model_ozv)

# write_rds(model_ozv, "data/model_ozv.rds")

plot_ozv = model_ozv |>
  ggplot(mapping = aes(x = reorder(case_id, date_decision), y = theta)) +
  geom_pointrange(aes(ymin = lower, ymax = higher)) +
  coord_flip()  +
  geom_pointrange(aes(ymin = lower, ymax = higher)) + 
  geom_vline(xintercept = "Pl.ÚS 45/06", linetype="dashed", color = "purple") +
  labs(y = "Estimated location of a decision",
       x = NULL,
       title = "Overview of all decisions")
plot_ozv

result_ozv = model_ozv |>
  filter(sign(higher) == sign(lower))


# INVESTIGATION -----------------------------------------------------------
# cases_investigation = tar_read(cases_investigation)
# 
doc_ids_article_conservative = c("ECLI:CZ:US:2003:3.US.8.03", "ECLI:CZ:US:1997:2.US.361.96", "ECLI:CZ:US:1999:1.US.84.99", "ECLI:CZ:US:2004:3.US.587.04", "ECLI:CZ:US:2000:1.US.249.2000", "ECLI:CZ:US:2007:4.US.264.06", "ECLI:CZ:US:2015:3.US.3835.14.1", "ECLI:CZ:US:2007:2.US.303.05.1", "ECLI:CZ:US:2016:4.US.3113.15.1")
doc_ids_article_progressive = c("ECLI:CZ:US:2013:1.US.2886.13.1", "ECLI:CZ:US:2014:1.US.3196.12.1", "ECLI:CZ:US:2015:2.US.3626.13.1", "ECLI:CZ:US:2016:2.US.3436.14.1", "ECLI:CZ:US:2015:1.US.1565.14.1")
# 
# model_investigation = tar_read(fitted_model_investigation) |>
#   mutate(article = case_when(doc_id %in% doc_ids_article_conservative ~ "conservative",
#                              doc_id %in% doc_ids_article_progressive ~ "progressive",
#                              .default = "rest"))
# 
# model_investigation
# 
# result_investigation = model_investigation |>
#   filter(sign(higher) == sign(lower))
# result_investigation  
# 
# plot_investigation = model_investigation |>
#   ggplot(mapping = aes(x = reorder(case_id, date_decision), y = theta)) +
#   geom_pointrange(aes(ymin = lower, ymax = higher)) +
#   gghighlight(sign(higher) == sign(lower)) +
#   coord_flip()  +
#   labs(y = "Estimated location of a decision",
#        x = NULL,
#        title = "Overview of all decisions")
# plot_investigation

data_investigation_article = tar_read(data_investigation_article)

model_investigation_article = tar_read(fitted_model_investigation_article) |>
  mutate(article = case_when(doc_id %in% doc_ids_article_conservative ~ "Restraint",
                             doc_id %in% doc_ids_article_progressive ~ "Active",
                             .default = "Rest"))

result_investigation_article = model_investigation_article |>
  filter(sign(higher) == sign(lower))
result_investigation_article

plot_investigation_article = model_investigation_article |>
  ggplot(mapping = aes(x = reorder(case_id, date_decision), y = theta, color = article)) +
  geom_pointrange(aes(ymin = lower, ymax = higher)) +
  coord_flip()  +
  scale_color_grey() +
  geom_hline(yintercept = 0, linetype="dashed", color = "purple") +
  theme(legend.title = element_blank()) +
  labs(y = "Estimated location of a decision",
       x = NULL,
       title = "Overview of all decisions") 
plot_investigation_article

acts_investigation = read_rds("../data/ccc_database/rds/ccc_references.rds") |>
  filter(doc_id %in% model_investigation_article$doc_id & act_type %in% "ordinary_act")
acts_investigation

# data_investigation_article_3rdterm = tar_read(data_investigation_article_3rdterm)
# 
# model_investigation_article_3rdterm = tar_read(fitted_model_investigation_article_3rdterm) |>
#   mutate(article = case_when(doc_id %in% doc_ids_article_conservative ~ "conservative",
#                              doc_id %in% doc_ids_article_progressive ~ "progressive",
#                              .default = "rest"))
# 
# result_investigation_article_3rdterm = model_investigation_article_3rdterm |>
#   filter(sign(higher) == sign(lower))
# result_investigation_article_3rdterm
# 
# plot_investigation_article_3rdterm = model_investigation_article_3rdterm |>
#   ggplot(mapping = aes(x = reorder(case_id, date_decision), y = theta, color = article)) +
#   geom_pointrange(aes(ymin = lower, ymax = higher)) +
#   # gghighlight(sign(highe r) == sign(lower)) +
#   coord_flip()  +
#   labs(y = "Estimated location of a decision",
#        x = NULL,
#        title = "Overview of all decisions")
# plot_investigation_article_3rdterm


# CUSTODY EXAMPLE ---------------------------------------------------------
model_custody = targets::tar_read(fitted_model_custody)

plot_custody = model_custody |>
  ggplot(mapping = aes(x = reorder(case_id, date_decision), y = theta)) +
  geom_pointrange(aes(ymin = lower, ymax = higher)) +
  coord_flip()  +
  geom_vline(xintercept = "II.ÚS 3765/11", linetype="dashed", color = "purple") +
  labs(y = "Estimated location of a decision.",
       x = NULL)

# DATA SUMMARY -------------------------------------------------------------
table_data_summary_investigation = data_investigation_article |>
  rownames_to_column() |>
  rename(doc_id = rowname) |>
  group_by(doc_id) |>
  summarise(sum = rowSums(across(where(is.numeric)))) |>
  ungroup() |>
  summarise(subject_matter = "Effective Investigation", 
            n = n(),
            avg_citations = mean(sum),
            median_citations = median(sum),
            sd_citations = sd(sum))

table_data_summary_consumer = data_consumer |>
  rownames_to_column() |>
  rename(doc_id = rowname) |>
  group_by(doc_id) |>
  summarise(sum = rowSums(across(where(is.numeric)))) |>
  ungroup() |>
  summarise(subject_matter = "Consumer Protection", 
            n = n(),
            avg_citations = mean(sum),
            median_citations = median(sum),
            sd_citations = sd(sum))

table_data_summary_ozv = data_ozv |>
  rownames_to_column() |>
  rename(doc_id = rowname) |>
  group_by(doc_id) |>
  summarise(sum = rowSums(across(where(is.numeric)))) |>
  ungroup() |>
  summarise(subject_matter = "GDR", 
            n = n(),
            avg_citations = mean(sum),
            median_citations = median(sum),
            sd_citations = sd(sum))


table_data_summary = rbind(
  table_data_summary_ozv,
  table_data_summary_consumer,
  table_data_summary_investigation
)
table_data_summary

save.image("data/report/vizualization.RData")


# RESTITUTIONS ------------------------------------------------------------
# cases_restitution_2nd = tar_read(cases_restitution_2nd)
# cases_restitution = tar_read(cases_restitution)
# 
# data_restition = tar_read(data_restitution)
# data_restition_2nd = tar_read(data_restitution_2nd)
# 
# test = tar_read(fitted_model_restitution_2nd) |>
#   filter(sign(higher) == sign(lower)) |>
#   left_join(read_rds("../data/ccc_database/rds/ccc_metadata.rds"))
# 
# tar_read(fitted_model_restitution) |>
#   filter(sign(higher) == sign(lower))
# 
# tar_read(fitted_model_restitution) |>
#   ggplot(mapping = aes(x = reorder(case_id, date_decision), y = theta)) +
#   geom_pointrange(aes(ymin = lower, ymax = higher)) +
#   coord_flip()  +
#   labs(y = "Estimated location of a decision",
#        x = NULL,
#        title = "Overview of all decisions")
# 
# tar_read(fitted_model_restitution_2nd) |>
#   ggplot(mapping = aes(x = reorder(case_id, date_decision), y = theta)) +
#   geom_pointrange(aes(ymin = lower, ymax = higher)) +
#   coord_flip()  +
#   labs(y = "Estimated location of a decision",
#        x = NULL,
#        title = "Overview of all decisions")