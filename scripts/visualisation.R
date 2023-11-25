library(tidyverse); theme_set(theme_minimal())
library(patchwork)


final_data_discrimination = targets::tar_read(write_fitted_discrimination)
final_data_restitution = targets::tar_read(write_fitted_restitution)
check = targets::tar_read(cite_data_restitution)


final_table = tribble(
  ~field_subject, ~variance, ~SD,
  "Restitutions", var(final_data_restitution$x.opinion.est), sd(final_data_restitution$x.opinion.est),
  "Diskrimination", var(final_data_discrimination$x.opinion.est), sd(final_data_discrimination$x.opinion.est)
) %>% 
  mutate(across(where(is.numeric), ~round(., digits = 3)))

htmlTable::htmlTable(final_table, rnames = FALSE) %>%
  kableExtra::save_kable(file = "report/graphics/final_table.png", density = 1000)


# DISCRIMINATION ----------------------------------------------------------
disc_p1 = final_data_discrimination %>%
  ggplot(data = ., aes(x = year_decision, y = x.opinion.est)) +
  geom_point() +
  theme(
    axis.text.x = element_text(angle = 90)) +
  labs(x = "Year of the decision",
       y = "Estimated location of a decision",
       title = "Development over time")

disc_p2 = final_data_discrimination %>%
  ggplot(mapping = aes(x = x.opinion.est)) +
  geom_density() +
  facet_wrap(~ formation) +
  labs(y = NULL,
       x = NULL)

disc_p3 = final_data_discrimination %>%
  ggplot(mapping = aes(x = doc_id, y = x.opinion.est)) +
  geom_pointrange(aes(ymin = x.opinion.025, ymax = x.opinion.975)) +
  theme(axis.text.y = element_blank()) +
  coord_flip()  +
  labs(y = "Estimated location of a decision",
       x = NULL, 
       title = "Overview of all decisions")

disc_p4 = final_data_discrimination %>%
  filter(sign(x.opinion.025) == sign(x.opinion.975)) %>%
  ggplot(mapping = aes(x = doc_id, y = x.opinion.est)) +
  geom_pointrange(aes(ymin = x.opinion.025, ymax = x.opinion.975)) +
  coord_flip()  +
  labs(y = "Estimated location of a decision",
       x = NULL)

disc_combined = disc_p1 + disc_p3

ggsave(filename = "report/graphics/disc_combined.png", plot = disc_combined, dpi = 600)
ggsave(filename = "report/graphics/disc_p1.png", plot = disc_p1, dpi = 600)
ggsave(filename = "report/graphics/disc_p2.png", plot = disc_p2, dpi = 600)
ggsave(filename = "report/graphics/disc_p3.png", plot = disc_p3, dpi = 600, width = 6, height = 8)

# RESTITUTION -------------------------------------------------------------
rest_p1 = final_data_restitution %>%
  ggplot(data = ., aes(x = year_decision, y = x.opinion.est)) +
  geom_point() +
  labs(x = "Year of the decision",
       y = "Estimated location of a decision",
       title = "Development over time")

rest_p2 = final_data_restitution %>%
  ggplot(mapping = aes(x = x.opinion.est)) +
  geom_density() +
  facet_wrap(~ formation) +
  labs(y = NULL,
       x = NULL)

rest_p3 = final_data_restitution %>%
  ggplot(mapping = aes(x = doc_id, y = x.opinion.est)) +
  geom_pointrange(aes(ymin = x.opinion.025, ymax = x.opinion.975)) +
  theme(axis.text.y = element_blank()) +
  coord_flip()  +
  labs(y = "Estimated location of a decision",
       x = NULL,
       title = "Overview of all decisions")

rest_p4 = final_data_restitution %>%
  filter(sign(x.opinion.025) == sign(x.opinion.975)) %>%
  ggplot(mapping = aes(x = case_id, y = x.opinion.est)) +
  geom_pointrange(aes(ymin = x.opinion.025, ymax = x.opinion.975)) +
  coord_flip()  +
  labs(y = "Estimated location of a decision",
       x = NULL,
       title = "Overview of decisions whose location\nstatistically significantly differs from 0")

rest_combined = rest_p3 + rest_p4

# Save plots
ggsave(filename = "report/graphics/rest_combined.png", plot = rest_combined, dpi = 600)
ggsave(filename = "report/graphics/rest_p1.png", plot = rest_p1, dpi = 600)
ggsave(filename = "report/graphics/rest_p2.png", plot = rest_p2, dpi = 600)
ggsave(filename = "report/graphics/rest_p3.png", plot = rest_p3, dpi = 600, width = 6, height = 8)
ggsave(filename = "report/graphics/rest_p4.png", plot = rest_p4, dpi = 600)


