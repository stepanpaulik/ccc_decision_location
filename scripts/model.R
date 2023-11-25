### Load Libraries ###
library(rjags)
library(car)
library(tidyverse)

# library(rstan)

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
      read_rds(metadata) %>%
        mutate(year_decision = factor(year(date_decision))) %>%
        select(doc_id, case_id, year_decision, formation),
      join_by(doc_id)
    )
  return(final_data)
}


# CiteCount = rep(NA, length(x.opinion.est))
# CitedCount = rep(NA, length(x.opinion.est))
# for (i in 1:OpinionCount) {
#   CiteCount[i] = sum(NewCaseID == i)
#   CitedCount[i] = sum(PrecedentID == i)
# }

