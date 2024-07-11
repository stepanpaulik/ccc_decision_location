library(tidyverse)
library(targets)
library(XML) 
library(shinystan)
library(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores()) 
library(reticulate)
library(stargazer)
library(furrr)

project.seed = 1337L

# DATA WRANGLING  ---------------------------------------------------------
metadata = read_rds("../data/ccc_database/rds/ccc_metadata.rds") %>%
  select(doc_id, case_id, date_decision, subject_register) %>%
  unnest_longer(subject_register)

subject_matters = unique(metadata$subject_register)

subject_matter_data = map(.x = subject_matters, ~filter(.data = metadata, subject_register == .x))

file = "data/US_Cituje_old.xlsx"
data_location = "../data/2b_model/"

# cases = read_rds("../data/ccc_database/rds/ccc_metadata.rds") |>
#   select(doc_id, case_id, date_decision) |>
#   filter(doc_id %in% cases$doc_id) |>
#   left_join(read_rds("../data/ccc_database/rds/ccc_texts.rds"))

data = readxl::read_xlsx(file) |>
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
         cited_doc_id = str_remove(string = cited_doc_id, pattern = "-[0-9]+")) |>
  filter(quality %in% c("Souhlasí a Následuje", "Vysvětlení", "Aplikuje a rozvíjí", "Vysvětlení", "Neaplikuje, ale souhlasí")) |>
  left_join(read_rds("../data/ccc_database/rds/ccc_metadata.rds") %>%
              select(doc_id, case_id, date_decision, subject_register, type_decision), by = join_by(citing_doc_id == case_id, citing_date_decision == date_decision, citing_type_decision == type_decision)) |>
  distinct(citing_doc_id, cited_doc_id, .keep_all = T) |> # Due to Error in the Beck database tagging both the nález and the usnesení of the cited decision
  left_join(read_rds("../data/ccc_database/rds/ccc_texts.rds")) |>
  mutate(cited_count = str_count(string = str_replace((str_replace(string = text, pattern = "\\n", replacement = " ")), pattern = "  ", " "), pattern = str_replace(string = cited_doc_id, pattern = "ÚS", replacement = " ÚS"))) |>
  select(citing_doc_id, citing_date_decision, cited_doc_id, cited_count, subject_register)
  

data_subject_matter = list()
data_subject_matter$names = subject_matters
data_subject_matter$data = map(.x = subject_a matter_data, ~data %>% # OPRAVIT
                                      filter(citing_doc_id %in% .x$case_id) %>%
                                      left_join(., .x, by = join_by(citing_doc_id == case_id, citing_date_decision == date_decision)) %>%
                                      mutate(citing_doc_id = doc_id) %>%
                                      select(-doc_id) %>%
                                      drop_na(citing_doc_id) %>%
                                      summarise(n_references = n(),
                                                n_decisions = length(unique(citing_doc_id))) %>%
                                      ungroup() %>%
                                      mutate(n_references = sum(n_references),
                                             n_decisions = sum(n_decisions)) %>% 
                                      as_tibble())

data_subject_matter =  data_subject_matter |>
  as_tibble() |>
  unnest_longer(data) |>
  mutate(references_decisions_ratio = data$n_references/data$n_decisions)

subject_matters_to_filter = data_subject_matter$names[data_subject_matter$data$n_decisions > 25 & data_subject_matter$data$n_decisions < 150 & data_subject_matter$references_decisions_ratio > 3]

subset_data = function(data, subject){
  # Creates a DF with unique doc_ids as well as filtered restitution cases
  output = data %>%
    filter(grepl(subject, subject_register)) |> 
    select(citing_doc_id, cited_doc_id, cited_count) |>
    distinct() |>
    mutate(cited_count = replace(cited_count, cited_count == 0, 1)) |>
    pivot_wider(names_from = "cited_doc_id", values_from = cited_count, values_fill = 0) |>
    column_to_rownames(var = "citing_doc_id")
  return(output)
}

reshape_data = function(data, filtered = TRUE) {
  # --- Reshape ------------------------------------------------------------------
  # Goal: Return Three Data Matrices:
  #   1 real, 1 with count, 1 with citation matrix
  if(nrow(data) == 0) return(NULL)
  
  # J number of legal documents
  J <- length(rownames(data))
  # K number of different sources overall
  K <- length(colnames(data))
  #  N
  N <- J*K
  
  # y[N] dummy if user i follows elite j
  set_of_all_links <- colnames(data)
  
  # y connection matrix as a vector 
  # y is binary vector 
  # y_count is count 
  y_count <- vector()
  for(i in seq(1,J)){
    y_count <- append(y_count, data[i,])
  }
  y_count <- unlist(y_count)
  y <- y_count
  y[y_count > 1] <- 1
  
  table(y_count)
  table(y)
  
  #  jj[N] verdict/decision for observation y_n
  jj <- vector()
  for(i in seq(1,J)){ # accounting for python starting at 0
    jj <- append(jj, rep(i,K))
  }
  
  #  kk[N]
  # legal document/source for observation y_n
  kk <- rep(seq(1,K), J) # accounting for python starting at 0
  
  # This filters those legal sources out, that appear only once --------------
  # Step 1: Get a mask
  # y into matrix: Rows nr. of cases, cols nr of sources
  # y_matrix = np.asarray(y).reshape(J,K)
  y_matrix <- data
  # colsums
  # citations = y_matrix.sum(axis=0)
  citations <- colSums(y_matrix)
  # colsums > 1 == True
  mask_sources_to_keep_one_decision <- citations > 1
  # TF vector * nr of decisions (gets mask to right shape)
  mask_sources_to_keep <- rep(mask_sources_to_keep_one_decision,J)
  # Step 2: Select with the mask
  y_cit_filter <- y[mask_sources_to_keep]
  y_count_cit_filter <- y_count[mask_sources_to_keep]
  set_of_all_links_cit_filter <- set_of_all_links[mask_sources_to_keep_one_decision]
  K_cit_filter <- sum(mask_sources_to_keep_one_decision)
  N_cit_filter <- length(y_cit_filter)
  J_cit_filter <- N_cit_filter/K_cit_filter
  
  jj_cit_filter <- vector()
  # re-create counters
  for(i in seq(1,J_cit_filter)){
    jj_cit_filter <- append(jj_cit_filter, rep(i,K_cit_filter))
  }
  
  #  kk[N]
  # legal document/source for observation y_n
  kk_cit_filter <- rep(seq(1,K_cit_filter), J_cit_filter)
  
  if(filtered == TRUE) {
    data = list(case_ids = rownames(y_matrix),
                J = J,
                K = K_cit_filter,
                N = N_cit_filter,
                jj = jj_cit_filter,
                kk = kk_cit_filter,
                y = y_cit_filter)
  } else {
    data = list(case_ids = rownames(y_matrix),
                J = J,
                K = K,
                N = N,
                jj = jj,
                kk = kk,
                y = y)
  }
  return(data)
}

fit_model = function(data){
  if(is.null(data)) return(NULL)
  
  median_over_param_list <- function(param_list) {
    if (length(class(param_list))==2){
      if (class(param_list)[1]=="matrix"){
        out <- apply(param_list, 2, median)  
      }
    } else if (length(class(param_list))==1){
      if (class(param_list)=="array"){
        out <- median(param_list)
      }
    }
    return(out)
  }
  
  
  # Calculate starting values
  make_starting_values <- function(vb_result){
    results <- extract(vb_result)
    # assign means and names
    results_means <- list(NA)
    for (i in seq(1,length(results))){
      results_means[[i]] <- median_over_param_list(results[[i]])
    }
    names(results_means) <- names(results)
    results_means_unlist <- unlist(results_means, recursive = FALSE)
    return(results_means_unlist)
  }
  
  
  # Assign the correct names
  lister <- function(inputlist, inits.for.run){
    one.list <- as.list(inputlist)
    names(one.list) <- names(inits.for.run)  
    return(one.list)
  }
  
  # Function to get the starting values in the correct shape
  # Takes as input results from either NUTS pre run or VB pre run
  # Returns the list with the correct shape
  start.val.maker <- function(fit.vb.or.nuts, nr.cores){
    inits.for.run <-make_starting_values(fit.vb.or.nuts)
    # now bring into shape: need to align with nr of chains
    init.multiplier <- seq(1, 3, (1/(nr.cores-1)*2))
    init.matrix <- matrix(
      rep(inits.for.run, length(init.multiplier)), ncol = length(init.multiplier))
    for (i in seq(1, length(init.multiplier))){
      init.matrix[,i] <- init.matrix[,i]*init.multiplier[i]
    }
    init.list <- split(init.matrix, col(init.matrix))
    init.list.shaped <- lapply(init.list, lister, inits.for.run = inits.for.run)
    return(init.list.shaped)
  }
  
  
  # -- Estimate Starting Values --------------------------------------------------
  
  start.values.and.anchors.from.vb <- function(data,
                                               nr.cores, model_file, seed.input){
    pre.fit.vb <- vb(
      stan_model(file = model_file) , 
      data = , 
      init = "random", 
      seed = project.seed)
    anchors <- find.anchors(pre.fit.vb)
    init.list.shaped <- start.val.maker(pre.fit.vb, nr.cores = nr.cores)
    results <- list(init.list.shaped, anchors)
    return(results)
  }
  
  
  start.values.and.anchors.from.stan <- function(data, 
                                                 nr.cores, model_file, seed.input){
    cat(paste('Using 1 core \n'))
    pre.fit.stan <- stan(
      file =  model_file,
      data = data,  
      chains = 1,    # number of Markov chains
      iter = 2000,    # total number of iterations per chain
      cores = 1,    # number of cores 
      refresh = 50,    # show progress every 'refresh' iterations
      init = 'random',
      seed = seed.input,
      control=list(adapt_delta=0.8)
    )
    anchors <- find.anchors(pre.fit.stan)
    init.list.shaped <- start.val.maker(pre.fit.stan, nr.cores = nr.cores)
    results <- list(init.list.shaped, anchors)
    return(results)
  }
  
  
  
  
  # -- Find Anchors ------------------------------------------------------------------
  # Takes the fit from the first run on 1 chain and returns positions 
  # of the left and right anchor for theta
  find.anchors <- function(stan.fit){
    theta.medians <- apply(extract(stan.fit)$theta1, 2, median)
    left.anchor <- which(theta.medians %in% min(theta.medians))
    right.anchor <- which(theta.medians %in% max(theta.medians))
    return(c(left.anchor, right.anchor))
  }
  
  
  run.stan.to.estimate <- function(data, nr.iterations, model, seed.input,
                                 anchors.input = FALSE,
                                 init.model = FALSE,
                                 nr.cores.input = FALSE,
                                 random.inits = FALSE){
  # Select correct stan models 
  if (model == 'poisson'){
    if (anchors.input != FALSE){
      model_file <- 'scripts/code_helpers/stan_poisson_1D_decenter_anchors.stan'
    } else {
      model_file <- 'scripts/code_helpers/stan_poisson_1D_decenter.stan'
    }
  } else if (model == 'hierarchical'){
    model_file <- 'scripts/code_helpers/stan_poisson_estimate_ri_ident.stan'
  } else {
    cat('Please specify correct model. Either "poisson" or "hierarchical" \n')
  }
  
  # Select nr of cores to run on
  if (nr.cores.input != FALSE){
    nr.cores = nr.cores.input
  } else {
    nr.cores = parallel::detectCores()  
  }
  
  # calculate the length of each chain
  length.run <- round(nr.iterations/nr.cores)
  
  # Generate Data adding the anchors if you specify them explicitly. 
  # First part of two for setting anchors
  if (class(anchors.input[1]) == 'numeric'){
    data.final <- c(data, 
                    left_anchor = anchors.input[1], 
                    right_anchor = anchors.input[2])  
  }
  
  # Running the models
  time1 <- proc.time()
  # Generating the inits
  if (init.model=='vb'){
    results <- start.values.and.anchors.from.vb(
      data, 
      nr.cores=nr.cores,
      model_file = 'scripts/code_helpers/stan_poisson_pre_run.stan'
    )  
    init.list <- results [[1]]
    anchors <- results [[2]]
    cat('Running model with inits from a VB run \n')
  } else if (init.model=='stan'){
    results <- start.values.and.anchors.from.stan(
      data, 
      nr.cores=nr.cores,
      model_file='scripts/code_helpers/stan_poisson_pre_run.stan', 
      seed.input=seed.input
    )
    init.list <- results [[1]]
    anchors <- results [[2]]
    cat('Running model with inits from a 1 chain NUTS run \n')
  } else cat("Specify correct init model, either 'vb' or 'stan'")
  
  # option to override the calculcated inits. Makes sense if you want to have 
  # anchors, but random inits.
  if (random.inits==TRUE) init.list<-'random' 
  
  # If you want to have anchors but do not provide them, take them from the 
  # function in start.values.and.anchors.from.vb. Second part of chosing anchors
  if (anchors.input == TRUE){
    # assert that there is a pre-run to estimate the anchors
    if (init.model %in% c('vb', 'stan')){
      data.final <- c(data, left_anchor = anchors[1], right_anchor = anchors[2])   
      cat('Left anchor is', anchors[1], 'and right anchor is', anchors[2],'\n')
    } else cat("You can only calculate anchors if you chose to estimate a pre-run")
  } else if (anchors.input==FALSE) {
    data.final <- data
  }
  cat(paste('Now turning to the full estimation. I am using', nr.cores, "cores for it. \n"))
  if (nr.cores == parallel::detectCores()) cat('Turn off the heating: Using all available cores.\n')
  
  # Estimating for real
  fit2 <- stan(
    file = model_file,  # Stan program
    data = data.final,  # Named list of data
    chains = nr.cores,  # Number of Markov chains. 1 per core
    iter = length.run,  # Total number of iterations per chain
    cores = nr.cores,   # Number of cores (using 2 just for the vignette)
    refresh = 50,
    init = init.list,
    seed = seed.input,
    control=list(adapt_delta=0.8) # this can reduce divergence errors
  )
  time2 <- proc.time()
  duration = time2 - time1
  cat(paste('it took me', round(duration[3]/60, digits = 2), 'mins'))
  return(fit2)}
  
  fit.stan <- run.stan.to.estimate(data, 
                                   seed.input = project.seed, init.model = 'stan',
                                   nr.iterations = 8000, anchors.input = TRUE,
                                   model = 'poisson')
  
  # summary(fit.stan)
  
  
  draws = extract(fit.stan)
  inverter = 1
  
  
  # PROCESSING MODEL --------------------------------------------------------
  high.bound <- function(dat){
    out <- quantile(x = dat, .95)
    return(out)
  }
  
  low.bound <- function(dat){
    out <- quantile(x = dat, .05)
    return(out)
  }
  
  theta <- apply(draws$theta, 2, median)*inverter
  higher <- theta.hb <- apply(draws$theta, 2, high.bound)*inverter
  lower <- theta.lb <- apply(draws$theta, 2, low.bound)*inverter
  ruler <- seq(1,length(theta))
  
  data_output = tibble(
    case = data$case_ids,
    theta = apply(draws$theta, 2, median)*inverter,
    higher =  apply(draws$theta, 2, high.bound)*inverter,
    lower = apply(draws$theta, 2, low.bound)*inverter
  )
  return(data_output)
}



start.time <- Sys.time()
plan(multisession, workers = parallel::detectCores() - 2)
new_data1 = map(subject_matters_to_filter[1:10], ~subset_data(data = data, subject = .x) |>
                        reshape_data() |>
                 fit_model(), .progress = T)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

new_data1 |>
  map(~.x |> ggplot(mapping = aes(x = case, y = theta)) +
        geom_pointrange(aes(ymin = lower, ymax = higher)) +
        coord_flip()  +
        labs(y = "Estimated location of a decision",
             x = NULL, 
             title = "Overview of all decisions"))

significant_decisions = new_data1 |>
  map(~.x |> filter(sign(higher) == sign(lower)))

save.image("10_subjects_finished.RData")
