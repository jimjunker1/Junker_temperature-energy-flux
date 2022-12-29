##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param ann_comm_boots
##' @param stream_gini_df
##' @param diet_similarity_mat
##' @param skew_analysis
analyze_temp_stats <- function(ann_comm_boots = production_boots[["ann_comm_boots"]],
                               ann_spp_boots = production_boots[["ann_spp_boots"]],
                               stream_gini_df = gini_analysis[["stream_gini_df"]],
                               diet_similarity_mat = diet_similarity[['among_modeled_overlap']],
                               skew_analysis = skew_analysis,
                               n_boot = 1e2) {
  
  conflicted:::conflicts_reset()
  
  stream_temps = stream_temp_labels %>% data.frame %>%
    rownames_to_column('site_id') %>% setNames(., c('site_id', 'tempC')) %>%
    dplyr::mutate(tempC = as.numeric(tempC))
  
  set.seed(123)
  sppBootsDf = ann_spp_boots %>%
    purrr::map(~.x %>% 
                 junkR::named_group_split(taxon_id) %>%
                 purrr::map(~.x %>% slice_sample(n = n_boot, replace = TRUE) %>%
                              dplyr::select(taxon_id, pb_y, M_mg_ind) %>%
                              dplyr::mutate(n_rep = 1:n())) %>% bind_rows) %>%
    purrr::map(~.x %>% group_by(n_rep) %>%
                 dplyr::summarise(across(c(pb_y,M_mg_ind), ~mean(.x, na.rm = TRUE)))) %>% 
    bind_rows(.id = "site_id" ) %>%
    left_join(stream_temps)
    
  pb_spp_temp_boots = sppBootsDf %>%
    group_by(n_rep) %>%
    do(model = lm(log(pb_y) ~ tempC, data = .))
  
  pb_spp_temp_coefs = pb_spp_temp_boots %>%
    purrr::pmap_dbl(~..2 %>% pluck('coefficients') %>% pluck('tempC'))
  
  
  m_spp_temp_boots = sppBootsDf %>%
    group_by(n_rep) %>%
    do(model = lm(log(M_mg_ind) ~ tempC, data = .))
  
  m_spp_temp_coefs = m_spp_temp_boots %>%
    group_by(n_rep) %>%
    purrr::pmap_dbl(~..2 %>% pluck('coefficients') %>% pluck('tempC'))
  
  set.seed(123)
  boots_df = ann_comm_boots %>%
     map(~.x %>% slice_sample(n = n_boot, replace = TRUE) %>%
         dplyr::mutate(n_rep = 1:n())) %>% bind_rows %>%
    left_join(stream_temps)
  
  pb_temp_boots = boots_df %>%
    group_by(n_rep) %>%
    do(model = lm(log(pb_y) ~ tempC, data = .))
 
  pb_temp_coefs = pb_temp_boots %>%
    purrr::pmap_dbl(~..2 %>% pluck('coefficients') %>% pluck('tempC'))
  
  M_temp_boots = boots_df %>%
    group_by(n_rep) %>%
    do(model = lm(log(M_mg_ind) ~ tempC, data = .))
  
  M_temp_coefs = M_temp_boots %>%
    purrr::pmap_dbl(~..2 %>% pluck('coefficients') %>% pluck('tempC'))
  
  #create df of diet_similarity
  diet_similarity_df= diet_similarity_mat %>%
    future_map(~.x %>% pluck('overlapmatrix') %>% as.data.frame %>%
          setNames(., nm = names(stream_order_list)) %>%
          dplyr::mutate(site = names(stream_order_list)) %>%
        column_to_rownames("site")) %>%
          future_map(function(x){ 
            y = as.matrix(x)
            y[lower.tri(y)]<- NA
            y %>% data.frame %>%
              rownames_to_column("site1") %>%
              pivot_longer(-site1, names_to = 'site2', values_to = 'overlap') %>%
              na.omit}) %>%
    bind_rows(.id = "boot_n")
  
   #create temp distance df
  site_combns = expand.grid(site1 = stream_order, site2 = stream_order) %>%
    left_join(stream_temps %>% dplyr::rename(site1 = 'site_id')) %>%
    left_join(stream_temps %>% dplyr::rename(site2 = 'site_id'), by = 'site2') %>%
    setNames(.,nm = c('site1','site2','tempC_1','tempC_2')) %>%
    dplyr::filter(tempC_1 > tempC_2) %>%
    dplyr::mutate(temp_diff = tempC_1 - tempC_2)

  # join temp distance and diet overlap
  distance_similarity = diet_similarity_df %>%
    left_join(site_combns %>% dplyr::select(site1,site2,temp_diff), by = c("site1","site2"))
  
 # estimate relationship between evenness and temperature 
  set.seed(123)
  evenness_df = stream_gini_df %>% named_group_split(site) %>%
    map(~.x %>% slice_sample(n = n_boot, replace = TRUE) %>%
          dplyr::mutate(n_rep = 1:n())) %>% bind_rows %>%
    left_join(stream_temps %>% dplyr::rename(site = 'site_id'))
  
  evenness_temp_boots = evenness_df %>%
    group_by(n_rep) %>%
    do(model = lm(`Non-normalized Gini` ~ tempC, data = .))
  
  evenness_temp_coefs = evenness_temp_boots %>%
    purrr::pmap_dbl(~..2 %>% pluck('coefficients') %>% pluck('tempC'))
  
  norm_evenness_temp_boots = evenness_df %>%
    group_by(n_rep) %>%
    do(model = lm(`Normalized Gini` ~ tempC, data = .))
  
  norm_evenness_temp_coefs = norm_evenness_temp_boots %>%
    purrr::pmap_dbl(~..2 %>% pluck('coefficients') %>% pluck('tempC'))
  
  # estimate the relationship with production skew and temperature
  
  new_data = data.frame(tempC = seq((min(stream_temps$tempC)-0.5),(max(stream_temps$tempC)+0.5), length.out = 100)) %>%
    dplyr::mutate(tempC_stand = tempC - mean(tempC, na.rm = TRUE))
  
  set.seed(123)
  pb_skew_df = skew_analysis[['pb_skew_boots']] %>% named_group_split(site) %>%
    map(~.x %>% slice_sample(n = n_boot, replace = TRUE) %>%
          dplyr::mutate(n_rep = 1:n())) %>% bind_rows %>%
    left_join(stream_temps %>% dplyr::rename(site = 'site_id')) %>%
    dplyr::mutate(tempC_stand = tempC - mean(tempC, na.rm = TRUE),
                  pb_y_skew_mod = (pb_y_skew + 1)/2)
  
  pb_skew_formula = brms::bf(pb_y_skew_mod ~ tempC_stand,
                            phi ~ tempC_stand,
                            zoi ~ 1,
                            coi ~ 1,
                            family = brms::zero_one_inflated_beta())
  
  # get_prior(pb_skew_formula, data = pb_skew_df, family = brms::zero_one_inflated_beta())
  pb_skew_priors = c(set_prior("normal(0,1.2)", class = "b", coef = "tempC_stand"),
                    set_prior("normal(0.5,0.5)", class = "Intercept"))
  
  pb_skew_temp_boots = brms::brm(formula = pb_skew_formula,
                                data = pb_skew_df,
                                prior = pb_skew_priors,
                                warmup = 10e3,
                                iter = 12e3,
                                chains = 3,
                                control = list(adapt_delta = 0.99),
                                sample_prior = "yes",
                                file = "./data/pb_skew_temp_model",
                                file_refit = 'on_change',
                                backend = "cmdstanr")
  pb_skew_temp_boots = add_criterion(pb_skew_temp_boots, "loo", file = "./data/pb_skew_temp_model.rds")

  pb_skew_temp_pred = pb_skew_temp_boots %>%
    add_epred_draws(newdata = new_data) %>%
    dplyr::mutate(new_epred = (.epred - 1)/2)

  pb_skew_temp_coefs = pb_skew_temp_boots %>%
    brms::posterior_samples("tempC_stand")

  
  pb_skew_temp_null = brms::brm(formula = brms::bf(pb_y_skew_mod ~ 1,
                                                   phi ~ 1,
                                                   zoi ~ 1,
                                                   coi ~ 1,
                                                   family = brms::zero_one_inflated_beta()),
                                 data = pb_skew_df,
                                 # prior = pb_skew_priors,
                                 warmup = 10e3,
                                 iter = 12e3,
                                 chains = 3,
                                 control = list(adapt_delta = 0.99),
                                 sample_prior = "yes",
                                 file = "./data/pb_skew_temp_null.rds",
                                 file_refit = 'on_change',
                                backend = "cmdstanr")
  pb_skew_temp_null = add_criterion(pb_skew_temp_null, 'loo', file = "./data/pb_skew_temp_null")
  
  loo::loo_compare(pb_skew_temp_boots, pb_skew_temp_null)
 
  set.seed(123)
  M_skew_df = skew_analysis[['M_skew_boots']] %>% named_group_split(site) %>%
    map(~.x %>% slice_sample(n = n_boot, replace = TRUE) %>%
          dplyr::mutate(n_rep = 1:n())) %>% bind_rows %>%
    left_join(stream_temps %>% dplyr::rename(site = 'site_id')) %>%
    dplyr::mutate(tempC_stand = tempC - mean(tempC, na.rm = TRUE),
                  m_mg_ind_skew_mod = (M_mg_ind_skew+1)/2)
  
  m_skew_formula = brms::bf(
    # the mean proportion
    m_mg_ind_skew_mod ~ tempC_stand,
    # the precision parameter
    phi ~ tempC_stand,
    # the zero inflated portion
    zoi ~ 1,
    # the one inflated portion
    coi ~ 1,
    # set the likelihood family
    family = brms::zero_one_inflated_beta())
  
  m_skew_priors = c(set_prior("normal(0,1.2)", class = "b", coef = "tempC_stand"),
                      set_prior("normal(-0.15,0.5)", class = "Intercept"))
  
  M_skew_temp_boots = brms::brm(formula = m_skew_formula,
                                data = M_skew_df,
                                prior = m_skew_priors,
                                warmup = 10e3,
                                iter = 12e3,
                                chains = 3,
                                control = list(adapt_delta = 0.99),
                                sample_prior = "yes",
                                file = "./data/m_skew_temp_model",
                                file_refit = 'on_change',
                                backend = "cmdstanr")
  M_skew_temp_boots = add_criterion(M_skew_temp_boots, 'loo',file = "./data/m_skew_temp_model.rds")

  m_skew_temp_pred = M_skew_temp_boots %>%
    add_epred_draws(newdata = new_data)
 
  M_skew_temp_coefs = M_skew_temp_boots %>%
    brms::posterior_samples("tempC_stand")
  
  M_skew_temp_null = brms::brm(formula = brms::bf(m_mg_ind_skew_mod ~ 1,
                                                  phi ~ 1,
                                                  zoi ~ 1,
                                                  coi ~ 1),
                               family = brms::zero_one_inflated_beta(),
                              data = M_skew_df,
                                # prior = m_skew_priors,
                                warmup = 10e3,
                                iter = 12e3,
                                chains = 3,
                                control = list(adapt_delta = 0.99),
                                sample_prior = "yes",
                                file = "./data/m_skew_temp_null",
                                file_refit = 'on_change',
                              backend = "cmdstanr")
  M_skew_temp_null = add_criterion(M_skew_temp_null, 'loo', file = "./data/m_skew_temp_null.rds")

  loo::loo_compare(M_skew_temp_boots, M_skew_temp_null)
  # random vs non-random structure of energy fluxes with temperature. 
  
  set.seed(123)
  pb_probs_df = skew_analysis[['pb_skew_probs']] %>% bind_rows(.id = 'site') %>%
    pivot_longer(-site, names_to = 'boot', values_to = 'pb_skew_prob') %>% select(-boot) %>%
    named_group_split(site) %>%
    map(~.x %>% slice_sample(n = n_boot, replace = TRUE) %>%
          dplyr::mutate(n_rep = 1:n())) %>% bind_rows %>%
    left_join(stream_temps %>% dplyr::rename(site = 'site_id')) %>%
    dplyr::mutate(tempC_stand = tempC - mean(tempC, na.rm = TRUE))

    pb_probs_formula = brms::bf(pb_skew_prob ~ tempC_stand,
                             phi ~ tempC_stand,
                             zi ~  1,
                             family = brms::zero_inflated_beta()
                             )
    # check priors 
    # brms::get_prior(pb_probs_formula,
    #                 data = pb_probs_df,
    #                 family = brms::zero_inflated_beta())
    
    pb_probs_priors = c(set_prior("normal(-1,1.2)", class = "b", coef = "tempC_stand"),
                        set_prior("normal(-0.002,0.5)", class = "Intercept"))
     
    pb_probs_temp_boots =brms::brm(pb_probs_formula,
                         data = pb_probs_df,
                         prior = pb_probs_priors,
                         warmup = 10e3,
                         iter = 12e3,
                         chains = 3,
                         control = list(adapt_delta = 0.99),
                         sample_prior = "yes",
                         file = "./data/pb_probs_temp_model",
                         file_refit = 'on_change',
                         backend = "cmdstanr")
    pb_probs_temp_boots = brms::add_criterion(pb_probs_temp_boots,
                                              c('loo'), file = "./data/pb_probs_temp_model.rds")

    pb_probs_temp_null =brms::brm(brms::bf(pb_skew_prob ~ 1,
                                           phi ~ 1,
                                           zi ~  1,
                                           family = brms::zero_inflated_beta()),
                                   data = pb_probs_df,
                                   # prior = pb_probs_priors,
                                   warmup = 10e3,
                                   iter = 12e3,
                                   chains = 3,
                                   control = list(adapt_delta = 0.99),
                                   sample_prior = "yes",
                                   file = "./data/pb_probs_temp_null",
                                   file_refit = 'on_change',
                                  backend = "cmdstanr")
    pb_probs_temp_null = brms::add_criterion(pb_probs_temp_null,
                                              c('loo'), file = "./data/pb_probs_temp_null.rds")
    loo::loo_compare(pb_probs_temp_boots, pb_probs_temp_null)
    
    # pb_temp_fancy_zoi <- pb_probs_temp_boots %>% 
    #   emmeans(~ tempC_stand,
    #           at = list(tempC_stand = seq(-7, 16, by = 0.5)),
    #           epred = TRUE,
    #           re_formula = NULL) %>% 
    #   gather_emmeans_draws()
    # 
    # ggplot(pb_temp_fancy_zoi,
    #        aes(x = tempC_stand, y = plogis(.value))) +
    #   stat_lineribbon() +
    #   scale_fill_viridis_d() +
    #   scale_color_viridis_d() +
    #   scale_y_continuous(name = "Pr(x >= )", limits = c(0,1)) +
    #   geom_point(data = pb_probs_df %>% 
    #                group_by(site) %>% 
    #                dplyr::summarise(pb_skew_prob = mean(pb_skew_prob),
    #                                 tempC = mean(tempC),
    #                                 tempC_stand = mean(tempC_stand)), aes(x = tempC_stand, y = pb_skew_prob), inherit.aes = FALSE)
    # 
    
  pb_probs_temp_coefs = pb_probs_temp_boots %>%
    brms::posterior_samples("tempC_stand")

  pb_probs_temp_pred = pb_probs_temp_boots %>%
    add_predicted_draws(newdata = new_data)

  # fit a concave polynomial model to probability data
  # pb_probs_concave_boots = pb_probs_df %>%
  #   group_by(n_rep) %>%
  #   do(model = lm(pb_skew_prob ~ poly(tempC, 2), data = .))
  # 
  # pb_probs_concave_coefs = pb_probs_concave_boots %>%
  #   purrr::pmap(~..2 %>% pluck('coefficients'))
  # 
  # pb_probs_concave_stats = pb_probs_concave_boots %>%
  #   purrr::pmap(~..2 %>% broom::glance())
  # 
  # new_data = data.frame(tempC = seq(min(stream_temps$tempC),max(stream_temps$tempC)+0.2, 0.5))
  # 
  # pb_probs_concave_pred = pb_probs_concave_boots %>%
  #   purrr::pmap(~..2 %>% predict(newdata = new_data) %>% data.frame(.fitted = .) %>% dplyr::mutate(tempC = unlist(new_data, use.names = FALSE))) 
  # 
  # # get average model from bootstrapped models
  # 
  # pb_probs_concave_med = pb_probs_df %>%
  #   group_split(n_rep) %>%
  #   map(~lm(pb_skew_prob ~ poly(tempC, 2), data = .)) %>%
  #   MuMIn::model.avg(.)  %>%
  #   predict(newdata = new_data) %>% data.frame(.fitted = .) %>% dplyr::mutate(tempC = unlist(new_data, use.names = FALSE))
  # 
  # pb_probs_concave_med = pb_probs_concave_coefs %>%
  #   bind_rows %>%
  #   dplyr::summarise(across(everything(), ~median(.x , na.rm = TRUE))) %>%
  #   setNames(.,c("intercept", "b1", "b2")) %>%
  #   dplyr::mutate(.[[1]] + .[[2]] * unlist(new_data, use.names = FALSE) +  .[[3]]*(unlist(new_data, use.names = FALSE))^2) %>%
  #   unlist %>% data.frame(tempC = new_data, .fitted = .)
##
  
  set.seed(123)
  M_probs_df = skew_analysis[['M_skew_probs']] %>% bind_rows(.id = "site") %>%
    pivot_longer(-site, names_to = 'boot', values_to = 'M_skew_prob') %>% select(-boot) %>%
    named_group_split(site) %>%
    map(~.x %>% slice_sample(n = n_boot, replace = TRUE) %>%
          dplyr::mutate(n_rep = 1:n())) %>% bind_rows %>%
    left_join(stream_temps %>% dplyr::rename(site = 'site_id')) %>%
    dplyr::mutate(tempC_stand = tempC - mean(tempC, na.rm = TRUE))
  
  m_probs_formula = brms::bf(M_skew_prob ~ tempC_stand,
                              phi ~ tempC_stand,
                              zoi ~ 1,
                              coi ~ 1,
                              family = brms::zero_one_inflated_beta()
  )
  # check priors 
  # brms::get_prior(pb_probs_formula,
  #                 data = pb_probs_df,
  #                 family = brms::zero_one_inflated_beta())
  
  m_probs_priors = c(set_prior("normal(0,1.2)", class = "b", coef = "tempC_stand"),
                      set_prior("normal(0.25,0.6)", class = "Intercept"))
  
  m_probs_temp_boots =brms::brm(m_probs_formula,
                                 data = M_probs_df,
                                 prior = m_probs_priors,
                                 warmup = 10e3,
                                 iter = 12e3,
                                 chains = 3,
                                 control = list(adapt_delta = 0.99),
                                 sample_prior = "yes",
                                 file = "./data/m_probs_temp_model",
                                 file_refit = 'on_change',
                                backend = "cmdstanr")
  m_probs_temp_boots = add_criterion(m_probs_temp_boots, 'loo', file = "./data/m_probs_temp_model.rds")
  
  m_probs_temp_null =brms::brm(brms::bf(M_skew_prob ~ 1,
                                        phi ~ 1,
                                        zoi ~ 1,
                                        coi ~ 1,
                                        family = brms::zero_one_inflated_beta()
  ),
                                data = M_probs_df,
                                # prior = m_probs_priors,
                                warmup = 10e3,
                                iter = 12e3,
                                chains = 3,
                                control = list(adapt_delta = 0.99),
                                sample_prior = "yes",
                                file = "./data/m_probs_null_model",
                                file_refit = 'on_change',
  backend = "cmdstanr")
  m_probs_temp_null = add_criterion(m_probs_temp_null, 'loo', file = "./data/m_probs_null_model.rds")
  
  loo::loo_compare(m_probs_temp_boots, m_probs_temp_null)
  
  m_probs_temp_coefs = m_probs_temp_boots %>%
    brms::posterior_samples("tempC_stand")
  #   purrr::pmap_dbl(~..2 %>% pluck('coefficients') %>% pluck('mean') %>% pluck('tempC'))
  
  m_probs_temp_pred = m_probs_temp_boots %>%
    add_predicted_draws(newdata = new_data)

  return(list(sppBootsDf = sppBootsDf, pb_spp_temp_boots = pb_spp_temp_boots,pb_spp_temp_coefs = pb_spp_temp_coefs,
              m_spp_temp_boots = m_spp_temp_boots, m_spp_temp_coefs = m_spp_temp_coefs,
              pb_temp_coefs = pb_temp_coefs, M_temp_coefs = M_temp_coefs, n_boot = n_boot, 
              distance_similarity = distance_similarity, M_skew_temp_coefs = M_skew_temp_coefs,
              pb_skew_temp_coefs = pb_skew_temp_coefs, 
              pb_probs_df = pb_probs_df, 
              pb_skew_df = pb_skew_df, m_probs_df = M_probs_df, 
              m_skew_df = M_skew_df, m_skew_temp_pred = m_skew_temp_pred,
              pb_probs_temp_pred= pb_probs_temp_pred,
              m_probs_temp_pred = m_probs_temp_pred,
              pb_skew_temp_pred = pb_skew_temp_pred,
              m_probs_temp_coefs = m_probs_temp_coefs,
              pb_probs_temp_coefs = pb_probs_temp_coefs))
}
