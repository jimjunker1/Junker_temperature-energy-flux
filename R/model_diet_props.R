##' This code builds a multilevel model for diet proportions across sites. It allows for species- and period-level
##' modifications. To allow for uncertainty to be propagated through to OM flux estimates, it employs a hierarchical 
##' bayesian model in brms
##'
##' .. content for \details{} ..
##'
##' @title
##' @param full_diet_df
model_diet_props <- function(full_diet_df = full_diet_df,
                             model = "dirichlet",...) {
  
  ## ++++ Helper functions ++++ ##
  expand.grid.df <- function(...) Reduce(function(...) merge(..., by=NULL), list(...))
  ## ++++ End Helper functions ++++ ##
  full_diet_count_df = full_diet_df %>% dplyr::mutate(rel_area = round(rel_area*1000,0))
  full_diet_prop_df = full_diet_df
  # convert gut lists into wide format for each site
  wide_gut_lists = full_diet_df %>%
          select(site, yr_third, taxon, id, diet_item, rel_area) %>%
          group_by(site, yr_third, id, taxon) %>%
          pivot_wider(names_from = 'diet_item', values_from = 'rel_area') %>%
  named_group_split(site) %>%
    rlist::list.subset(names(stream_order_list))
  # 
  # # # remove predators from lists
  # # # name predators 
  predator_lists = list(hver = c("Limnophora riparia"),
                        st6 = c("Limnophora riparia", "Antocha sp.", "Ephydridae.sp.", "Clinocera stagnalis"),
                        st9 = c("Limnophora riparia", "Antocha sp.", "Ephydridae.sp.", "Clinocera stagnalis"),
                        st7 = c("Limnophora riparia", "Macropelopia", "Dicranota", "Clinocera stagnalis"),
                        oh2 = c("Limnophora riparia", "Macropelopia", "Dicranota", "Clinocera stagnalis"),
                        st14 = c("Limnophora riparia", "Antocha sp.")) %>%
    rlist::list.subset(names(stream_order_list))

  wide_guts_lists_nopred = wide_gut_lists %>%
    map2(., predator_lists, ~.x %>%
          dplyr::filter(taxon %ni% .y)) %>%
    bind_rows %>%
    ungroup %>% select(-id)

  diet_meta = wide_guts_lists_nopred %>% ungroup %>% select(site, yr_third, taxon)
  diet_props = wide_guts_lists_nopred %>% ungroup %>% select(-site:-taxon) %>% as.matrix
  diet_bind = wide_guts_lists_nopred %>% ungroup
  sum(apply(diet_props,1,sum), na.rm = TRUE)
  # 
  diet_df = diet_meta;diet_df$props = diet_props
  # saveRDS(diet_df, file = "./data/diet_df.rds")
  # diet_df = readRDS(file = "./data/diet_df.rds")
  #turn off conflicts for running brm models 
  conflicted:::conflicts_reset()

  #### full brm diet model ###
  # multilevel_model_mvbind = brms::bf(mvbind(amorphous_detritus, animal, cyanobacteria, diatom, filamentous, green_algae, plant_material) ~ 1 + (1|site) + (1|taxon/yr_third/site))
  # poiss_model_prior = brms::get_prior(multilevel_model_mvbind, 
  #                               data = diet_bind,
  #                               family = poisson(link = "log"))
  # 
  # poiss_prior = c(prior(normal(0,1), class = "Intercept", resp = "amorphousdetritus"),
  #                 prior(normal(0,0.5), class = "Intercept", resp = "animal"),
  #                 prior(normal(0,0.1), class = "Intercept", resp = "cyanobacteria"),
  #                 prior(normal(0.5,0.5), class = "Intercept", resp = "diatom"),
  #                 prior(normal(0,1), class = "Intercept", resp = "filamentous"),
  #                 prior(normal(0,1), class = "Intercept", resp = "greenalgae"),
  #                 prior(normal(0,0.6), class = "Intercept", resp = "plantmaterial"))
  # 
  # set.seed(123)
  # poiss_diet_model <- brm(multilevel_model_mvbind,
  #                   data = diet_bind, 
  #                   family = poisson(link = "log"),
  #                   prior = poiss_prior,
  #                   warmup = 1000,
  #                   iter = 3e3, 
  #                   file = "./data/diet-poiss_brms_m", # cache model (can be removed)
  #                   backend = 'cmdstanr',
  #                   rerun = 'on-change',
  
  #                   control = list(adapt_delta = 0.99,
  #                                  max_treedepth = 15))
  # dirichlet model
  multilevel_model_formula <- brms::bf(props ~ 1 + (1|site) + (1|taxon/yr_third/site))
  dir_model_prior = brms::get_prior(multilevel_model_formula, 
                                data = diet_df,
                                family = dirichlet())
  stan_diet_data = make_standata(formula = multilevel_model_formula, data = diet_df, family = dirichlet())
  
  dir_prior = c(prior(normal(0,0.2), class = "Intercept", dpar = "muanimal"),
                prior(normal(0,0.5), class = "sd", dpar = "muanimal"),
                prior(normal(0,0.2), class = "Intercept", dpar = "mucyanobacteria"),
                prior(normal(0,0.5), class = "sd", dpar = "mucyanobacteria"),
                prior(normal(1,2), class = "Intercept", dpar = "mudiatom"),
                prior(normal(0,3), class = "Intercept", dpar = "mufilamentous"),
                prior(normal(0,3), class = "Intercept", dpar = "mugreenalgae"),
                prior(normal(0,1), class = "Intercept", dpar = "muplantmaterial"))
  
  dir_diet_model <- brm(multilevel_model_formula,
                    data = diet_df, 
                    family = dirichlet(),
                    prior = dir_prior,
                    warmup = 1000,
                    iter = 2e3,
                    seed = 123,
                    control = list(adapt_delta = 0.99),
                    sample_prior = "yes",
                    file = "./data/diet-dir_brms_m2",
                    rerun = 'never',
                    backend = 'cmdstanr')
  
stan_diet_code =make_stancode(formula = multilevel_model_formula, family = dirichlet(), prior = dir_prior, data = diet_df, save_model = './data/diet_hier_dir_model.stan')
  # saveRDS(dir_diet_model, "./data/diet-dir_brms_m2.rds")

  # create model summary for assessing model fit
  dirichlet_priors = posterior_samples(dir_diet_model) %>%
    dplyr::select(contains("prior")) %>%
    dplyr::mutate(iter = 1:nrow(.)) %>%
    as_tibble
  
  diet_predictions_mat = fitted(dir_diet_model,
                            newdata = diet_df,
                            allow_new_levels = TRUE,
                            summary = FALSE,
                            scale = 'response',
                            ndraws = nboot) %>%
    as_tibble() %>% t() %>% data.frame %>% 
    rownames_to_column('diet_item') %>% 
    dplyr::mutate(diet_item_new = stringr::str_extract(diet_item, "[^0-9\\.]+$")) %>% 
      select(-diet_item) %>%
      select(diet_item = 'diet_item_new', everything())
  
  diet_meta_mat = do.call("rbind", replicate(dim(diet_props)[2], diet_meta, simplify = FALSE))
  
  diet_predictions = bind_cols(diet_meta_mat, diet_predictions_mat) %>%
    pivot_longer(-site:-diet_item, names_to = 'boot_id', values_to = 'rel_area')
  
  #Add back in predator diets
  # create boot_id sequence 
  boot_id = data.frame(boot_id = paste0("X",1:1000))
  predator_diet_props <- data.frame(diet_item = unique(diet_predictions$diet_item), rel_area = 0) %>% 
    dplyr::mutate(rel_area = ifelse(diet_item == "animal", 1, 0)) %>%
    pivot_wider(names_from = 'diet_item', values_from = 'rel_area')
  
  predator_diets_meta <- wide_gut_lists %>%
    map2(., predator_lists, ~.x %>%
           dplyr::filter(taxon %in% .y)) %>%
    bind_rows %>%
    ungroup %>% select(site, yr_third, taxon) %>% bind_cols(predator_diet_props) %>%
    pivot_longer(-site:-taxon, names_to = 'diet_item', values_to = 'rel_area')
  
  predator_diets <- expand.grid.df(predator_diets_meta, boot_id) 
  
  diet_predictions = diet_predictions %>% 
    bind_rows(predator_diets) %>% 
    select(site, yr_third, taxon, boot_id, diet_item, rel_area)
  
  diet_seasonal_boot_split = diet_predictions %>%
    named_group_split(site) %>% 
    map(~named_group_split(.,yr_third) %>% map(~named_group_split(.,boot_id))) %>%
    rlist::list.subset(names(stream_order_list))
  
  return(list(diet_predictions = diet_predictions, diet_seasonal_boot_split = diet_seasonal_boot_split))
}
