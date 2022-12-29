##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param seasonal_diets
##' @param seasonal_fluxes
estimate_flux <- function(seasonal_diets = diet_matrices, seasonal_fluxes = seasonal_boot_split) {
  ## ++++ Helper functions ++++ ##
  prod_to_met <- function(production, NPE,...){
    production %>% 
      dplyr::mutate(across(contains("prod"), list(loss = ~.x/NPE), .names = "{.fn}")) %>% 
      dplyr::select(taxon_id, loss) %>% tibble::deframe() -> losses
  }
  
  boot_flux_function = function(mat, losses, resource_effs_vct,...){
    
    colnames(mat) %>% tibble %>%
      setNames(., nm = 'matrix_name') %>%
      dplyr::mutate(efficiencies = dplyr::recode(matrix_name, !!!resource_effs_vct, .default = 0.7)) -> x 
   spp_order = colnames(mat)
      setNames(x$efficiencies, nm = x$matrix_name)-> matrix_efficiencies
      
      resource_losses = setNames(rep(0, length(resource_effs_vct)), nm = names(resource_effs_vct))
      losses = c(losses,resource_losses)
      matrix_efficiencies = matrix_efficiencies[spp_order];losses = losses[spp_order]
    
    flux_obj = fluxing(mat = mat, losses = losses, efficiencies =  matrix_efficiencies,
                                bioms.losses = FALSE, bioms.prefs = FALSE, ef.level = 'prey', method = 'tbp')
    return(flux_obj)
    
  }
  ## ++++ End Helper functions ++++ ##
  # resource date frame with efficiences
  diet_item = list("amorphous_detritus",
                "cyanobacteria",
                "diatom",
                "filamentous",
                "green_algae",
                "plant_material",
                "animal")
 
   efficiencies = list(
     c(0.08, 0.1, 0.12),
     c(0.08, 0.1, 0.12),
     c(0.24, 0.3, 0.36),
     c(0.24, 0.3, 0.36),
     c(0.24, 0.3, 0.36),
     c(0.08, 0.1, 0.12),
     c(0.56, 0.7, 0.84)
  )
  resource_keyval = setNames(efficiencies, nm = unlist(diet_item))

 # estimate the beta distributions to draw diet AEs from
   set.seed(123)
   beta_dist.pars = map(efficiencies, ~rriskDistributions::get.beta.par(p = c(0.025,0.5,0.975), q = unlist(.x), plot = FALSE, show.output = FALSE))
   res_effs = map(beta_dist.pars, ~rbeta(nboot, shape1 = .x[1], shape2 = .x[2])) %>% 
     setNames(., nm = unlist(diet_item)) %>% bind_cols %>% split(., seq(nrow(.))) %>% map(~unlist(.))

 #NPE quantile 0.025,0.5,0.975
   set.seed(42)
   NPE_dist.pars = rriskDistributions::get.beta.par(p = c(0.025,0.5,0.975),q = c(0.4,0.45,0.5), show.output = FALSE, plot = FALSE)
   NPE = rbeta(nboot, shape1 = NPE_dist.pars[1], shape2 = NPE_dist.pars[2])
 # combined the production from Jan-Apr, May-Aug, Sep-Dec
   
   # convert production to metabolic fluxes
   # debugonce(prod_to_met)
   fluxes = map(seasonal_fluxes, ~.x %>% map(., ~.x %>% map2(., NPE, ~prod_to_met(.x, NPE = .y)))) %>% rlist::list.subset(names(stream_order_list))
    # seasonal_diets= seasonal_diets[1:2]
    # fluxes = fluxes[1:2]
   # 
   # debugonce(boot_flux_function)
   flux_full = map2(seasonal_diets, fluxes, function(x,y){
     map2(x, y, function(a,b){
   pmap(list(a,b,res_effs), ~boot_flux_function(mat =..1, losses = ..2, resource_effs_vct = ..3))
     })})
   
   ann_fluxes = fluxes %>% map(~.x %>% map(~.x %>% bind_rows(.id = 'boot_id')) %>% bind_rows(.id = 'yr_third') %>% pivot_longer(-boot_id:-yr_third, names_to = 'taxon', values_to = 'flux_mg_m') %>%
                                             group_by(taxon, boot_id) %>% dplyr::summarise(flux_mg_m_y = sum(flux_mg_m, na.rm = TRUE))) %>% bind_rows(.id = 'site')
       
   return(list(energy_demand= ann_fluxes, flux_full = flux_full))
}
