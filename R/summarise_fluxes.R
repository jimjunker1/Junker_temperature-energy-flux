##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param flux_estimates
##' @param int_spp_meta
summarise_fluxes <- function(flux_estimates = flux_estimates[["flux_full"]], int_spp_meta = production_boots[["int_spp_meta"]]) {

  #compress the flux matrix to the OM fluxes from resources
  compress_flux <- function(mat){
    diet_items = list("amorphous_detritus",
                     "cyanobacteria",
                     "diatom",
                     "filamentous",
                     "green_algae",
                     "plant_material",
                     "animal")
    
    mat %>% data.frame %>% rownames_to_column('diet_item') %>%
      pivot_longer(-diet_item, names_to = 'taxon', values_to = 'flux_mg_m_int') %>%
      dplyr::filter(diet_item %in% diet_items,
                    taxon %ni% diet_items)
  }

  filtered_fluxes = map(flux_estimates, ~map(.,~map(., ~compress_flux(.x))))
  
  flux_boots_df = filtered_fluxes %>% map(.,~map(.,~.x %>% bind_rows(.id = 'boot_id')) %>% bind_rows(.id = 'yr_third')) %>% bind_rows(.id = 'site') %>% dplyr::rename(flux_mg_m_third = 'flux_mg_m_int')
  
  annual_spp_diet_flux_summary <- flux_boots_df %>% 
    group_by(site, boot_id, diet_item, taxon) %>% 
    dplyr::summarise(flux_mg_m_y = sum(flux_mg_m_third))%>%
    named_group_split(site) %>% map(., ~.x %>% 
                                      group_by(site, diet_item, taxon) %>% 
                                      dplyr::summarise(across(matches('flux'), list(mean = ~mean(.x, na.rm = TRUE),
                                                            quant2.5 = ~quantile(.x, 0.025, na.rm = TRUE),
                                                            quant25 = ~quantile(.x, 0.25, na.rm = TRUE),
                                                            quant50 = ~quantile(.x, 0.50, na.rm = TRUE),
                                                            quant75 = ~quantile(.x, 0.75, na.rm = TRUE),
                                                            quant97.5 = ~quantile(.x, 0.975, na.rm = TRUE))))) %>%
  bind_rows
  
  annual_spp_flux_boots <- flux_boots_df %>% 
    group_by(site, boot_id, taxon) %>% 
    dplyr::summarise(flux_mg_m_y = sum(flux_mg_m_third))%>% ungroup 
    
    annual_spp_flux_summary <- annual_spp_flux_boots %>%
    group_by(site, taxon) %>% dplyr::summarise(across(matches('flux'), list(mean = ~mean(.x, na.rm = TRUE),
                                                                            quant2.5 = ~quantile(.x, 0.025, na.rm = TRUE),
                                                                            quant25 = ~quantile(.x, 0.25, na.rm = TRUE),
                                                                            quant50 = ~quantile(.x, 0.50, na.rm = TRUE),
                                                                            quant75 = ~quantile(.x, 0.75, na.rm = TRUE),
                                                                            quant97.5 = ~quantile(.x, 0.975, na.rm = TRUE))))
  
  annual_comm_flux_summary <- flux_boots_df %>%
    group_by(site, boot_id, diet_item, taxon) %>% 
    dplyr::summarise(flux_mg_m_y = sum(flux_mg_m_third))%>% ungroup %>%
    group_by(site, boot_id) %>% dplyr::summarise(across(matches('flux'), list(flux_mg_m_y = ~sum(.x, na.rm = TRUE)), .names = "{.fn}")) %>%
    group_by(site) %>% dplyr::summarise(across(matches('flux'), list(mean = ~mean(.x, na.rm = TRUE),
                                                                     quant2.5 = ~quantile(.x, 0.025, na.rm = TRUE),
                                                                     quant25 = ~quantile(.x, 0.25, na.rm = TRUE),
                                                                     quant50 = ~quantile(.x, 0.50, na.rm = TRUE),
                                                                     quant75 = ~quantile(.x, 0.75, na.rm = TRUE),
                                                                     quant97.5 = ~quantile(.x, 0.975, na.rm = TRUE)))) %>%
    dplyr::mutate(site = factor(site, levels = stream_order))
  
  int_spp_flux_boots = flux_boots_df %>% inner_join(int_spp_meta %>% bind_rows %>% select(site = 'site_id', date_id, jul_day, y_day, yr_third, boot_id, taxon = 'taxon_id', prop_prod )) %>%
    # group_by(site, date_id, jul_day, y_day, yr_third ) %>%
    dplyr::mutate(flux_mg_m_int = prop_prod*flux_mg_m_third)
  
  return(list(flux_boots_df = flux_boots_df, annual_spp_diet_flux_summary = annual_spp_diet_flux_summary, annual_spp_flux_boots = annual_spp_flux_boots, annual_spp_flux_summary = annual_spp_flux_summary , annual_comm_flux_summary = annual_comm_flux_summary, int_spp_flux_boots = int_spp_flux_boots))
  
}
