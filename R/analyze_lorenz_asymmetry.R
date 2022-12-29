##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param lorenz_analysis
analyze_lorenz_asymmetry <- function(lorenz_analysis) {
  # debugonce(Lasym_strict)
  # ord_vars = list('flux_mg_m_y','flux_mg_m_y_mean','pb_y','bio_mg_m','M_mg_ind')
  # Lasym_boots = lorenz_analysis %>%
  #   map2(.,ord_vars, ~.x %>%
  #         named_group_split(site) %>%
  #         map(~.x %>% group_by(boot_id) %>%
  #               dplyr::summarise(Lasym = Lasym_strict(x = .x$flux_mg_m_y, ord_var = .y))))
  # NULL
  
  

}

# debugonce(Lasym_strict)
# lorenz_analysis[[3]] %>% named_group_split(site) %>%
#   map(~.x %>% group_by(boot_id) %>%
#   dplyr::summarise(Lasym = Lasym_strict(x = .$flux_mg_m_y, ord_var = 'pb_y')))
# 
# 
# debugonce(Lasym_strict)
# Lasym_strict(x$flux_mg_m_y, ord_var = pb_y)
