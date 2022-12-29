##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param production_boots
analyze_diversity <- function(production_boots) {

  # stream community-level summaries 
  # interval level community summary
  int_spp_wide = production_boots[["int_spp_summary"]] %>% bind_rows %>%
    select(site_id:prod_mg_m_int_mean) %>% replace_na(list(prod_mg_m_int_mean = 0)) %>%
    pivot_wider(names_from = 'taxon_id', values_from = 'prod_mg_m_int_mean', values_fill = 0)
  
  set.seed(123)
  NMDS = vegan::metaMDS(int_spp_wide %>% ungroup %>%select(-site_id, -date_id))
  
  NMDS.scrs = as.data.frame(vegan::scores(NMDS, display = 'sites')) %>% 
    bind_cols(int_spp_wide %>% select(site_id, date_id)) %>%
    select(site_id, date_id, everything()) %>%
    dplyr::mutate(site_id = factor(site_id, levels = names(stream_order_list))) %>%
    group_by(site_id) %>% dplyr::arrange(date_id) 
 
    return(list(int_spp_wide = int_spp_wide,
              NMDS = NMDS,
              NMDS.scrs = NMDS.scrs))
  
  }
