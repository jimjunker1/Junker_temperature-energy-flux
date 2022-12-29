##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param spp_rankings
analyze_gini <- function(ann_spp_flux = flux_summaries[["annual_spp_flux_boots"]]) {

  
  stream_gini_df = ann_spp_flux %>% dplyr::filter(flux_mg_m_y >0) %>%
    named_group_split(site) %>% rlist::list.subset(names(stream_order_list)) %>%
    future_map(~.x %>% named_group_split(boot_id) %>% 
          future_map(~.x %>% select(flux_mg_m_y) %>% unlist %>% gini_even(.)) %>% bind_rows(.id = 'boot_id')) %>%
    bind_rows(.id = 'site')
  
  stream_gini_summary = stream_gini_df %>%
    group_by(site) %>%
    dplyr::summarise(across(matches('Gini'), list(mean = ~mean(.x, na.rm = TRUE),
                                                  quant2.5 = ~quantile(.x, 0.025, na.rm = TRUE),
                                                  quant25 = ~quantile(.x, 0.25, na.rm = TRUE),
                                                  quant50 = ~quantile(.x, 0.50, na.rm = TRUE),
                                                  quant75 = ~quantile(.x, 0.75, na.rm = TRUE),
                                                  quant97.5 = ~quantile(.x, 0.975, na.rm = TRUE)))) %>%
    dplyr::mutate(site = factor(site, levels = stream_order))
  
  return(list(stream_gini_df = stream_gini_df, stream_gini_summary = stream_gini_summary))
  # 
  # spp_ginis = spp_rankings %>%
  #   map(~.x %>% 
  #         map(~.x %>% group_by(site_id) %>%
  #         dplyr::mutate(across(contains('mean'),  ~ .x/sum(.x), .names = "{.col}_relative")) %>%
  #           dplyr::arrange(across(contains('rank'))) %>%
  #           dplyr::mutate(across(contains('relative'), ~cumsum(.x), .names = "{.col}_cumulative"))))
  
}
