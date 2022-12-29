##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param production_boots
summarise_production <- function(production_boots) {
  # stream community-level summaries 
  # interval level community summary
  int_comm_summary = production_boots[["int_comm_boots"]] %>%
                             map(~.x %>%
                             ungroup %>% group_by(site_id, date_id) %>%
                             dplyr::summarise(across(matches('prod|bio|ind|pb'), list(mean = ~mean(.x, na.rm = TRUE),
                                                                               quant2.5 = ~quantile(.x, 0.025, na.rm = TRUE),
                                                                               quant25 = ~quantile(.x, 0.25, na.rm = TRUE),
                                                                               quant50 = ~quantile(.x, 0.50, na.rm = TRUE),
                                                                               quant75 = ~quantile(.x, 0.75, na.rm = TRUE),
                                                                               quant97.5 = ~quantile(.x, 0.975, na.rm = TRUE))))) %>%
    bind_rows
  
  # annual community summaries
    ann_comm_summary = production_boots[["ann_comm_boots"]] %>%
                             map( ~.x %>%
                             ungroup %>% group_by(site_id) %>%
                             dplyr::summarise(across(matches('prod|bio|ind|pb'), list(mean = ~mean(.x, na.rm = TRUE),
                                                                               quant2.5 = ~quantile(.x, 0.025, na.rm = TRUE),
                                                                               quant25 = ~quantile(.x, 0.25, na.rm = TRUE),
                                                                               quant50 = ~quantile(.x, 0.50, na.rm = TRUE),
                                                                               quant75 = ~quantile(.x, 0.75, na.rm = TRUE),
                                                                               quant97.5 = ~quantile(.x, 0.975, na.rm = TRUE))))) %>%
    bind_rows
  
  
  # stream species summaries 
   int_spp_summary = production_boots[["int_spp_summary"]] 
    
   ann_spp_summary = production_boots[["ann_spp_boots"]] %>%
                           map(~.x %>%
                           group_by(site_id, taxon_id) %>%
                           dplyr::summarise(across(matches('prod|bio|ind|pb'), list(mean = ~mean(.x, na.rm = TRUE),
                                                                             quant2.5 = ~quantile(.x, 0.025, na.rm = TRUE),
                                                                             quant25 = ~quantile(.x, 0.25, na.rm = TRUE),
                                                                             quant50 = ~quantile(.x, 0.50, na.rm = TRUE),
                                                                             quant75 = ~quantile(.x, 0.75, na.rm = TRUE),
                                                                             quant97.5 = ~quantile(.x, 0.975, na.rm = TRUE))))) %>% 
   bind_rows
  
   production_spp_list = ann_spp_summary %>% named_group_split(site_id) %>% map(~.x %>% select(taxon_id)) %>% rlist::list.subset(names(stream_order_list))
   
   return(list(int_comm_summary = int_comm_summary, ann_comm_summary = ann_comm_summary, ann_spp_summary = ann_spp_summary, int_spp_summary= int_spp_summary, production_spp_list = production_spp_list))

}
