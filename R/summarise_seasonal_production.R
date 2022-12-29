##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param int_spp_boots
summarise_seasonal_production <- function(int_spp_boots =
                                      production_boots[["int_spp_boots"]]) {

  
# combine produciton into Jan-Apr, May-Aug, Sep-Dec
  
    
 season_spp_boots_full = future_map(int_spp_boots, ~.x %>%
                           dplyr::mutate(month_id = lubridate::month(as.Date(date_id)),
                                                       yr_third = case_when(month_id %in% 1:4 ~ "first",
                                                                             month_id %in% 5:8 ~ "second",
                                                                             month_id %in% 9:12 ~ "third"),
                                         jul_day = julian(as.Date(date_id), origin = as.Date("2010-01-01")),
                                         y_day = lubridate::yday(date_id)) %>%
                           group_by(site_id, taxon_id, boot_id,  yr_third) %>%
                           dplyr::summarise(prod_mg_m_int = sum(prod_mg_m_int),
                                            start_date = as.Date(min(jul_day), origin = as.Date("2010-01-01")),
                                            end_date = as.Date(max(y_day), origin = as.Date("2010-01-01")))) %>%
                               rlist::list.subset(names(stream_order_list))
 
 season_spp_summaries = season_spp_boots_full %>% 
   map(~.x %>% ungroup %>%
         group_by(site_id, taxon_id, start_date, end_date) %>%
         dplyr::summarise(across(matches('prod'), list(mean = ~mean(.x, na.rm = TRUE),
                                                       quant2.5 = ~quantile(.x, 0.025, na.rm = TRUE),
                                                       quant25 = ~quantile(.x, 0.25, na.rm = TRUE),
                                                       quant50 = ~quantile(.x, 0.50, na.rm = TRUE),
                                                       quant75 = ~quantile(.x, 0.75, na.rm = TRUE),
                                                       quant97.5 = ~quantile(.x, 0.975, na.rm = TRUE))))) %>% 
  bind_rows %>% dplyr::mutate(site_id = factor(site_id, levels = stream_order))
 
season_spp_boots_split = season_spp_boots_full %>%
  map(~.x %>% ungroup %>% named_group_split(yr_third)) %>%
  rlist::list.subset(names(stream_order_list))

return(list(season_spp_boots_full = season_spp_boots_full, season_spp_summaries = season_spp_summaries, season_spp_boots_split = season_spp_boots_split))
}
