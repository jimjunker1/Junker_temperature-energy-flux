##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param ann_spp_flux
analyze_lorenz <- function(ann_spp_flux = flux_summaries[["annual_spp_flux_boots"]], spp_rankings_boots = spp_rankings_boots) {

  ## ++++ Helper functions ++++ ##
  
boot_rank_function <- function(flux_list,trait_list, value = NULL){
  if(is.null(value))print("Warning: value is not specified. Column #4 will be used")
  flux_boots = flux_list 
  trait_boots = trait_list %>% dplyr::rename(site = 'site_id', taxon = 'taxon_id' ) %>%
    dplyr::mutate(taxon = make.names(taxon))
  
  trait_flux_list = flux_boots %>% left_join(trait_boots, by = c("site", "boot_id", "taxon")) %>% named_group_split(boot_id)
if(value != "bio_mg_m"){
  ranked_boots = future_map(trait_flux_list,~.x %>%
                        dplyr::arrange(across(contains('rank'))) %>%
                        dplyr::mutate(cumul_flux = cumsum(flux_mg_m_y),
                                      rel_flux = cumul_flux/sum(flux_mg_m_y),
                                      spp_id = 1:n()) %>%
                        dplyr::mutate(across(matches('rank'), list(rel_spp = ~.x/max(.x, na.rm = TRUE)), .names = "{.fn}")) %>%
                          bind_rows(data.frame(site = stream_order, rel_flux = rep(0, length(stream_order)), rel_spp = rep(0, length(stream_order)), rel_bio = rep(0,length(stream_order))))%>%
                          data.frame) %>%
    bind_rows(.id = 'boot_id')
} else{
  ranked_boots = future_map(trait_flux_list,~.x %>%
                              dplyr::arrange(across(contains('rank'))) %>%
                              dplyr::mutate(cumul_flux = cumsum(flux_mg_m_y),
                                            cumul_bio = cumsum(bio_mg_m),
                                            rel_flux = cumul_flux/sum(flux_mg_m_y),
                                            rel_bio = cumul_bio/sum(bio_mg_m),
                                            spp_id = 1:n()) %>%
                              dplyr::mutate(across(matches('rank'), list(rel_spp = ~.x/max(.x, na.rm = TRUE)), .names = "{.fn}")) %>%
                              bind_rows(data.frame(site = stream_order, rel_flux = rep(0, length(stream_order)), rel_spp = rep(0, length(stream_order)), rel_bio = rep(0,length(stream_order))))%>%
                              data.frame) %>%
    bind_rows(.id = 'boot_id')
}
  
  return(ranked_boots)
}
  
  ## ++++ End Helper functions ++++ ##

  ann_spp_flux_list = ann_spp_flux %>%
    dplyr::filter(flux_mg_m_y >0) %>%#remove zeros
    named_group_split(site) %>%
    rlist::list.subset(names(stream_order_list))
  
  relative_flux_df = ann_spp_flux_list %>% 
    future_map(~.x %>% named_group_split(boot_id) %>% 
          future_map(~.x %>% dplyr::arrange(flux_mg_m_y) %>%
                dplyr::mutate(cumul_flux = cumsum(flux_mg_m_y),
                              rel_flux = cumul_flux/sum(flux_mg_m_y),
                              spp_id = 1:n(),
                              rel_spp = spp_id/max(spp_id)) %>%
                  bind_rows(data.frame(site = stream_order, rel_flux = rep(0, length(stream_order)), rel_spp = rep(0, length(stream_order))))) %>% 
            bind_rows(.id = 'boot_id')) %>%
    bind_rows(.id = 'site') %>%
    dplyr::mutate(site = factor(site, levels = names(stream_order_list)))
  
  relative_flux_summary = ann_spp_flux %>%
    dplyr::filter(flux_mg_m_y >0) %>%
    group_by(site, taxon) %>%
    dplyr::summarise(across(matches('flux'), list(mean = ~mean(.x, na.rm = TRUE)))) %>%
    ungroup %>% group_by(site) %>%
    dplyr::arrange(flux_mg_m_y_mean) %>%
    dplyr::mutate(cumul_flux = cumsum(flux_mg_m_y_mean),
                  rel_flux = cumul_flux/sum(flux_mg_m_y_mean),
                  spp_id = 1:n(),
                  rel_spp = spp_id/max(spp_id)) %>%
    bind_rows(data.frame(site = stream_order, rel_flux = rep(0, length(stream_order)), rel_spp = rep(0, length(stream_order))))%>%
    dplyr::mutate(site = factor(site, levels = names(stream_order_list)))
  
  # debugonce(boot_rank_function)
  pb_ranking_boots <- ann_spp_flux_list %>% 
    map2(., spp_rankings_boots[["PB_spp_rank_boots"]], ~boot_rank_function(.x,.y, value = "pb_y")) %>%
    rlist::list.subset(names(stream_order_list)) %>% bind_rows %>%
    dplyr::mutate(site = factor(site, levels = names(stream_order_list)))
  
  bio_ranking_boots <- ann_spp_flux_list %>%
    map2(., spp_rankings_boots[["bio_spp_rank_boots"]], ~boot_rank_function(.x, .y, value = "bio_mg_m")) %>%
    rlist::list.subset(names(stream_order_list)) %>% bind_rows%>%
    dplyr::mutate(site = factor(site, levels = names(stream_order_list)))
  
  M_ranking_boots <- ann_spp_flux_list %>%
    map2(., spp_rankings_boots[["M_spp_rank_boots"]], ~boot_rank_function(.x, .y, value = "M_mg_ind")) %>%
    rlist::list.subset(names(stream_order_list)) %>% bind_rows %>%
    dplyr::mutate(site = factor(site, levels = names(stream_order_list)))
      
  
  return(list(relative_flux_df = relative_flux_df, relative_flux_summary = relative_flux_summary, 
              pb_ranking_boots = pb_ranking_boots, bio_ranking_boots = bio_ranking_boots, 
              M_ranking_boots = M_ranking_boots))

}
