##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param ann_spp_summary
##' @param ann_flux_summary
rank_spp_traits <- function(ann_spp_summary = production_summaries[["ann_spp_summary"]], ann_flux_summary = flux_summaries[["annual_spp_flux_summary"]]) {
  
  ann_spp_summary = ann_spp_summary %>% dplyr::rename(site = 'site_id', taxon = 'taxon_id' ) %>%
    dplyr::select(site, taxon, matches('mean')) %>% dplyr::mutate(taxon = make.names(taxon))
  ann_flux_summary = ann_flux_summary %>% dplyr::select(site, taxon, matches('mean'))
  # create ranked lists for analysis
  # # sum across intervals for each stream
  prod_spp_rank = ann_spp_summary %>%
    left_join(ann_flux_summary, by = c('site', 'taxon')) %>%
    # purrr::map(~.x %>%
                 dplyr::select(site, taxon, matches('prod.*_mean'), matches('flux')) %>%
                 dplyr::filter_at(vars(matches('flux')), all_vars(. > 0)) %>% 
                 dplyr::mutate(prod_ann_rank = dense_rank(prod_mg_m_y_mean)) %>%
                 dplyr::arrange(across(matches('.*ann_rank'))) %>%
                 dplyr::mutate(cumul_flux = cumsum(flux_mg_m_y_mean),
                               rel_flux = cumul_flux/sum(flux_mg_m_y_mean)) %>%
                 dplyr::mutate(across(matches('.*ann_rank'), list(rel_spp = ~.x/max(.x, na.rm = TRUE)), .names = "{.fn}")) %>%
    bind_rows(data.frame(site = stream_order, rel_flux = rep(0, length(stream_order)), rel_spp = rep(0, length(stream_order)))) %>%
    # junkR::named_group_split(site) %>%
    # rlist::list.subset(names(stream_order_list))
    dplyr::mutate(site = factor(site, levels = names(stream_order_list)))
  
  # biomass
  # # sum across intervals for each stream
  bio_spp_rank = ann_spp_summary %>%
    left_join(ann_flux_summary, by = c('site', 'taxon')) %>%
    # purrr::map(~.x %>%
          dplyr::select(site, taxon, matches('bio.*_mean'), matches('flux')) %>%
    dplyr::filter_at(vars(matches('flux')), all_vars(. > 0)) %>% 
    dplyr::mutate(bio_ann_rank = dense_rank(bio_mg_m_mean)) %>%
            dplyr::arrange(across(matches('.*ann_rank'))) %>%
            dplyr::mutate(cumul_flux = cumsum(flux_mg_m_y_mean),
                          cumul_bio = cumsum(bio_mg_m_mean),
                          rel_flux = cumul_flux/sum(flux_mg_m_y_mean),
                          rel_bio = cumul_bio/max(cumul_bio)) %>%
            dplyr::mutate(across(matches('.*ann_rank'), list(rel_spp = ~.x/max(.x, na.rm = TRUE)), .names = "{.fn}")) %>%
     bind_rows(data.frame(site = stream_order, rel_flux = rep(0, length(stream_order)), rel_spp = rep(0, length(stream_order)), rel_bio = rep(0,length(stream_order)))) %>%
    # junkR::named_group_split(site) %>%
    # rlist::list.subset(names(stream_order_list))
    dplyr::mutate(site = factor(site, levels = names(stream_order_list)))
  
  # abundance
  # # sum across intervals for each stream
  n_spp_rank = ann_spp_summary %>%
    left_join(ann_flux_summary, by = c('site', 'taxon')) %>%
    # purrr::map(~.x %>%
          dplyr::select(site, taxon, matches('n.*_mean'), matches('flux')) %>%
    dplyr::filter_at(vars(matches('flux')), all_vars(. > 0)) %>% 
    dplyr::mutate(n_ann_rank = dense_rank(n_ind_m_mean)) %>%
            dplyr::arrange(across(matches('.*ann_rank'))) %>%
            dplyr::mutate(cumul_flux = cumsum(flux_mg_m_y_mean),
                          rel_flux = cumul_flux/sum(flux_mg_m_y_mean)) %>%
            dplyr::mutate(across(matches('.*ann_rank'), list(rel_spp = ~.x/max(.x, na.rm = TRUE)), .names = "{.fn}")) %>%
    bind_rows(data.frame(site = stream_order, rel_flux = rep(0, length(stream_order)), rel_spp = rep(0, length(stream_order)))) %>%
    # junkR::named_group_split(site) %>%
    # rlist::list.subset(names(stream_order_list))
    dplyr::mutate(site = factor(site, levels = names(stream_order_list)))
  
  
  # body size, *M*
  M_spp_rank = ann_spp_summary %>%
    left_join(ann_flux_summary, by = c('site', 'taxon')) %>%
    # purrr::map(~.x %>%
          dplyr::select(site, taxon, matches('M_mg.*mean'), matches('flux')) %>%
    dplyr::filter_at(vars(matches('flux')), all_vars(. > 0)) %>% 
    dplyr::mutate(M_ann_rank = dense_rank(M_mg_ind_mean))%>%
            dplyr::arrange(across(matches('.*ann_rank'))) %>%
            dplyr::mutate(cumul_flux = cumsum(flux_mg_m_y_mean),
                          rel_flux = cumul_flux/sum(flux_mg_m_y_mean)) %>%
            dplyr::mutate(across(matches('.*ann_rank'), list(rel_spp = ~.x/max(.x, na.rm = TRUE)), .names = "{.fn}")) %>%
    bind_rows(data.frame(site = stream_order, rel_flux = rep(0, length(stream_order)), rel_spp = rep(0, length(stream_order)))) %>%
    # junkR::named_group_split(site) %>%
    # rlist::list.subset(names(stream_order_list))
    dplyr::mutate(site = factor(site, levels = names(stream_order_list)))
  
  
  # PB ratio
  PB_spp_rank = ann_spp_summary %>%
    left_join(ann_flux_summary, by = c('site', 'taxon')) %>%
    # purrr::map(~.x %>%
          dplyr::select(site, taxon, matches('pb.*_mean'), matches('flux')) %>%
    dplyr::filter_at(vars(matches('flux')), all_vars(. > 0)) %>% 
    dplyr::mutate(pb_ann_rank = dense_rank(pb_y_mean))%>%
            dplyr::arrange(across(matches('.*ann_rank'))) %>%
            dplyr::mutate(cumul_flux = cumsum(flux_mg_m_y_mean),
                          rel_flux = cumul_flux/sum(flux_mg_m_y_mean)) %>%
            dplyr::mutate(across(matches('.*ann_rank'), list(rel_spp = ~.x/max(.x, na.rm = TRUE)), .names = "{.fn}")) %>%
            bind_rows(data.frame(site = stream_order, rel_flux = rep(0, length(stream_order)), rel_spp = rep(0, length(stream_order)))) %>%
    # junkR::named_group_split(site) %>%
    # rlist::list.subset(names(stream_order_list))
    dplyr::mutate(site = factor(site, levels = names(stream_order_list)))
  
  
  return(list( prod_spp_rank = prod_spp_rank, bio_spp_rank = bio_spp_rank, n_spp_rank = n_spp_rank, M_spp_rank = M_spp_rank,PB_spp_rank = PB_spp_rank))

}
