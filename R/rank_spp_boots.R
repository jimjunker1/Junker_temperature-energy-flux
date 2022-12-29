##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param nameme1
rank_spp_boots <- function(ann_spp_boots = production_boots[["ann_spp_boots"]]) {

  # biomass
  # # sum across intervals for each stream
  bio_spp_rank_boots = ann_spp_boots %>%
    map(., ~.x %>%
    junkR::named_group_split(boot_id) %>%
    purrr::map(~.x %>%
                 dplyr::select(site_id, taxon_id, matches('bio.*')) %>%
                 dplyr::mutate(bio_ann_rank = dense_rank(bio_mg_m))) %>%
    bind_rows(.id = 'boot_id'))
  # abundance
  # # sum across intervals for each stream
  n_spp_rank_boots = ann_spp_boots %>%
    map(., ~.x %>%
    junkR::named_group_split(boot_id) %>%
    purrr::map(~.x %>%
                 dplyr::select(site_id, taxon_id, matches('n_ind.*')) %>%
                 dplyr::mutate(n_ann_rank = dense_rank(n_ind_m))) %>%
  bind_rows(.id = 'boot_id'))
  # body size, *M*
  M_spp_rank_boots = ann_spp_boots %>%
    map(., ~.x %>%
    junkR::named_group_split(boot_id) %>%
    purrr::map(~.x %>%
                 dplyr::select(site_id, taxon_id, matches('M.*_ind')) %>%
                 dplyr::mutate(M_ann_rank = dense_rank(M_mg_ind))) %>%
    bind_rows(.id = 'boot_id'))
  
  # PB ratio
  PB_spp_rank_boots = ann_spp_boots %>%
    map(., ~.x %>%
    junkR::named_group_split(boot_id) %>%
    purrr::map(~.x %>%
                 dplyr::select(site_id, taxon_id, matches('pb.*')) %>%
                 dplyr::mutate(pb_ann_rank = dense_rank(pb_y))) %>%
    bind_rows(.id = 'boot_id'))
  
  return(list( bio_spp_rank_boots = bio_spp_rank_boots, n_spp_rank_boots = n_spp_rank_boots, M_spp_rank_boots = M_spp_rank_boots ,PB_spp_rank_boots = PB_spp_rank_boots))
  

}
