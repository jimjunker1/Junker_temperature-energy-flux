##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title

read_production <- function() {

  ####    full raw production, biomass, abundance import and clean    ####
  prod_full_list = readRDS(file = "./data/prod_full_fin.rds") %>%
    #rename Tubificid update to Naidid to recognize taxon changes
    furrr::future_map(~.x %>% furrr::future_map(~.x %>% 
                                    # rename and consolidate a few taxa to maintain resolution among streams
                                    dplyr::mutate(taxon_id = case_when(grepl("Ceratopogonidae.+", taxon_id) ~ "Ceratopegonid",
                                                                       grepl("Eukiefferiella.+", taxon_id) ~ "Eukiefferiella sp.",
                                                                       grepl("Oligochaeta.*",taxon_id) ~ "Lumbricidae",
                                                                       TRUE ~ taxon_id)) %>%
                                    dplyr::mutate(taxon_id = case_when(stringr::str_detect("Tubificid 1",taxon_id) ~ "Naidid 1",
                                                                             stringr::str_detect("Tub. 2", taxon_id) ~ "Naidid 2",
                                                                             stringr::str_detect(" Eukiefferiella|Eukiefferiella", taxon_id) ~ "Eukiefferiella sp.",
                                                                             stringr::str_detect("Lumb 3", taxon_id) ~ "Lumbricidae",
                                                                             stringr::str_detect("Orthocladius fridgidus", taxon_id) ~ "Orthocladius frigidus",
                                                                             TRUE ~ taxon_id )) %>% group_by(site_id, taxon_id, date_id, boot_id) %>%
                                    dplyr::summarise(across(where(is.numeric), ~sum(.x))) %>%
                                    ungroup)) %>%
    rlist::list.subset(names(stream_order_list))
  
  spp_removals = list(hver = c('Homoptera', 'Lepidoptera', 'Limnophora.pupa', 'Midge.pupa', 
                               'Slug', 'Snail.egg', 'Thrips'),
                      st6 = c("Homoptera", "Limnophora.pupa", "Midge.pupa", "S..vittatum.pupa"), 
                      st9 = c('Clinocera.pupa','Homoptera', 'Limnophora.pupa',
                              'Limnophora.egg', 'Midge.pupa', 'S..vittatum.pupa', 'Slug',
                              'Snail.egg','Thrips'),
                      oh2 = c(),
                      st7 = c(),
                      st14 = c('Homoptera', 'S..vernum.pupa', 'S..vitattum.pupa', 'Spider..terr..','Thrips','S. vitattum pupa','S. vernum pupa')) %>%
    rlist::list.subset(names(stream_order_list))
  
  ## remove species with no 
  
  prod_full_list = purrr::map2(prod_full_list, spp_removals, function(prod, removals){
    map(prod, ~.x %>% dplyr::filter(taxon_id %ni% removals))})
    
  ##  
  int_spp_boots = prod_full_list %>% map(~rlist::list.subset(.,grepl("Pint", names(.))) %>% flatten_df)%>%
    rlist::list.subset(names(stream_order_list))
  
  # stream community-level summaries 
  # interval level community summary
  int_comm_boots = map(prod_full_list, ~.x %>%
                           map(~.x %>%
                                 group_by(site_id, date_id, boot_id) %>% 
                                 dplyr::select(-taxon_id) %>%
                                 dplyr::summarise(across(where(is.numeric), list(sum = sum)))) %>%
                           reduce(left_join) %>% 
                           dplyr::rename(prod_mg_m_int = 'prod_mg_m_int_sum',
                                  bio_mg_m = 'bio_mg_m_sum',
                                  n_ind_m = 'n_ind_m_sum') %>%
                           ungroup %>% group_by(boot_id) %>%
                           dplyr::mutate(pb_int = prod_mg_m_int/bio_mg_m,
                                  M_mg_ind = bio_mg_m/n_ind_m))%>%
    rlist::list.subset(names(stream_order_list))
  
  # annual community summaries
  ann_comm_boots = map(int_comm_boots, ~.x %>%
                           group_by(site_id, boot_id) %>%
                           dplyr::select(-pb_int, -M_mg_ind) %>%
                           dplyr::summarise(across(matches('bio|ind'), list(mean = mean)),
                                     across(contains('prod'), sum)) %>%
                           dplyr::rename(prod_mg_m_y = 'prod_mg_m_int',
                                  bio_mg_m = 'bio_mg_m_mean',
                                  n_ind_m = 'n_ind_m_mean') %>%
                           dplyr::mutate(pb_y = prod_mg_m_y/bio_mg_m,
                                  M_mg_ind = bio_mg_m/n_ind_m))%>%
    rlist::list.subset(names(stream_order_list))
  
  # stream species summaries 
  int_spp_summary = map(prod_full_list, ~.x %>% 
                          reduce(left_join) %>%
                          dplyr::mutate(pb_int = prod_mg_m_int/bio_mg_m,
                                        M_mg_ind = bio_mg_m/n_ind_m) %>%
                          dplyr::mutate(across(matches("pb|M"), ~ ifelse(is.na(.x) | is.infinite(.x) | .x == 0, NA, .x))) %>%
                              group_by(site_id, date_id, taxon_id) %>%
                              dplyr::summarise(across(matches("bio|ind|prod|pb"), list(mean = ~mean(.x, na.rm = TRUE),
                                                                                    quant2.5 = ~quantile(.x, 0.025, na.rm = TRUE),
                                                                                    quant25 = ~quantile(.x, 0.25, na.rm = TRUE),
                                                                                    quant50 = ~quantile(.x, 0.50, na.rm = TRUE),
                                                                                    quant75 = ~quantile(.x, 0.75, na.rm = TRUE),
                                                                                    quant97.5 = ~quantile(.x, 0.975, na.rm = TRUE)))) %>%
                          dplyr::mutate(across(everything(), ~ ifelse(is.na(.x) | .x == 0, NA, .x))))%>%
    rlist::list.subset(names(stream_order_list))
  
  ann_spp_boots = map(prod_full_list, ~.x %>%
                          map(~.x %>%
                                group_by(site_id, taxon_id, boot_id) %>%
                                dplyr::select(-date_id) %>%
                                dplyr::summarise(across(matches('bio|ind'), list(mean = mean)),
                                          across(contains('prod'), sum))) %>%
                          reduce(left_join) %>% 
                          dplyr::rename(prod_mg_m_y = "prod_mg_m_int",
                                 bio_mg_m = 'bio_mg_m_mean',
                                 n_ind_m = 'n_ind_m_mean') %>%
                          ungroup %>% group_by(boot_id) %>%
                          dplyr::mutate(pb_y = prod_mg_m_y/bio_mg_m,
                                 M_mg_ind = bio_mg_m/n_ind_m))%>%
    rlist::list.subset(names(stream_order_list))
  
  # build a summary df to back out interval level flux after combining into thirds
  int_spp_meta = map(int_spp_boots, ~.x %>%
                          dplyr::mutate(month_id = lubridate::month(as.Date(date_id)),
                                        yr_third = case_when(month_id %in% 1:4 ~ "first",
                                                             month_id %in% 5:8 ~ "second",
                                                             month_id %in% 9:12 ~ "third"),
                                        jul_day = julian(as.Date(date_id), origin = as.Date("2010-01-01")),
                                        y_day = lubridate::yday(date_id)) %>%
                          group_by(site_id, taxon_id, boot_id, yr_third) %>%
                          dplyr::mutate(cum_prod = sum(prod_mg_m_int, na.rm = TRUE),
                                        prop_prod = prod_mg_m_int/cum_prod) %>%
                          ungroup) %>% rlist::list.subset(names(stream_order_list))
  
  return(list(int_comm_boots = int_comm_boots, ann_comm_boots = ann_comm_boots, ann_spp_boots = ann_spp_boots, int_spp_boots = int_spp_boots, int_spp_summary = int_spp_summary, int_spp_meta = int_spp_meta))

}
