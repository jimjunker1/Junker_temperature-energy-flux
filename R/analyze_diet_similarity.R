##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param full_diet_df
##' @param modeled_diets
analyze_diet_similarity <- function(gut_lists = readRDS(file_in("./data/gut_lists.rds")), diet_predictions = modeled_diets[["diet_predictions"]]) {
  
  gut_df = gut_lists[["diet_list"]]

  
  wide_diets_list = gut_df %>% bind_rows %>% ungroup %>% select(site, month_id, taxon, id, diet_item, rel_area) %>%
    group_by(site, month_id, taxon, id) %>%
    pivot_wider( names_from = 'diet_item', values_from = 'rel_area') %>% 
    named_group_split(site) %>%
    rlist::list.subset(names(stream_order_list))
  
  RInSp_lists = wide_diets_list %>% map(~RInSp::import.RInSp(.x, col.header = TRUE, info.cols = c(1:4), data.type = 'double'))
  
  within_empirical_overlap = RInSp_lists %>% map(function(x){
    y = RInSp::overlap(x)
    diag(y[['overlapmatrix']]) <- NA
    y = append(y, list(min_overlap = min(y[['overlapmatrix']], na.rm = TRUE),
                       max_overlap = max(y[['overlapmatrix']], na.rm = TRUE)))
    })
  
  within_diet_overlap_minmax = within_empirical_overlap %>% 
    map(~.x %>% .[grepl("min|max", names(.))]) %>%
    bind_rows(.id = 'site')
  
  # estimate across stream diet overlaps from modeled diets
  
  diet_predictions %>%
    dplyr::mutate(site = factor(site, levels = names(stream_order_list))) %>%
    group_by(site, taxon, boot_id, diet_item) %>%
    dplyr::summarise(summed_area = sum(rel_area, na.rm = TRUE)) %>%
    dplyr::mutate(rel_area = summed_area/sum(summed_area, na.rm = TRUE)) %>%
    select(site, taxon, boot_id, diet_item, rel_area) %>%
    ungroup -> annual_diet_predictions
  
  annual_diet_predictions %>% pivot_wider(names_from = 'diet_item', values_from = 'rel_area') %>%
    named_group_split(site) %>%
    future_map(~.x %>% named_group_split(boot_id) %>%
          future_map(~RInSp::import.RInSp(.x, col.header = TRUE, info.cols = c(1:3), data.type = 'double')) %>%
          future_map(function(x){
            y = RInSp::overlap(x)
            diag(y[['overlapmatrix']]) <- NA
            y = append(y, list(min_overlap = min(y[['overlapmatrix']], na.rm = TRUE),
                               max_overlap = max(y[['overlapmatrix']], na.rm = TRUE)))
          })) -> within_modeled_overlap
  
  within_overlap_summ = within_modeled_overlap %>%
    map(~.x %>% map(~.x %>% pluck("meanoverlap")) %>% unlist) %>%
    bind_rows(.id = 'site') %>%
    pivot_longer(-site, names_to = 'boot_id', values_to = 'meanoverlap') %>%
    group_by(site) %>% 
    dplyr::summarise(across('meanoverlap', list(mean = ~mean(.x, na.rm = TRUE),
                                                        quant2.5 = ~quantile(.x, 0.025, na.rm = TRUE),
                                                        quant25 = ~quantile(.x, 0.25, na.rm = TRUE),
                                                        quant50 = ~quantile(.x, 0.50, na.rm = TRUE),
                                                        quant75 = ~quantile(.x, 0.75, na.rm = TRUE),
                                                        quant97.5 = ~quantile(.x, 0.975, na.rm = TRUE))))
  
  annual_diet_predictions %>% group_by(site, boot_id, diet_item) %>%
    dplyr::summarise(summed_area = sum(rel_area, na.rm = TRUE)) %>%
    dplyr::mutate(rel_area = summed_area/sum(summed_area, na.rm = TRUE)) %>%
    select(-summed_area) %>%
    pivot_wider(names_from = 'diet_item', values_from = 'rel_area') %>%
    named_group_split(boot_id) %>%
    future_map(~RInSp::import.RInSp(.x, col.header = TRUE, info.cols = c(1:2), data.type = 'double')) %>%
    future_map(function(x){
      y = quiet(RInSp::overlap(x))
      diag(y[['overlapmatrix']]) <- NA
           y = append(y, list(min_overlap = min(y[['overlapmatrix']], na.rm = TRUE),
                              max_overlap = max(y[['overlapmatrix']], na.rm = TRUE)))
           }) -> among_modeled_overlap
  
  among_overlap_mean = among_modeled_overlap %>%
    map(~.x %>% pluck('meanoverlap')) %>% unlist
  
  among_overlap_array = among_modeled_overlap %>%
    map(~.x %>% pluck('overlapmatrix')) %>% abind::abind(.) %>%
    array(., dim = c(6,6,1000))
  
  among_overlap_list = among_modeled_overlap %>%
    map(~.x %>% pluck('overlapmatrix') %>% data.frame %>%
          setNames(., nm = names(stream_order_list)) %>%
          dplyr::mutate(site = names(stream_order_list)))
  
  pair_overlap_mean = among_overlap_array %>%
    apply(., c(1,2), function(x) mean(x, na.rm = TRUE))
  
  pair_overlap_quantiles = among_overlap_array%>%
   apply(., c(1,2), function(x) quantile(x,probs = c(0.025, 0.5, 0.975), na.rm = TRUE))
  
  return(list(within_diet_overlap = within_empirical_overlap, within_diet_overlap_minmax = within_diet_overlap_minmax,
              within_overlap_summ = within_overlap_summ, among_modeled_overlap = among_modeled_overlap,
              among_overlap_array = among_overlap_array, among_overlap_mean = among_overlap_mean, 
              pair_overlap_mean = pair_overlap_mean, pair_overlap_quantiles = pair_overlap_quantiles))
  
}
