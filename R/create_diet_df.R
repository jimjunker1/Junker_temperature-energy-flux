##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param gut_lists
##' @param spp_list
##' @param diet_model
create_diet_df <- function(gut_lists = readRDS("./data/gut_lists.rds"), spp_list =
                           production_summaries[["production_spp_list"]]) {
  
  # period list 
  period_list = list(c('first','second','third'))
  
  spp_list = spp_list %>% rlist::list.subset(names(stream_order_list))
  
  resource_list = gut_lists[["resource_list"]] %>% rlist::list.subset(names(stream_order_list))
  
  #create full expanded df of site, taxon, 
  stream_diet_full_dfs = pmap(list(spp_list, resource_list, period_list), ~expand.grid(taxon = unlist(..1),
                                                                                  diet_item = unlist(..2), 
                                                                                  yr_third = unlist(..3),
                                                                                  KEEP.OUT.ATTRS = FALSE,
                                                                                  stringsAsFactors = FALSE) %>%
                           dplyr::mutate(rel_area = NA_real_)) %>% 
                             bind_rows(.id = 'site') %>%
    dplyr::mutate(id = NA) %>% select(site, yr_third, taxon, id, diet_item, rel_area) %>%
    dplyr::mutate(n_id = interaction(site, yr_third, taxon))
  
  # create a diet df of all present diets
  diet_list = gut_lists[['diet_list']] %>% bind_rows %>% ungroup %>%
    dplyr::mutate(month_id = match(month_id, month.abb),
                  yr_third = case_when(month_id %in% 1:4 ~ "first",
                                       month_id %in% 5:8 ~ "second",
                                       month_id %in% 9:12 ~ "third")) %>%
    select(site, yr_third, taxon, id, diet_item, rel_area) %>%
    dplyr::mutate(n_id = interaction(site, yr_third, taxon))
    
  # full diets with all NAs present  
  stream_diet_missings = anti_join(stream_diet_full_dfs, diet_list, by = "n_id") %>%
    bind_rows(diet_list) %>% select(-n_id)

  return(stream_diet_dfs = stream_diet_missings)
  }
