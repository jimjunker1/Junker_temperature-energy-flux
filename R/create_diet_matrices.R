##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param diet_df
##' @param seasonal_boot_list
##' 
create_diet_matrices <- function(diet_df = modeled_diets[['diet_seasonal_boot_split']], seasonal_boot_split) {
 
  ## ++++ Helper functions ++++ ##
  
  create_matrix = function(single_diet_df, name_order){
    # create matrix from full diet proportion data.frame
    # this function takes a single_diet_df, so it must be mapped
    # if converting a list
    spp_names = unique(single_diet_df$taxon)
    spp_names = spp_names[spp_names %in% name_order]
    res_names = unique(single_diet_df$diet_item)
    full_names = c(spp_names, res_names)
    full_long_df = expand.grid(taxon = full_names, diet_item = full_names)
    res_long_df = single_diet_df %>% select(taxon, diet_item, rel_area)
    filled_long_df = join(full_long_df, res_long_df, by = c("taxon", "diet_item")) %>% dplyr::mutate(rel_area = tidyr::replace_na(rel_area, 0))
    # create matrix from long data frame
    full_matrix = filled_long_df %>% pivot_wider(names_from = 'taxon', values_from = 'rel_area', values_fn = mean) %>%
      column_to_rownames('diet_item') %>% as.matrix

    return(full_matrix)
  }
  
  ## ++++ End Helper functions ++++ ##
  # set the name order
  name_list = seasonal_boot_split %>% map(~.x %>% pluck(1) %>% pluck(1) %>% select(taxon_id) %>% unlist) %>% 
    rlist::list.subset(names(stream_order_list))
  
  #create blank stream matrices
  # debugonce(create_matrix)
  stream_matrices = map2(diet_df, name_list, function(x,y){x %>% 
     map(., ~.x %>% future_map(.,~create_matrix(.x,y )))}) %>% rlist::list.subset(names(stream_order_list))
  
  return(stream_matrices = stream_matrices)
  
}
