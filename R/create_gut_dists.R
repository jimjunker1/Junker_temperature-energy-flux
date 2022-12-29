##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param gut_lists
create_gut_dists <- function(gut_lists = gut_lists[['diet_list']]) {

  gut_matrices = gut_lists[[1]] %>% map(~.x %>% 
                                     dplyr::select(site, date, taxon, diet_item, rel_area) %>%
                                     group_by(site,date, taxon) %>%
                                     pivot_wider(names_from = 'diet_item',values_from = 'rel_area')
                                     )

}
