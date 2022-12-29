##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param gut_lists
split_seasonal_diets <- function(gut_lists) {
  ## ++++ Helper functions ++++ ##
  
  split_diets <- function(x){
    
    if(any(is.na(x$date))){
      diet_list = vector('list',3L)
      for(i in 1:3){
        diet_list[[i]] <- x
      }
      return(diet_list)
    } else {
      diet_list = x %>%
        dplyr::mutate(month_id = match(month_id, month.abb),
                      yr_third = case_when(month_id %in% 1:4 ~ "first",
                                           month_id %in% 5:8 ~ "second",
                                           month_id %in% 9:12 ~ "third")) %>%
        ungroup %>% group_split(yr_third)
      
      if(length(diet_list) < 3){
        list.length = length(diet_list)
        to_add = 3-list.length
        for(i in 1:to_add){
          diet_list[[(i+list.length)]] <- x %>% 
            group_by(site, date, taxon, diet_item) %>% 
            dplyr::summarise(rel_area = NA)
        }
      }
      
      return(diet_list)
    }
  }
  ## ++++ End Helper functions ++++ ##
  debugonce(split_diets)
  diets = gut_lists[['diet_list']]
  diet_splits = diets %>% map(~split_diets(.x))

}
