##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param season_spp_split
split_seasonal_production <- function(season_spp_split =
                                  seasonal_production[["season_spp_boots_split"]]) {

 prod_boot_split = map(season_spp_split, ~.x %>% map(~.x %>% named_group_split(boot_id)))
 
 return(prod_boot_split)
 
}
