#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param nameme1
plot_spp_ann_trait <- function(sppDf = production_summaries[["ann_spp_summary"]],
                               taxaDf = flux_summaries[['']]) {

  sppDf %>%
    group_by(taxon_id) %>% 
    dplyr::mutate(spp_pb_mean = )
    dplyr::select(pb_y_mean, taxon_id) %>% dplyr::arrange(pb_y_mean) %>% dplyr::mutate(taxon_id = factor(taxon_id, levels = taxon_id)) %>% ggplot()+geom_boxplot(aes(x = taxon_id, y = pb_y_mean))
    
    return(NULL)

}
