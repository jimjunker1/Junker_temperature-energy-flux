#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param nameme1
plot_spp_ann_trait <- function(sppDf = production_summaries[["ann_spp_summary"]]) {

  sppDf %>%
    dplyr::mutate(site_id = factor(site_id, levels= stream_order)) %>%
    ggplot()+
    geom_point(aes(x = log(M_mg_ind_mean), y = log(pb_y_mean), color = site_id))
    

}
