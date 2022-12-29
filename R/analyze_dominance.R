##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param nameme1
analyze_dominance <- function(ann_spp_flux_boots = flux_summaries[["annual_spp_flux_boots"]]) {
  #'
  #'
  ## ++++ Helper functions ++++ ##
   part_dominance = function(x){
   
     o = sort(x, decreasing = TRUE)
     # m = x[o]
     p = c()
     
     for(i in 1:length(o)){
       p[i] = 100*o[i]/sum(o[i:length(o)])
     }
     return(p)
   }
  
  ## ++++ End Helper functions ++++ ##
  
  ann_spp_flux_summary = ann_spp_flux_boots %>%
    group_by(site, taxon) %>%
    dplyr::summarise(across(matches('flux'), list(mean = ~mean(.x, na.rm = TRUE),
                                                  quant2.5 = ~quantile(.x, 0.025, na.rm = TRUE),
                                                  quant25 = ~quantile(.x, 0.25, na.rm = TRUE),
                                                  quant50 = ~quantile(.x, 0.50, na.rm = TRUE),
                                                  quant75 = ~quantile(.x, 0.75, na.rm = TRUE),
                                                  quant97.5 = ~quantile(.x, 0.975, na.rm = TRUE))))
  
  # debugonce(part_dominance)
  partial_dominance = ann_spp_flux_summary %>%
    group_by(site) %>% select(contains('mean')) %>%
    dplyr::arrange(desc(flux_mg_m_y_mean)) %>%
    dplyr::mutate(rel_flux = flux_mg_m_y_mean/sum(flux_mg_m_y_mean),
                  spp_rank = dense_rank(rel_flux),
                  partial_dominance = part_dominance(rel_flux))
    

}
